if (exists("CalculateErrors", mode = "function")) rm(CalculateErrors)
if (exists("run_all_combinations", mode = "function")) rm(run_all_combinations)
if (exists("build_fixed_orders", mode = "function")) rm(build_fixed_orders)


library(readxl)
library(ggplot2)
library(ggh4x)   # per-facet y-limits, themed strips, facet_grid2(..., space='free_y')
library(grid)
library(magick)

build_fixed_orders <- function(groups2, global_y_order = NULL) {
  stopifnot(all(c("Group","Abbreviation_old") %in% colnames(groups2)))
  
  fixed_group_levels <- unique(as.character(groups2$Group))  # facet row order
  
  if (is.null(global_y_order)) {
    global_y_order <- unique(as.character(groups2$Abbreviation_old))
  } else {
    global_y_order <- as.character(global_y_order)
  }
  
  group_to_abbr <- lapply(fixed_group_levels, function(gl) {
    grp_set <- as.character(groups2$Abbreviation_old[groups2$Group == gl])
    global_y_order[global_y_order %in% grp_set]
  })
  names(group_to_abbr) <- fixed_group_levels
  
  list(
    group_levels   = fixed_group_levels,
    group_to_abbr  = group_to_abbr,
    global_y_order = global_y_order
  )
}

CalculateErrors <- function(
    Group,
    name,
    groups2,
    lm_path,
    lm_sheet = "Steroids' adjusted lms",
    lm_condition = NULL,
    
    use_qval = FALSE,
    alpha = 0.10,
    se_multiplier = 0.5,
    strict_gender = TRUE,
    
    fixed_group_levels = NULL,
    fixed_y_by_group   = NULL,
    global_y_order     = NULL,
    
    # Colored strips & label placement
    group_strip_fills  = NULL,
    shorten_long_group_names = TRUE,
    
    # strip label placement for left strips (angle=90 on the left):
    # NOTE: with angle = 90, vjust moves horizontally toward/away from panel
    strip_hjust = 0.50,         # keep centered on baseline
    strip_vjust = 0.30,         # smaller -> closer to the panel (right)
    strip_margin_left  = 6,     # pts (outer side)
    strip_margin_right = 4,     # pts (gap to panel)
    
    # Fonts (consistent everywhere)
    axis_title_size = 14,
    axis_text_size  = 12,
    strip_text_size = 12,
    title_face = "plain",       # or "bold"
    text_face  = "plain",
    strip_face = "plain",
    
    # Device sizing (auto-height; no panel-unit forcing)
    device_width_px  = 8500,
    device_height_px = NULL,    # if NULL -> auto-compute from analyte counts
    device_res       = 800,
    
    # X-axis tuning
    symmetric_x      = TRUE,
    x_breaks_n       = 7,
    x_nice_step      = 0.05,
    x_minor_width    = 0.025,
    x_expand         = c(0.02, 0.02),
    x_label_accuracy = 0.01,
    x_title_top_margin = 12,
    
    # Output
    xlim = NULL,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plot = FALSE,
    
    ...   # ignored (safe pass-through)
) {
  # --- helpers ---
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])","", x, perl = TRUE)
    gsub("[^A-Z0-9]", "", x)
  }
  header_norm <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[\r\n]+", " ", x); x <- gsub('"', "", x)
    x <- gsub("\\s+",  " ", x);  x <- trimws(x)
    gsub("[^a-z0-9]","", x)
  }
  parse_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- gsub("\\s+","", as.character(x))
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  }
  infer_condition_from_name <- function(s) {
    if (is.null(s)) return(NA_character_)
    st <- tolower(s)
    if (grepl("steatosis", st)) return("Steatosis")
    if (grepl("fibrosis", st)) return("Fibrosis")
    if (grepl("necro[- ]?inflammation", st)) return("Necroinflammation")
    if (grepl("homa", st)) return("HOMAIR")
    NA_character_
  }
  normalize_cond <- function(s) gsub("[^a-z0-9]","", tolower(as.character(s)))
  
  # --- safety ---
  stopifnot(is.data.frame(groups2))
  req_cols <- c("Group","Abbreviation","Abbreviation_old","Name")
  stopifnot(all(req_cols %in% colnames(groups2)))
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  gender_token <- switch(tolower(Group), "all"="both", "male"="male", "female"="female", "both")
  
  # --- read LM ---
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  if (nrow(lm_raw) == 0) stop("LM sheet '", lm_sheet, "' is empty.")
  cn <- colnames(lm_raw); cn_norm <- header_norm(cn)
  idx_steroid <- which(cn_norm == "steroid")[1]
  idx_gender  <- which(cn_norm == "genderincondition")[1]
  idx_cond    <- which(cn_norm == "condition")[1]
  idx_coef    <- which(cn_norm == "coef")[1]
  idx_se      <- which(cn_norm == "stderr")[1]
  idx_pval    <- which(cn_norm == "pval")[1]
  idx_qval    <- which(cn_norm == "qval")[1]
  if (any(is.na(c(idx_steroid, idx_gender, idx_cond, idx_coef, idx_se))))
    stop("LM must have: Steroid, Gender in condition, Condition, Coef, Stderr.\nSeen: ",
         paste(unique(cn_norm), collapse=" "))
  
  LM <- data.frame(
    Steroid      = trimws(gsub("[\r\n]+"," ", as.character(lm_raw[[idx_steroid]]))),
    GenderInCond = trimws(gsub("[\r\n]+"," ", as.character(lm_raw[[idx_gender]]))),
    Condition    = trimws(gsub("[\r\n]+"," ", as.character(lm_raw[[idx_cond]]))),
    Coef         = parse_num(lm_raw[[idx_coef]]),
    Stderr       = parse_num(lm_raw[[idx_se]]),
    Pval         = if (!is.na(idx_pval)) parse_num(lm_raw[[idx_pval]]) else NA_real_,
    Qval         = if (!is.na(idx_qval)) parse_num(lm_raw[[idx_qval]]) else NA_real_,
    stringsAsFactors = FALSE
  )
  LM <- LM[is.finite(LM$Coef) & is.finite(LM$Stderr), , drop = FALSE]
  if (nrow(LM) == 0) stop("No finite rows for Coef & Stderr in LM sheet.")
  
  # --- condition & gender ---
  if (is.null(lm_condition)) {
    lm_condition <- infer_condition_from_name(name)
    if (is.na(lm_condition)) {
      conds <- sort(unique(LM$Condition))
      if (length(conds) == 1) lm_condition <- conds[1] else
        stop("Set lm_condition explicitly. Available: ", paste(conds, collapse = ", "))
    }
  }
  rows_cond <- normalize_cond(LM$Condition) == normalize_cond(lm_condition)
  
  gic       <- tolower(LM$GenderInCond)
  gic_token <- sub(".*_", "", gic)
  rows_gender <- gic_token == gender_token
  
  LM_sel <- LM[rows_cond & rows_gender, , drop = FALSE]
  if (nrow(LM_sel) == 0) {
    if (isTRUE(strict_gender)) {
      stop("No LM rows for Condition='", lm_condition, "' with gender token '", gender_token, "'.")
    } else {
      warning("No exact '", gender_token, "' rows; using condition-only (strict_gender=FALSE).")
      LM_sel <- LM[rows_cond, , drop = FALSE]
    }
  }
  
  # --- plotting DF ---
  p_used <- if (isTRUE(use_qval) && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  ResComb <- data.frame(
    name     = LM_sel$Steroid,
    name_raw = LM_sel$Steroid,
    result   = LM_sel$Coef,
    se_lm    = LM_sel$Stderr,
    pval1    = p_used,
    stringsAsFactors = FALSE
  )
  
  # --- map to groups / Abbreviation_old ---
  map_tbl <- do.call(rbind, list(
    data.frame(alias = as.character(groups2$Abbreviation_old), Group = groups2$Group,
               AbbrevOld = as.character(groups2$Abbreviation_old), stringsAsFactors = FALSE),
    data.frame(alias = as.character(groups2$Abbreviation), Group = groups2$Group,
               AbbrevOld = as.character(groups2$Abbreviation_old), stringsAsFactors = FALSE),
    data.frame(alias = as.character(groups2$Name), Group = groups2$Group,
               AbbrevOld = as.character(groups2$Abbreviation_old), stringsAsFactors = FALSE)
  ))
  map_tbl <- map_tbl[!is.na(map_tbl$alias) & nzchar(map_tbl$alias), ]
  map_tbl$key_norm <- norm_key(map_tbl$alias)
  ResComb$key_norm <- norm_key(ResComb$name)
  
  MapDF <- merge(ResComb[, c("name","key_norm")],
                 unique(map_tbl[, c("key_norm","Group","AbbrevOld")]),
                 by = "key_norm", all.x = TRUE)
  
  ResComb$Group     <- MapDF$Group[match(ResComb$key_norm, MapDF$key_norm)]
  ResComb$AbbrevOld <- MapDF$AbbrevOld[match(ResComb$key_norm, MapDF$key_norm)]
  
  # --- drop unmapped (fixed: |) ---
  drop_mask <- is.na(ResComb$Group) | is.na(ResComb$AbbrevOld)
  if (any(drop_mask)) {
    warning("Dropped unmapped analytes: ", paste(sort(unique(ResComb$name[drop_mask])), collapse = ", "))
    ResComb <- ResComb[!drop_mask, , drop = FALSE]
  }
  if (nrow(ResComb) == 0) stop("No analytes left after mapping to groups2.")
  
  # --- error bars & significance ---
  eb <- se_multiplier * ResComb$se_lm
  ResComb$errord1 <- ResComb$result - eb
  ResComb$errord2 <- ResComb$result + eb
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, "Yes", "No")
  
  # --- fixed orders (derive if missing; honor global_y_order) ---
  if (is.null(fixed_group_levels) || is.null(fixed_y_by_group)) {
    fixed_group_levels <- unique(as.character(groups2$Group))
    if (is.null(global_y_order)) {
      global_y_order <- unique(as.character(groups2$Abbreviation_old))
    } else {
      global_y_order <- as.character(global_y_order)
    }
    fixed_y_by_group <- lapply(fixed_group_levels, function(gl) {
      grp_set <- as.character(groups2$Abbreviation_old[groups2$Group == gl])
      global_y_order[global_y_order %in% grp_set]
    })
    names(fixed_y_by_group) <- fixed_group_levels
  } else if (!is.null(global_y_order)) {
    fixed_y_by_group <- lapply(fixed_group_levels, function(gl) {
      grp_set <- as.character(groups2$Abbreviation_old[groups2$Group == gl])
      global_y_order[global_y_order %in% grp_set]
    })
    names(fixed_y_by_group) <- fixed_group_levels
  }
  
  # --- expand per facet; display labels ---
  abbr_group <- function(x) {
    x <- as.character(x)
    if (shorten_long_group_names) {
      # x <- sub("^Mineralocorticoids$", "Mineraloc.", x)
      x <- sub("^Glucocorticoids$",   "Glucoc.",   x)
      # x <- sub("^Estrogens$",   "Estrog.",   x)
    }
    x
  }
  group_label_map <- setNames(abbr_group(fixed_group_levels), fixed_group_levels)
  
  ResComb$Group <- factor(as.character(ResComb$Group), levels = fixed_group_levels)
  ResComb$AbbrevOld <- as.character(ResComb$AbbrevOld)
  
  groups_frame <- do.call(rbind, lapply(fixed_group_levels, function(gl) {
    data.frame(
      Group     = factor(gl, levels = fixed_group_levels),
      GroupDisp = factor(group_label_map[[gl]], levels = group_label_map[fixed_group_levels]),
      AbbrevOld = fixed_y_by_group[[gl]],
      stringsAsFactors = FALSE
    )
  }))
  ResComb_full <- merge(groups_frame, ResComb, by = c("Group","AbbrevOld"), all.x = TRUE, sort = FALSE)
  
  # --- x-range (symmetric if requested) ---
  nice_up <- function(z, step = 0.05) step * ceiling(z / step)
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    if (isTRUE(symmetric_x)) {
      max_abs <- max(abs(ResComb_full$errord1), abs(ResComb_full$errord2), na.rm = TRUE)
      xmax    <- nice_up(max_abs, step = x_nice_step)
      xlim    <- c(-xmax, xmax)
    } else {
      xlim <- range(c(ResComb_full$errord1, ResComb_full$errord2), na.rm = TRUE)
    }
  }
  
  # --- colored strips (background only) ---
  if (is.null(group_strip_fills)) {
    base_cols <- c("#1f77b4","#d62728","#2ca02c","#9467bd","#ff7f0e","#17becf","#7f7f7f")
    group_strip_fills <- setNames(base_cols[seq_along(fixed_group_levels)], fixed_group_levels)
  }
  fills_in_order <- unname(group_strip_fills[fixed_group_levels])
  
  # IMPORTANT: text is handled in theme below; we only set background here
  strip_arg <- ggh4x::strip_themed(
    background_y = ggh4x::elem_list_rect(fill = fills_in_order, colour = NA)
  )
  
  # --- per-facet y-scale limits (exact order) ---
  y_scales <- lapply(fixed_group_levels, function(gl) {
    ggplot2::scale_y_discrete(limits = fixed_y_by_group[[gl]], drop = FALSE)
  })
  
  # --- auto device height from counts (no panel-unit forcing) ---
  n_by_group     <- vapply(fixed_group_levels, function(gl) length(fixed_y_by_group[[gl]]), integer(1))
  approx_mm_rows <- sum(5.5 * n_by_group + 8) + 20  # ~5.5 mm per analyte + 8 mm per group + margins
  if (is.null(device_height_px)) {
    px_per_mm <- device_res / 25.4
    device_height_px <- max(3000L, as.integer(ceiling(approx_mm_rows * px_per_mm)))
  }
  
  # --- plot ---
  sig_cols <- c(No = "#999999", Yes = "blue")
  
  p <- ggplot2::ggplot(
    ResComb_full,
    ggplot2::aes(y = AbbrevOld, x = result, colour = Significance1)
  ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "#cccccc") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = errord1, xmax = errord2), height = 0, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = sig_cols, drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = xlim,
      breaks = scales::pretty_breaks(n = x_breaks_n),
      minor_breaks = if (is.null(x_minor_width)) NULL else scales::breaks_width(x_minor_width),
      labels = scales::label_number(accuracy = x_label_accuracy),
      expand = expansion(mult = x_expand)
    ) +
    ggh4x::facet_grid2(
      rows   = ggplot2::vars(GroupDisp),
      scales = "free_y", space = "free_y",
      switch = "y",
      strip  = strip_arg
    ) +
    ggh4x::facetted_pos_scales(y = y_scales) +
    ggplot2::xlab("Liner Model Estimates (SE)") +
    ggplot2::ylab("Steroids") +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      # Axis fonts
      axis.title.x = ggplot2::element_text(size = axis_title_size, face = title_face,
                                           margin = ggplot2::margin(t = x_title_top_margin)),
      axis.title.y = ggplot2::element_text(size = axis_title_size, face = title_face),
      axis.text.x  = ggplot2::element_text(size = axis_text_size,  face = text_face),
      axis.text.y  = ggplot2::element_text(size = axis_text_size,  face = text_face),
      legend.position   = "none",
      # Strip label text (uniform & positioned); handled by THEME (not strip_themed)
      strip.text.y.left = ggplot2::element_text(
        angle  = 90,
        hjust  = strip_hjust,              # baseline center
        vjust  = strip_vjust,              # ← horizontal nudge toward panel
        size   = strip_text_size,
        face   = strip_face,
        colour = "white",
        margin = ggplot2::margin(l = strip_margin_left, r = strip_margin_right)
      )
    )
  
  
  p <- p+
    theme(
      axis.title.x = element_text(face = "bold"),  # Bold x-axis label
      axis.title.y = element_text(face = "bold")   # Bold y-axis label
    )
  
  
  g <- ggplot2::ggplotGrob(p)
  
  # --- save / return ---
  if (isTRUE(save_outputs)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    safe_stem <- gsub("[^A-Za-z0-9._-]+","_", name)
    jpg_path <- file.path(output_dir, paste0(safe_stem, ".jpg"))
    svg_path <- file.path(output_dir, paste0(safe_stem, ".svg"))
    pdf_path <- file.path(output_dir, paste0(safe_stem, ".pdf"))
    
    jpeg(jpg_path, width = device_width_px, height = device_height_px,
         quality = 100, pointsize = 16, res = device_res)
    print(grid::grid.draw(g))
    dev.off()
    
    img <- magick::image_read(jpg_path)
    magick::image_write(magick::image_convert(img, format = "pdf"), pdf_path)
    magick::image_write(magick::image_convert(img, format = "svg"), svg_path)
  }
  
  if (isTRUE(return_plot)) return(list(gtable = g, data = ResComb_full, plot = p))
  invisible(NULL)
}


run_all_combinations <- function(
    groups2,
    lm_path,
    lm_sheet = "Steroids' adjusted lms",
    groups = c("All", "Female", "Male"),
    conditions = c("Steatosis", "Fibrosis", "Necroinflammation", "HOMA-IR"),
    name_prefix = "Forest plot of",
    global_y_order = NULL,
    use_qval = FALSE,
    alpha = 0.10,
    se_multiplier = 0.5,
    strict_gender = TRUE,
    group_strip_fills = NULL,
    shorten_long_group_names = TRUE,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plots = FALSE,
    verbose = TRUE,
    ...
) {
  stopifnot(is.data.frame(groups2))
  req_cols <- c("Group","Abbreviation","Abbreviation_old","Name")
  stopifnot(all(req_cols %in% colnames(groups2)))
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  ord <- build_fixed_orders(groups2, global_y_order = global_y_order)
  fixed_group_levels <- ord$group_levels
  fixed_y_by_group   <- ord$group_to_abbr
  global_y_order     <- ord$global_y_order
  
  logs  <- list(); plots <- list(); k <- 0
  for (g in groups) {
    for (cond in conditions) {
      k <- k + 1
      plot_name <- paste(name_prefix, g, "Steroid Coefs in", cond)
      if (isTRUE(verbose)) message(sprintf("[INFO] %s | %s", g, cond))
      out <- tryCatch({
        CalculateErrors(
          Group = g,
          name = plot_name,
          groups2 = groups2,
          lm_path = lm_path,
          lm_sheet = lm_sheet,
          lm_condition = cond,
          use_qval = use_qval,
          alpha = alpha,
          se_multiplier = se_multiplier,
          strict_gender = strict_gender,
          fixed_group_levels = fixed_group_levels,
          fixed_y_by_group   = fixed_y_by_group,
          global_y_order     = global_y_order,
          group_strip_fills  = group_strip_fills,
          shorten_long_group_names = shorten_long_group_names,
          output_dir = output_dir,
          save_outputs = save_outputs,
          return_plot = return_plots,
          ...
        )
      }, error = function(e) e)
      
      if (inherits(out, "error")) {
        msg <- conditionMessage(out)
        if (isTRUE(verbose)) message("[WARN] Skipped ", g, " | ", cond, " — ", msg)
        logs[[k]] <- data.frame(Group=g, Condition=cond, Status="error", Message=msg, stringsAsFactors=FALSE)
      } else {
        if (isTRUE(verbose) && isTRUE(save_outputs)) {
          message(sprintf("[OK] Saved to: %s", normalizePath(output_dir)))
        }
        logs[[k]] <- data.frame(Group=g, Condition=cond, Status="ok", Message="", stringsAsFactors=FALSE)
        if (isTRUE(return_plots)) plots[[paste(g, cond, sep = " | ")]] <- out$gtable
      }
    }
  }
  log_df <- do.call(rbind, logs)
  if (isTRUE(return_plots)) return(list(log = log_df, plots = plots))
  log_df
}

outdir <- file.path(getwd(), "forest_plots_fix")
dir.create(outdir, showWarnings = FALSE)

global_order <- c("11b-OHA4","11-KDHT","11-KT","11-KA4","A4","AN","DHEA","DHT","T/Epi-T",
                  "E2","E1","S","E","A","B","DOC","17a-OHP5","17a-OHP4","P5","P4")

my_strip_fills <- c(
  Androgens          = "#1f77b4",
  Estrogens          = "#d62728",
  Glucocorticoids    = "#2ca02c",
  Mineralocorticoids = "#9467bd",
  Progestogens       = "#ff7f0e"
)

# groups2red=groups2

# groups2red[groups2red[,1]=='Estrogens',1]='Estrog.'

CalculateErrors(
  Group = "All",
  name  = "TEST_All_Steatosis4",
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = "Steroids' adjusted lms",
  lm_condition = "Steatosis",
  global_y_order = global_order,
  group_strip_fills = my_strip_fills,
  # label placement toward panel & margins
  strip_vjust = 0.28, strip_margin_left = 8, strip_margin_right = 8,
  # fonts
  axis_title_size = 12, axis_text_size = 12, strip_text_size = 12,
  # x-axis
  symmetric_x = TRUE, x_breaks_n = 7, x_nice_step = 0.05,
  x_minor_width = 0.025, x_expand = c(0.02,0.02), x_label_accuracy = 0.01,
  # device
  device_width_px = 5000, device_height_px = 8800, device_res = 800,
  output_dir = outdir, save_outputs = TRUE, return_plot = FALSE
)


#2) Batch
log_df <- run_all_combinations(
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = "Steroids' adjusted lms",
  groups = c("All","Female","Male"),
  conditions = c("Steatosis","Fibrosis","Necroinflammation","HOMA-IR"),
  name_prefix = "Forest plot of",
  global_y_order = global_order,
  group_strip_fills = my_strip_fills,
  # same visual knobs as above (they’ll pass through):
  strip_vjust = 0.28, strip_margin_left = 8, strip_margin_right = 8,
  # fonts
  axis_title_size = 14, axis_text_size = 12, strip_text_size = 12,
  # x-axis
  symmetric_x = TRUE, x_breaks_n = 7, x_nice_step = 0.05,
  x_minor_width = 0.025, x_expand = c(0.02,0.02), x_label_accuracy = 0.01,
  # device
  device_width_px = 5000, device_height_px = 8800, device_res = 800,
  output_dir = outdir, save_outputs = TRUE, return_plot = FALSE
)

log_df

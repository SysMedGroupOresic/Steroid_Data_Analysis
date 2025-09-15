library(readxl)
library(ggplot2)
library(ggh4x)    # per-facet scales + themed strips
library(grid)
library(magick)

build_fixed_orders <- function(groups2) {
  stopifnot(all(c("Group","Abbreviation_old") %in% colnames(groups2)))
  # Facet row order = appearance order in groups2
  fixed_group_levels <- unique(as.character(groups2$Group))
  # Per-facet y order = Abbreviation_old order within each Group (as in groups2)
  group_to_abbr <- lapply(fixed_group_levels, function(gl) {
    as.character(groups2$Abbreviation_old[groups2$Group == gl])
  })
  names(group_to_abbr) <- fixed_group_levels
  list(group_levels = fixed_group_levels, group_to_abbr = group_to_abbr)
}
CalculateErrors <- function(
    Group,                         # "All" | "Female" | "Male"
    name,                          # output stem (used in filenames)
    groups2,                       # mapping: Group, Abbreviation, Abbreviation_old, Name
    lm_path,                       # path to Excel file with LM results
    lm_sheet = "Steroids' adjusted lms",
    lm_condition = NULL,           # e.g., "Steatosis", "Fibrosis", "Necroinflammation", "HOMA-IR"
    use_qval = FALSE,              # color by Q-values instead of P-values
    alpha = 0.10,                  # significance cutoff
    se_multiplier = 0.5,           # ±(SE/2) error bars by default
    strict_gender = TRUE,          # require exact *_both/*_male/*_female
    fixed_group_levels = NULL,     # optional; computed from groups2 if NULL
    fixed_y_by_group   = NULL,     # optional; computed from groups2 if NULL
    group_strip_fills  = NULL,     # optional named vector: original Group -> strip fill color
    shorten_long_group_names = TRUE, # shorten Mineralocorticoids/Glucocorticoids on strips
    xlim = NULL,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plot = FALSE
) {
  # ---- safety ----
  stopifnot(is.data.frame(groups2))
  req_cols <- c("Group","Abbreviation","Abbreviation_old","Name")
  if (!all(req_cols %in% colnames(groups2))) {
    stop("groups2 must have columns: ", paste(req_cols, collapse = ", "))
  }
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  # ---- helpers ----
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])","", x, perl = TRUE)       # drop leading X before digits
    gsub("[^A-Z0-9]", "", x)
  }
  header_norm <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[\r\n]+", " ", x)
    x <- gsub('"', "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    gsub("[^a-z0-9]","", x)
  }
  parse_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x <- gsub("\\s+", "", x)
    x <- gsub(",", ".", x) # decimal commas → dot
    suppressWarnings(as.numeric(x))
  }
  infer_condition_from_name <- function(s) {
    if (is.null(s)) return(NA_character_)
    st <- tolower(s)
    if (grepl("steatosis", st)) return("Steatosis")
    if (grepl("fibrosis", st)) return("Fibrosis")
    if (grepl("necro[- ]?inflammation", st)) return("Necroinflammation")
    if (grepl("homa", st)) return("HOMAIR")  # accept HOMA-IR
    NA_character_
  }
  normalize_cond <- function(s) gsub("[^a-z0-9]","", tolower(as.character(s)))
  
  gender_token <- switch(tolower(Group), "all"="both", "male"="male", "female"="female", "both")
  
  # ---- read LM sheet ----
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
  
  if (any(is.na(c(idx_steroid, idx_gender, idx_cond, idx_coef, idx_se)))) {
    stop("LM sheet must have at least: Steroid, Gender in condition, Condition, Coef, Stderr.",
         "\nHeaders seen (normalized): ", paste(unique(cn_norm), collapse = " "))
  }
  
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
  
  # ---- condition selection ----
  if (is.null(lm_condition)) {
    lm_condition <- infer_condition_from_name(name)
    if (is.na(lm_condition)) {
      conds <- sort(unique(LM$Condition))
      if (length(conds) == 1) lm_condition <- conds[1]
      else stop("Set lm_condition explicitly. Available: ", paste(conds, collapse = ", "))
    }
  }
  rows_cond <- normalize_cond(LM$Condition) == normalize_cond(lm_condition)
  
  # ---- strict gender token ----
  gic       <- tolower(LM$GenderInCond)
  gic_token <- sub(".*_", "", gic)   # 'both' | 'male' | 'female'
  rows_gender <- gic_token == gender_token
  
  LM_sel <- LM[rows_cond & rows_gender, , drop = FALSE]
  if (nrow(LM_sel) == 0) {
    if (isTRUE(strict_gender)) {
      stop("No LM rows for Condition='", lm_condition, "' with gender token '", gender_token, "'.")
    } else {
      warning("No exact '", gender_token, "' rows for ", lm_condition, "; using condition-only (strict_gender=FALSE).")
      LM_sel <- LM[rows_cond, , drop = FALSE]
    }
  }
  
  # ---- build plotting DF ----
  p_used <- if (isTRUE(use_qval) && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  
  ResComb <- data.frame(
    name     = LM_sel$Steroid,
    name_raw = LM_sel$Steroid,
    result   = LM_sel$Coef,
    se_lm    = LM_sel$Stderr,
    pval1    = p_used,
    stringsAsFactors = FALSE
  )
  
  # ---- map to groups / Abbreviation_old via aliases from groups2 ----
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
  
  # ---- strict drop unmapped (FIXED: use |, not backslash) ----
  drop_mask <- is.na(ResComb$Group) | is.na(ResComb$AbbrevOld)
  if (any(drop_mask)) {
    warning("Dropped unmapped analytes: ", paste(sort(unique(ResComb$name[drop_mask])), collapse = ", "))
    ResComb <- ResComb[!drop_mask, , drop = FALSE]
  }
  if (nrow(ResComb) == 0) stop("No analytes left after strict mapping to groups2.")
  
  # ---- error bars: ± (SE * se_multiplier) ----
  eb <- se_multiplier * ResComb$se_lm
  ResComb$errord1 <- ResComb$result - eb
  ResComb$errord2 <- ResComb$result + eb
  
  # ---- significance colouring ----
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, "Yes", "No")
  
  # ---- derive fixed orders from groups2 if missing ----
  if (is.null(fixed_group_levels) || is.null(fixed_y_by_group)) {
    ord <- build_fixed_orders(groups2)
    fixed_group_levels <- ord$group_levels
    fixed_y_by_group   <- ord$group_to_abbr
  }
  
  # ---- enforce factor for Group in the original order; keep AbbrevOld character ----
  ResComb$Group <- factor(as.character(ResComb$Group), levels = fixed_group_levels)
  ResComb$AbbrevOld <- as.character(ResComb$AbbrevOld)
  
  # ---- expand per facet so y-axis always shows full set (and build display names) ----
  abbr_group <- function(x) {
    x <- as.character(x)
    if (shorten_long_group_names) {
      x <- sub("^Mineralocorticoids$", "Mineraloc.", x)
      x <- sub("^Glucocorticoids$",   "Glucoc.",   x)
    }
    x
  }
  group_label_map <- setNames(abbr_group(fixed_group_levels), fixed_group_levels)
  
  groups_frame <- do.call(rbind, lapply(fixed_group_levels, function(gl) {
    data.frame(
      Group     = factor(gl, levels = fixed_group_levels),
      GroupDisp = factor(group_label_map[[gl]], levels = group_label_map[fixed_group_levels]),
      AbbrevOld = fixed_y_by_group[[gl]],
      stringsAsFactors = FALSE
    )
  }))
  
  ResComb_full <- merge(groups_frame, ResComb, by = c("Group","AbbrevOld"), all.x = TRUE, sort = FALSE)
  
  # ---- x-range ----
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb_full$errord1, ResComb_full$errord2), na.rm = TRUE)
  }
  
  # ---- facet strip colors ----
  if (is.null(group_strip_fills)) {
    base_cols <- c("#1f77b4","#d62728","#2ca02c","#9467bd","#ff7f0e","#17becf","#7f7f7f")
    group_strip_fills <- setNames(base_cols[seq_along(fixed_group_levels)], fixed_group_levels)
  }
  fills_in_order <- unname(group_strip_fills[fixed_group_levels])
  strip_arg <- ggh4x::strip_themed(
    background_y = ggh4x::elem_list_rect(fill = fills_in_order, colour = NA),
    text_y       = ggh4x::elem_list_text(colour = rep("white", length(fills_in_order)), face = "bold")
  )
  
  # ---- per-facet y-scale limits (exact Abbreviation_old sequence) ----
  y_scales <- lapply(fixed_group_levels, function(gl) {
    ggplot2::scale_y_discrete(limits = fixed_y_by_group[[gl]], drop = FALSE)
  })
  
  # ---- plot (your requested grammar) ----
  sig_cols <- c(No = "#999999", Yes = "blue")
  
  p <- ggplot2::ggplot(
    ResComb_full,
    ggplot2::aes(y = AbbrevOld, x = result, colour = Significance1)
  ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "#cccccc") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = errord1, xmax = errord2), height = 0, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = sig_cols, drop = FALSE) +
    ggplot2::coord_cartesian(xlim = xlim, expand = TRUE) +
    ggh4x::facet_grid2(
      rows   = ggplot2::vars(GroupDisp),
      scales = "free_y", space = "free_y",
      switch = "y",
      strip  = strip_arg
    ) +
    ggh4x::facetted_pos_scales(
      y = y_scales
    ) +
    ggplot2::xlab("Liner Model Estimates (SE)") +
    ggplot2::ylab("Steroids") +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      axis.text.y         = ggplot2::element_text(size = 8),
      legend.position     = "none",
      strip.text.y.left   = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5,
                                                  margin = ggplot2::margin(r = 6))
    )
  
  g <- ggplot2::ggplotGrob(p)
  
  # ---- save / return ----
  if (isTRUE(save_outputs)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    safe_stem <- gsub("[^A-Za-z0-9._-]+","_", name)
    jpg_path <- file.path(output_dir, paste0(safe_stem, ".jpg"))
    svg_path <- file.path(output_dir, paste0(safe_stem, ".svg"))
    pdf_path <- file.path(output_dir, paste0(safe_stem, ".pdf"))
    
    jpeg(jpg_path, width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
    print(grid::grid.draw(g))
    dev.off()
    
    # (optional) PDF via magick / daiR
    my_image <- magick::image_read(jpg_path)
    magick::image_write(magick::image_convert(my_image, format = "pdf"), pdf_path)
    magick::image_write(magick::image_convert(my_image, format = "svg"), svg_path)
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
    use_qval = FALSE,
    alpha = 0.10,
    se_multiplier = 0.5,          # ±(SE/2)
    strict_gender = TRUE,
    group_strip_fills = NULL,     # named vector: original Group -> color (optional)
    shorten_long_group_names = TRUE,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plots = FALSE,
    verbose = TRUE
) {
  stopifnot(is.data.frame(groups2))
  req_cols <- c("Group","Abbreviation","Abbreviation_old","Name")
  stopifnot(all(req_cols %in% colnames(groups2)))
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  # Fixed orders derived once from groups2
  ord <- build_fixed_orders(groups2)
  fixed_group_levels <- ord$group_levels
  fixed_y_by_group   <- ord$group_to_abbr
  
  logs  <- list()
  plots <- list()
  k <- 0
  
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
          group_strip_fills  = group_strip_fills,
          shorten_long_group_names = shorten_long_group_names,
          xlim = NULL,
          output_dir = output_dir,
          save_outputs = save_outputs,
          return_plot = return_plots
        )
      }, error = function(e) e)
      
      if (inherits(out, "error")) {
        msg <- conditionMessage(out)
        if (isTRUE(verbose)) message("[WARN] Skipped ", g, " | ", cond, " — ", msg)
        logs[[k]] <- data.frame(Group=g, Condition=cond, Status="error", Message=msg, stringsAsFactors=FALSE)
      } else {
        if (isTRUE(verbose) && isTRUE(save_outputs)) {
          message(sprintf("[OK] Saved: %s", normalizePath(output_dir)))
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
# Example strip colors per original Group (optional)
my_strip_fills <- c(
  Androgens          = "#1f77b4",
  Estrogens          = "#d62728",
  Glucocorticoids    = "#2ca02c",
  Mineralocorticoids = "#9467bd",
  Progestogens       = "#ff7f0e"
)

# Where to save images
outdir <- file.path(getwd(), "forest_plots")
dir.create(outdir, showWarnings = FALSE)

# Run all combinations and capture the log
log_df <- run_all_combinations(
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = "Steroids' adjusted lms",
  groups = c("All","Female","Male"),
  conditions = c("Steatosis","Fibrosis","Necroinflammation","HOMA-IR"),
  name_prefix = "Forest plot of",
  use_qval = FALSE,
  alpha = 0.10,
  se_multiplier = 0.5,
  strict_gender = TRUE,
  group_strip_fills = my_strip_fills,      # or NULL to use defaults
  shorten_long_group_names = TRUE,
  output_dir = outdir,
  save_outputs = TRUE,
  return_plots = FALSE,
  verbose = TRUE
)

log_df
# Inspect saved files
list.files(outdir, pattern = "\\.(jpg|svg|pdf)$", ignore.case = TRUE)

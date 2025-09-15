# Build global fixed orders from groups2 (facet order + per-facet Abbreviation_old order)
build_fixed_orders <- function(groups2) {
  stopifnot(all(c("Group","Abbreviation_old") %in% colnames(groups2)))
  # Facet (group) order = row order in groups2
  fixed_group_levels <- unique(as.character(groups2$Group))
  # Per-group Abbreviation_old order, as they appear in groups2
  group_to_abbr <- lapply(fixed_group_levels, function(gl) {
    as.character(groups2$Abbreviation_old[groups2$Group == gl])
  })
  names(group_to_abbr) <- fixed_group_levels
  list(group_levels = fixed_group_levels, group_to_abbr = group_to_abbr)
}

CalculateErrors <- function(
    Group, name, groups2, lm_path,
    lm_sheet = "Steroids' adjusted lms",
    lm_condition = NULL,
    use_qval = FALSE,
    alpha = 0.10,
    se_multiplier = 0.5,
    strict_gender = TRUE,
    fixed_group_levels,
    group_to_abbr,
    group_palette = NULL,
    xlim = NULL,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plot = FALSE
) {
  # Safe regex fix
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X([0-9])", "\\1", x)
    gsub("[^A-Z0-9]", "", x)
  }
  
  # Read LM data
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  if (nrow(lm_raw) == 0) stop("LM sheet is empty.")
  
  header_norm <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(gsub("[\r\n]+", " ", x))))
  cn_norm <- header_norm(colnames(lm_raw))
  idx <- function(name) which(cn_norm == name)[1]
  
  parse_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("\\s+", "", as.character(x)))))
  
  LM <- data.frame(
    Steroid      = trimws(as.character(lm_raw[[idx("steroid")]])),
    GenderInCond = trimws(as.character(lm_raw[[idx("genderincondition")]])),
    Condition    = trimws(as.character(lm_raw[[idx("condition")]])),
    Coef         = parse_num(lm_raw[[idx("coef")]]),
    Stderr       = parse_num(lm_raw[[idx("stderr")]]),
    Pval         = if (!is.na(idx("pval"))) parse_num(lm_raw[[idx("pval")]]) else NA_real_,
    Qval         = if (!is.na(idx("qval"))) parse_num(lm_raw[[idx("qval")]]) else NA_real_,
    stringsAsFactors = FALSE
  )
  LM <- LM[is.finite(LM$Coef) & is.finite(LM$Stderr), , drop = FALSE]
  if (nrow(LM) == 0) stop("No valid Coef/Stderr rows.")
  
  # Condition and gender filtering
  normalize_cond <- function(s) gsub("[^a-z0-9]", "", tolower(as.character(s)))
  infer_condition_from_name <- function(s) {
    st <- tolower(s)
    if (grepl("steatosis", st)) return("Steatosis")
    if (grepl("fibrosis", st)) return("Fibrosis")
    if (grepl("necro", st)) return("Necroinflammation")
    if (grepl("homa", st)) return("HOMAIR")
    NA_character_
  }
  
  gender_token <- switch(tolower(Group), "all" = "both", "male" = "male", "female" = "female", "both")
  lm_condition <- lm_condition %||% infer_condition_from_name(name)
  rows_cond <- normalize_cond(LM$Condition) == normalize_cond(lm_condition)
  gic_token <- sub(".*_", "", tolower(LM$GenderInCond))
  rows_gender <- gic_token == gender_token
  LM_sel <- LM[rows_cond & rows_gender, , drop = FALSE]
  if (nrow(LM_sel) == 0 && !strict_gender) {
    LM_sel <- LM[rows_cond, , drop = FALSE]
    warning("No exact gender match; using condition-only.")
  } else if (nrow(LM_sel) == 0) {
    stop("No LM rows for condition='", lm_condition, "' and gender='", gender_token, "'.")
  }
  
  # Prepare result data
  nice_name <- gsub("\\.", "-", LM_sel$Steroid)
  nice_name <- sub("^X11", "11", nice_name)
  nice_name <- sub("^X17", "17", nice_name)
  nice_name[nice_name == "T-Epi-T"] <- "T/Epi-T"
  p_used <- if (use_qval && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  
  ResComb <- data.frame(
    name      = nice_name,
    name_raw  = LM_sel$Steroid,
    result    = LM_sel$Coef,
    se_lm     = LM_sel$Stderr,
    pval1     = p_used,
    stringsAsFactors = FALSE
  )
  
  # Map to groups
  map_tbl <- do.call(rbind, lapply(c("Abbreviation_old", "Abbreviation", "Name"), function(col) {
    data.frame(alias = as.character(groups2[[col]]), Group = groups2$Group,
               AbbrevOld = groups2$Abbreviation_old, stringsAsFactors = FALSE)
  }))
  map_tbl <- map_tbl[nzchar(map_tbl$alias), ]
  map_tbl$key_norm <- norm_key(map_tbl$alias)
  ResComb$key_norm <- norm_key(ResComb$name)
  
  MapDF <- merge(ResComb[, c("name", "key_norm")],
                 unique(map_tbl[, c("key_norm", "Group", "AbbrevOld")]),
                 by = "key_norm", all.x = TRUE)
  
  ResComb$Group     <- MapDF$Group[match(ResComb$key_norm, MapDF$key_norm)]
  ResComb$AbbrevOld <- MapDF$AbbrevOld[match(ResComb$key_norm, MapDF$key_norm)]
  ResComb <- ResComb[!is.na(ResComb$Group) & !is.na(ResComb$AbbrevOld), , drop = FALSE]
  if (nrow(ResComb) == 0) stop("No analytes left after mapping.")
  
  # Error bars and significance
  eb <- se_multiplier * ResComb$se_lm
  ResComb$errord1 <- ResComb$result - eb
  ResComb$errord2 <- ResComb$result + eb
  ResComb$Significance1 <- factor(ifelse(ResComb$pval1 < alpha, "Yes", "No"), levels = c("No", "Yes"))
  ResComb$Group <- factor(ResComb$Group, levels = fixed_group_levels)
  
  # x-limits
  xlim <- xlim %||% {
    rng <- range(c(ResComb$errord1, ResComb$errord2, ResComb$result), na.rm = TRUE)
    span <- diff(rng); if (!is.finite(span)) span <- 1
    rng + c(-0.05, 0.05) * span
  }
  
  # Plot
  p <- ggplot2::ggplot(ResComb, ggplot2::aes(y = AbbrevOld, x = result, colour = Significance1)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "#cccccc") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = errord1, xmax = errord2), height = 0) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_colour_manual(values = c(No = "#999999", Yes = "blue"), drop = FALSE) +
    ggplot2::coord_cartesian(xlim = xlim, expand = TRUE) +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(Group),
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8),
      legend.position = "none"
    )
  
  g <- ggplot2::ggplotGrob(p)
  
  # Save
  if (isTRUE(save_outputs)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    jpg_path <- file.path(output_dir, paste0(name, "divi.jpg"))
    grDevices::jpeg(jpg_path, width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
    grid::grid.draw(g)
    grDevices::dev.off()
  }
  
  if (isTRUE(return_plot)) return(list(gtable = g, plot = p, data = ResComb))
  invisible(TRUE)
  
  # Save / return
  if (isTRUE(save_outputs)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Clean filename
    file_base <- gsub("[^a-zA-Z0-9_\\-]", "_", name)
    jpg_path <- file.path(output_dir, paste0(file_base, ".jpg"))
    svg_path <- file.path(output_dir, paste0(file_base, ".svg"))
    pdf_path <- file.path(output_dir, paste0(file_base, ".pdf"))
    
    # Save JPEG
    grDevices::jpeg(jpg_path, width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
    grid::grid.draw(g)
    grDevices::dev.off()
    
    # Save PDF
    grDevices::pdf(pdf_path, width = 7.5, height = 11)
    grid::grid.draw(g)
    grDevices::dev.off()
    
    # Save SVG via magick
    my_image <- magick::image_read(jpg_path)
    my_svg <- magick::image_convert(my_image, format = "svg")
    magick::image_write(my_svg, svg_path)
  }
  
  
  
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
    se_multiplier = 0.5,
    strict_gender = TRUE,
    group_palette = NULL,
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plots = FALSE,
    verbose = TRUE
) {
  stopifnot(is.data.frame(groups2))
  stopifnot(file.exists(lm_path))
  
  fo <- build_fixed_orders(groups2)
  fixed_group_levels <- fo$group_levels
  group_to_abbr <- fo$group_to_abbr
  
  logs <- list()
  plots <- list()
  combo_list <- expand.grid(Group = groups, Condition = conditions, stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(combo_list))) {
    g <- combo_list$Group[i]
    cond <- combo_list$Condition[i]
    plot_name <- paste(name_prefix, g, "Steroid Coefs in", cond)
    
    if (verbose) message(sprintf("[INFO] Processing: %s | %s", g, cond))
    
    result <- tryCatch({
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
        group_to_abbr = group_to_abbr,
        group_palette = group_palette,
        xlim = NULL,
        output_dir = output_dir,
        save_outputs = save_outputs,
        return_plot = return_plots
      )
    }, error = function(e) {
      if (verbose) message("[ERROR] ", g, " | ", cond, " — ", conditionMessage(e))
      e
    })
    
    logs[[i]] <- data.frame(
      Group = g,
      Condition = cond,
      Status = if (inherits(result, "error")) "error" else "ok",
      Message = if (inherits(result, "error")) conditionMessage(result) else "",
      stringsAsFactors = FALSE
    )
    
    if (!inherits(result, "error") && return_plots) {
      plots[[paste(g, cond, sep = " | ")]] <- result$gtable
    }
  }
  
  log_df <- do.call(rbind, logs)
  if (return_plots) return(list(log = log_df, plots = plots))
  log_df
}

group_palette <- c(
  "Androgens" = "#1f77b4",
  "Estrogens" = "#ff7f0e",
  "Glucocorticoids" = "#2ca02c",
  "Mineralocorticoids" = "#d62728",
  "Progestogens" = "#9467bd"
)

fo <- build_fixed_orders(groups2)

result <- CalculateErrors(
  Group = "All",
  name = "Forest plot of All Steroid Coefs in Steatosis",
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = "Steroids' adjusted lms",
  lm_condition = "Steatosis",
  use_qval = FALSE,
  alpha = 0.10,
  se_multiplier = 0.5,
  strict_gender = TRUE,
  fixed_group_levels = fo$group_levels,
  group_to_abbr = fo$group_to_abbr,
  group_palette = group_palette,
  xlim = NULL,
  output_dir = "forestplots",
  save_outputs = TRUE,
  return_plot = TRUE
)


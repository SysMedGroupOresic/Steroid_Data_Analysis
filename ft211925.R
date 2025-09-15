

library(readxl)
library(ggplot2)
library(ggforce)
library(ggforestplot)
library(grid)
library(magick)
# library(daiR)  # if you use daiR::image_to_pdf

CalculateErrors <- function(
    Group,                                 # "All" | "Female" | "Male"
    name,                                  # output stem for files
    groups2,                               # mapping table (Group, Abbreviation, Abbreviation_old, Name)
    lm_path,                               # path to LM Excel
    lm_sheet = "Steroids' adjusted lms",   # sheet with LM results
    lm_condition = NULL,                   # "Steatosis" | "Fibrosis" | "Necroinflammation" | "HOMA-IR"/"HOMAIR" | NULL (infer from name)
    use_qval = FALSE,                      # color by Q-values instead of P-values
    alpha = 0.10,                          # significance cutoff for coloring
    se_multiplier = 0.5,                   # ±(SE/2) error bars by default
    strict_gender = TRUE,                  # require exact *_both/*_male/*_female
    fixed_group_levels,                    # ⬅️ pass from wrapper: exact facet order (from groups2$Group)
    fixed_y_levels,                        # ⬅️ pass from wrapper: exact Abbreviation_old order (by group)
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
  if (missing(fixed_group_levels) || missing(fixed_y_levels)) {
    stop("Please pass fixed_group_levels and fixed_y_levels (precomputed from groups2 in the wrapper).")
  }
  
  # ---- helpers ----
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])","", x, perl=TRUE)   # drop leading X before digits
    gsub("[^A-Z0-9]", "", x)
  }
  header_norm <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[\r\n]+"," ", x)
    x <- gsub('"+', "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    gsub("[^a-z0-9]","", x)
  }
  parse_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x <- gsub("\\s+", "", x)
    x <- gsub(",", ".", x)               # decimal commas → dot
    suppressWarnings(as.numeric(x))
  }
  infer_condition_from_name <- function(s) {
    if (is.null(s)) return(NA_character_)
    st <- tolower(s)
    if (grepl("steatosis", st)) return("Steatosis")
    if (grepl("fibrosis", st)) return("Fibrosis")
    if (grepl("necroinflammation|necro-inflammation|necro inflammation", st)) return("Necroinflammation")
    if (grepl("homa", st)) return("HOMAIR")   # accept "HOMA-IR" in the title
    NA_character_
  }
  normalize_cond <- function(s) gsub("[^a-z0-9]", "", tolower(as.character(s)))
  gender_token <- switch(tolower(Group), "all"="both", "male"="male", "female"="female", "both")
  
  # ---- read LM ----
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
    stop("LM sheet must have at least: Steroid, Gender in condition, Condition, Coef, Stderr.\n",
         "Headers seen (normalized): ", paste(unique(cn_norm), collapse = " | "))
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
  
  # ---- condition ----
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
  gic <- tolower(LM$GenderInCond)
  gic_token <- sub(".*_", "", gic)  # 'both' | 'male' | 'female'
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
  name_display <- LM_sel$Steroid
  name_display <- gsub("\\.", "-", name_display)
  name_display <- sub("^X11", "11", name_display)
  name_display <- sub("^X17", "17", name_display)
  name_display[name_display == "T-Epi-T"] <- "T/Epi-T"
  
  p_used <- if (isTRUE(use_qval) && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  
  ResComb <- data.frame(
    name      = name_display,       # display label on y axis
    name_raw  = LM_sel$Steroid,     # original
    result    = LM_sel$Coef,
    se_lm     = LM_sel$Stderr,
    pval1     = p_used,
    stringsAsFactors = FALSE
  )
  
  # ---- mapping to groups and Abbreviation_old ----
  map_tbl <- do.call(rbind, list(
    data.frame(alias = as.character(groups2$Abbreviation_old), Group = groups2$Group,
               AbbrevOld = as.character(groups2$Abbreviation_old), stringsAsFactors = FALSE),
    data.frame(alias = as.character(groups2$Abbreviation),     Group = groups2$Group,
               AbbrevOld = as.character(groups2$Abbreviation_old), stringsAsFactors = FALSE),
    data.frame(alias = as.character(groups2$Name),             Group = groups2$Group,
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
  
  # ---- strict drop unmapped ----
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
  
  # ---- significance coloring ----
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, 'Yes', 'No')
  ResComb$Color1        <- ifelse(ResComb$pval1 < alpha, 'blue', 'grey')
  
  # ---- FORCE FIXED ORDERS (facet & y) ----
  # 1) Facet order: fixed_group_levels (exact same each call)
  ResComb$Group <- factor(ResComb$Group, levels = fixed_group_levels)
  
  # 2) Y order: use fixed Abbreviation_old sequence (fixed_y_levels), but show display names.
  #    Compute the display levels in *fixed_y_levels* order, then set factor levels.
  #    fixed_y_levels are Abbreviation_old values; map them to names present.
  display_levels <- ResComb$name[match(fixed_y_levels, ResComb$AbbrevOld)]
  display_levels <- display_levels[!is.na(display_levels)]
  if (length(display_levels) == 0) {
    stop("None of fixed_y_levels matched current data; check mapping Abbreviation_old vs LM analyte names.")
  }
  ResComb$name <- factor(ResComb$name, levels = unique(display_levels))
  
  # ---- xlim ----
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb$errord1, ResComb$errord2), na.rm = TRUE)
  }
  
  # ---- plot ----
  xlab_text <- if (abs(se_multiplier - 0.5) < 1e-9) 'LM Estimate ± 0.5·SE'
  else paste0('LM Estimate ± ', format(se_multiplier), '·SE')
  
  plote2 <- ggforestplot::forestplot(
    df       = ResComb,
    estimate = result,
    se       = 0,                 # draw our own error bars
    pvalue   = pval1,
    psignif  = alpha,
    xlim     = xlim,
    xlab     = xlab_text,
    ylab     = 'Steroid Groups',
    title    = '',
    colour   = Significance1
  ) +
    ggforce::facet_col(facets = ~Group, scales = "free_y", space = "free", strip.position = 'left') +
    geom_errorbarh(aes(xmin = errord1, xmax = errord2, height = .0, colour = Significance1))
  
  # subtle cosmetics
  try({ plote2$layers[[1]]$aes_params$odd <- "#00000000" }, silent = TRUE)
  hp <- if (sum(ResComb$Significance1 == 'Yes', na.rm = TRUE) == nrow(ResComb)) c('blue','blue') else c('#999999','blue')
  
  PlotVar  <- plote2 + theme(axis.text.y = element_blank()) + ggplot2::theme_classic()
  PlotVar2 <- PlotVar +
    geom_point(aes(colour = factor(Significance1)), colour = ResComb$Color1) +
    scale_color_manual(values = hp) +
    theme(legend.position = "none") +
    theme(strip.text.y = element_text(size = -Inf))
  
  g <- ggplot_gtable(ggplot_build(PlotVar2))  # gtable to draw/save
  
  # ---- save / return ----
  if (isTRUE(save_outputs)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    jpg_path <- file.path(output_dir, paste0(name, "divi.jpg"))
    svg_path <- file.path(output_dir, paste0(name, "divi.svg"))
    pdf_path <- paste0(jpg_path, ".pdf")
    
    jpeg(jpg_path, width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
    print(grid::grid.draw(g))
    dev.off()
    
    if (requireNamespace("daiR", quietly = TRUE)) {
      daiR::image_to_pdf(jpg_path, pdf_name = pdf_path)
    }
    my_image <- magick::image_read(jpg_path)
    my_svg   <- magick::image_convert(my_image, format = "svg")
    magick::image_write(my_svg, svg_path)
  }
  
  if (isTRUE(return_plot)) return(list(gtable = g, data = ResComb))
  # return the y-levels used (display labels in fixed order, subset by availability)
  return(levels(ResComb$name))
}



# library(readxl)
# library(ggplot2)
# library(ggforce)
# library(ggforestplot)
# library(grid)
# library(magick)
# # library(daiR)  # if you use daiR::image_to_pdf inside CalculateErrors
# 
# # groups2 mapping table (already in your session)
# groups2_df <- groups2
# stopifnot(all(c("Group","Abbreviation","Abbreviation_old","Name") %in% colnames(groups2_df)))
# 
# lm_path  <- "lms_tikka19324_v2.xlsx"
# lm_sheet <- "Steroids' adjusted lms"  # <-- use this sheet
# 
# 
# # Helper: read available Condition values from the LM sheet
# get_lm_conditions <- function(lm_path, lm_sheet) {
#   lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
#   # Normalize headers to find "Condition"
#   header_norm <- function(x) {
#     x <- tolower(as.character(x))
#     x <- gsub("[\r\n]+"," ", x); x <- gsub('"+', "", x)
#     x <- gsub("\\s+", " ", x); x <- trimws(x)
#     gsub("[^a-z0-9]","", x)
#   }
#   cn <- header_norm(colnames(lm_raw))
#   cond_col <- which(cn == "condition")[1]
#   if (is.na(cond_col)) stop("No 'Condition' column found in LM sheet.")
#   sort(unique(as.character(lm_raw[[cond_col]])))
# }
# 
# # Normalizer for matching "HOMA-IR" vs "HOMAIR"
# normalize_cond <- function(s) toupper(gsub("[^A-Za-z0-9]", "", s))
# 
# # Choose groups & display conditions you want
# groups <- c("All", "Female", "Male")
# conditions_display <- c("Steatosis", "Fibrosis", "Necroinflammation", "HOMA-IR")
# 
# available_conditions <- get_lm_conditions(lm_path, lm_sheet)
# 
# pick_lm_condition <- function(cond_display, available) {
#   target <- normalize_cond(cond_display)
#   idx <- which(normalize_cond(available) == target)
#   if (length(idx)) available[idx[1]] else cond_display
# }
# 
# batch_results <- list(); i <- 0
# for (grp in groups) {
#   for (cond_disp in conditions_display) {
#     
#     lm_cond <- pick_lm_condition(cond_disp, available_conditions)
#     plot_title <- paste0("Forest plot of ", grp, " Steroid Coefs in ", cond_disp)
#     
#     message("[INFO] Group=", grp, " | Condition=", cond_disp, " (LM uses: '", lm_cond, "')")
#     
#     res <- try({
#       CalculateErrors(
#         NonAlcoholicFattyLiverDisease = NAFLD_df,
#         OutcomeVariables = "Outcome",
#         Group = grp,
#         name = plot_title,
#         ordera = NULL, oute = NULL, first = TRUE, e = NULL, xlim = NULL,
#         groups2 = groups2_df,
#         lm_path = lm_path,
#         lm_sheet = lm_sheet,
#         lm_condition = lm_cond,   # explicit for robustness
#         use_qval = FALSE,         # flip TRUE if you want Q-values
#         # ci_level = 0.95,
#         alpha = 0.10
#       )
#     }, silent = TRUE)
#     
#     i <- i + 1
#     if (inherits(res, "try-error")) {
#       message("[WARN] Skipped: ", grp, " | ", cond_disp, " — ", conditionMessage(attr(res, "condition")))
#       batch_results[[i]] <- data.frame(Group=grp, Condition=cond_disp, Status="skipped", stringsAsFactors=FALSE)
#     } else {
#       batch_results[[i]] <- data.frame(Group=grp, Condition=cond_disp, Status="ok", stringsAsFactors=FALSE)
#     }
#   }
# }
# 
# do.call(rbind, batch_results)


run_all_combinations <- function(
    groups2,
    lm_path,
    lm_sheet = "Steroids' adjusted lms",
    groups = c("All", "Female", "Male"),
    conditions = c("Steatosis", "Fibrosis", "Necroinflammation", "HOMA-IR"),
    name_prefix = "Forest plot of",
    use_qval = FALSE,
    alpha = 0.10,
    se_multiplier = 0.5,       # ±(SE/2)
    strict_gender = TRUE,       # exact *_both/*_male/*_female
    output_dir = getwd(),
    save_outputs = TRUE,
    return_plots = FALSE,
    verbose = TRUE
) {
  stopifnot(is.data.frame(groups2))
  req_cols <- c("Group","Abbreviation","Abbreviation_old","Name")
  stopifnot(all(req_cols %in% colnames(groups2)))
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  # ---- COMPUTE FIXED ORDER ONCE ----
  fixed_group_levels <- unique(as.character(groups2$Group))  # facet order as in groups2
  fixed_y_levels <- unlist(lapply(fixed_group_levels, function(gl) {
    as.character(groups2$Abbreviation_old[groups2$Group == gl])
  }), use.names = FALSE)
  
  # ---- loop combos ----
  logs <- list()
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
          lm_condition = cond,            # handled robustly inside the function
          use_qval = use_qval,
          alpha = alpha,
          se_multiplier = se_multiplier,
          strict_gender = strict_gender,
          fixed_group_levels = fixed_group_levels,
          fixed_y_levels = fixed_y_levels,
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
        logs[[k]] <- data.frame(Group=g, Condition=cond, Status="ok", Message="", stringsAsFactors=FALSE)
        if (isTRUE(return_plots)) {
          plots[[paste(g, cond, sep=" | ")]] <- out$gtable
        }
      }
    }
  }
  log_df <- do.call(rbind, logs)
  if (isTRUE(return_plots)) return(list(log = log_df, plots = plots))
  log_df
}


# 0) Libraries
library(readxl)
library(ggplot2)
library(ggforce)
library(ggforestplot)
library(grid)
library(magick)
# library(daiR)

# 1) Ensure groups2 is loaded and in the exact display order you want (rows define order)
stopifnot(all(c("Group","Abbreviation","Abbreviation_old","Name") %in% colnames(groups2)))

# 2) Run all combos with fixed order, strict gender, ±SE/2 bars
log_df <- run_all_combinations(
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = "Steroids' adjusted lms",
  groups = c("All","Female","Male"),
  conditions = c("Steatosis","Fibrosis","Necroinflammation","HOMA-IR"),
  name_prefix = "Forest plot of",
  use_qval = FALSE,
  alpha = 0.10,
  se_multiplier = 0.5,       # ±(SE/2)
  strict_gender = TRUE,      # require *_both/*_male/*_female; stop per combo if missing
  output_dir = getwd(),
  save_outputs = TRUE,
  return_plots = FALSE,
  verbose = TRUE
)

log_df



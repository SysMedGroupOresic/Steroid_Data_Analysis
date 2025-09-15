CalculateErrors <- function(NonAlcoholicFattyLiverDisease,
                            OutcomeVariables,
                            Group,
                            name,
                            ordera = NULL,
                            oute = NULL,
                            first = FALSE,
                            e = NULL,
                            xlim = NULL,
                            groups2,
                            lm_path = "lms_tikka19324_v2.xlsx",
                            lm_sheet = NULL,
                            lm_condition = NULL,        # e.g. "Steatosis" / "Fibrosis" / "Necroinflammation"
                            use_qval = FALSE,           # color by Qval if available
                            ci_level = 0.90,
                            alpha = 0.10) {
  
  ## ------------------------ Safety checks ------------------------
  stopifnot(is.data.frame(groups2))
  needed_g2 <- c("Group","Abbreviation","Abbreviation_old","Name")
  if (!all(needed_g2 %in% colnames(groups2))) {
    stop("groups2 must contain columns: ", paste(needed_g2, collapse=", "))
  }
  if (!file.exists(lm_path)) stop("LM results file not found: ", lm_path)
  
  ## ------------------------ Helpers ------------------------
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])","",x,perl=TRUE)  # drop leading X before digit
    gsub("[^A-Z0-9]","",x)
  }
  # normalize headers: lowercase + remove all non-alphanumerics
  header_norm <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[\r\n]+"," ", x)
    x <- gsub('"+', "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    gsub("[^a-z0-9]", "", x)
  }
  parse_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x <- gsub("\\s+","",x)
    x <- gsub(",",".",x)
    suppressWarnings(as.numeric(x))
  }
  infer_condition <- function(s) {
    if (is.null(s)) return(NA_character_)
    s <- tolower(as.character(s))
    if (grepl("steatosis", s)) return("Steatosis")
    if (grepl("fibrosis", s)) return("Fibrosis")
    if (grepl("necroinflammation|necro-inflammation|necro inflammation", s)) return("Necroinflammation")
    NA_character_
  }
  
  ## ------------------------ Read Excel & normalize headers ------------------------
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  if (nrow(lm_raw) == 0) stop("LM file is empty.")
  
  cn <- colnames(lm_raw)
  cn_norm <- header_norm(cn)
  
  # Required headers IN NORMALIZED FORM (punctuation/spacing agnostic)
  # Matches both "N not zero" and "N.not.zero" as "nnotzero", etc.
  req_targets <- c(
    steroid = "steroid",
    gender  = "genderincondition",
    cond    = "condition",
    coef    = "coef",
    se      = "stderr",
    pval    = "pval",
    qval    = "qval",
    n       = "n",
    n0      = "nnotzero"
  )
  
  # Find the index of each required column by normalized name (first match)
  pick_idx <- function(target) {
    hit <- which(cn_norm == target)
    if (length(hit)) hit[1] else NA_integer_
  }
  idxs <- vapply(req_targets, pick_idx, integer(1L))
  
  # If any required is missing, give a helpful error & list what we saw
  if (any(is.na(idxs))) {
    missing_keys <- names(req_targets)[is.na(idxs)]
    stop(
      "Required columns not found in LM file: ",
      paste(missing_keys, collapse = ", "),
      "\nHeaders (normalized) seen:\n  ",
      paste(unique(cn_norm), collapse = " | "),
      "\nTip: Make sure your sheet has columns equivalent to:\n  Steroid | Gender in condition | Condition | Coef | Stderr | Pval | Qval | N | N not zero\n",
      "We match headers ignoring punctuation/spaces/case."
    )
  }
  
  # Build long DF directly (your file is single block with many rows)
  LM_long <- lm_raw[, idxs, drop = FALSE]
  colnames(LM_long) <- c("Steroid","Gender in condition","Condition","Coef","Stderr","Pval","Qval","N","N not zero")
  
  ## ------------------------ Coerce numeric & clean values ------------------------
  LM_long$Coef        <- parse_num(LM_long$Coef)
  LM_long$Stderr      <- parse_num(LM_long$Stderr)
  LM_long$Pval        <- parse_num(LM_long$Pval)
  LM_long$Qval        <- parse_num(LM_long$Qval)
  LM_long$N           <- suppressWarnings(as.integer(parse_num(LM_long$N)))
  LM_long$`N not zero`<- suppressWarnings(as.integer(parse_num(LM_long$`N not zero`)))
  
  # Keep rows with finite Coef and Stderr
  LM_long <- LM_long[is.finite(LM_long$Coef) & is.finite(LM_long$Stderr), , drop = FALSE]
  if (nrow(LM_long) == 0) stop("No rows with finite Coef and Stderr after parsing the LM file.")
  
  ## ------------------------ Choose Condition & Gender ------------------------
  if (is.null(lm_condition)) {
    lm_condition <- infer_condition(name)
    if (is.na(lm_condition)) {
      conds <- sort(unique(as.character(LM_long$Condition)))
      if (length(conds) == 1) {
        lm_condition <- conds[1]
      } else {
        stop("Please set lm_condition explicitly (e.g., 'Steatosis'). Available: ",
             paste(conds, collapse = ", "))
      }
    }
  }
  
  target_gender_token <- switch(tolower(Group),
                                "all" = "both",
                                "male" = "male",
                                "female" = "female",
                                "both")
  
  rows_cond   <- tolower(as.character(LM_long$Condition)) == tolower(lm_condition)
  rows_gender <- grepl(target_gender_token, tolower(as.character(LM_long$`Gender in condition`)))
  LM_sel <- LM_long[rows_cond & rows_gender, , drop = FALSE]
  if (nrow(LM_sel) == 0) {
    # fallback to condition-only if gender token doesn’t exist
    LM_sel <- LM_long[rows_cond, , drop = FALSE]
    if (nrow(LM_sel) == 0) {
      stop("No LM rows found for Condition='", lm_condition, "' (and Group='", Group, "').")
    } else {
      warning("No rows matched Gender token '", target_gender_token,
              "' for Condition='", lm_condition, "'. Using all genders for this condition.")
    }
  }
  LM_sel <- unique(LM_sel)
  
  ## ------------------------ Build ResComb (Coef ± z*SE) ------------------------
  # Pretty display name
  name_display <- LM_sel$Steroid
  name_display <- gsub("\\.", "-", name_display)
  name_display <- sub("^X11", "11", name_display)
  name_display <- sub("^X17", "17", name_display)
  name_display[name_display == "T-Epi-T"] <- "T/Epi-T"
  
  p_used <- if (isTRUE(use_qval) && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  
  ResComb <- data.frame(
    name      = name_display,
    name_raw  = LM_sel$Steroid,
    result    = LM_sel$Coef,
    se_lm     = LM_sel$Stderr,
    pval      = p_used,
    stringsAsFactors = FALSE
  )
  
  # z_ci <- qnorm(1 - (1 - ci_level)/2)
  # ResComb$errord1 <- ResComb$result - z_ci * ResComb$se_lm
  # ResComb$errord2 <- ResComb$result + z_ci * ResComb$se_lm
  
  # inside CalculateErrors(), replace the CI block with:
  half_se <- ResComb$se_lm / 2
  ResComb$errord1 <- ResComb$result - half_se
  ResComb$errord2 <- ResComb$result + half_se
  
  
  ResComb$pval0 <- ResComb$pval
  ResComb$pval1 <- ResComb$pval
  ResComb$Significance0 <- ifelse(ResComb$pval0 < alpha, 'Yes', 'No')
  ResComb$Color0        <- ifelse(ResComb$pval0 < alpha, 'blue', 'grey')
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, 'Yes', 'No')
  ResComb$Color1        <- ifelse(ResComb$pval1 < alpha, 'blue', 'grey')
  
  ## ------------------------ Map steroid Groups via groups2 ------------------------
  key_tbl <- unique(do.call(rbind, list(
    data.frame(key = as.character(groups2$Abbreviation),     Group = groups2$Group, stringsAsFactors = FALSE),
    data.frame(key = as.character(groups2$Abbreviation_old), Group = groups2$Group, stringsAsFactors = FALSE),
    data.frame(key = as.character(groups2$Name),             Group = groups2$Group, stringsAsFactors = FALSE)
  )))
  key_tbl <- key_tbl[!is.na(key_tbl$key) & nzchar(key_tbl$key), ]
  key_tbl$key_norm <- norm_key(key_tbl$key)
  ResComb$key_norm <- norm_key(ResComb$name)
  
  MapDF <- merge(ResComb[, c("name", "key_norm")],
                 unique(key_tbl[, c("key_norm", "Group")]),
                 by = "key_norm", all.x = TRUE)
  ResComb$Group <- MapDF$Group[match(ResComb$key_norm, MapDF$key_norm)]
  
  unmapped <- sort(unique(ResComb$name[is.na(ResComb$Group)]))
  if (length(unmapped)) {
    warning("Unmapped analytes (not found in groups2): ", paste(unmapped, collapse = ", "))
  }
  
  group_levels <- unique(groups2$Group)
  ResComb$Group <- factor(ResComb$Group, levels = group_levels)
  
  ## ------------------------ Ordering (y-axis) ------------------------
  if (is.null(ordera)) {
    g2_abbr  <- groups2$Abbreviation
    ordera_n <- norm_key(g2_abbr)
    present  <- g2_abbr[ordera_n %in% unique(ResComb$key_norm)]
    ordera   <- present
    if (identical(Group, "All") && isTRUE(first)) ordera <- rev(ordera)
  } else {
    if (identical(Group, "All") && isTRUE(first)) ordera <- rev(ordera)
  }
  ordera_norm    <- norm_key(ordera)
  display_levels <- ResComb$name[match(ordera_norm, ResComb$key_norm)]
  display_levels <- display_levels[!is.na(display_levels)]
  if (length(display_levels) == 0) display_levels <- ResComb$name
  ResComb$name <- factor(ResComb$name, levels = unique(display_levels))
  
  ResComb$Group2 <- as.numeric(as.factor(ResComb$Group))
  
  ## ------------------------ xlim ------------------------
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb$errord1, ResComb$errord2), na.rm = TRUE)
  }
  
  ## ------------------------ Forest plot ------------------------
  plote2 <- forestplot(
    df       = ResComb,
    estimate = result,
    se       = 0,   # we draw CI with geom_errorbarh
    pvalue   = pval1,
    psignif  = alpha,
    xlim     = xlim,
    xlab     = paste0('LM Coef (', round(ci_level*100), '% CI) for ', lm_condition,
                      if (!is.null(Group)) paste0(' (', Group, ')') else ''),
    ylab     = 'Steroid Groups',
    title    = '',
    colour   = Significance1
  ) +
    ggforce::facet_col(facets = ~Group, scales = "free_y", space = "free", strip.position = 'left') +
    geom_errorbarh(aes(xmin = errord1, xmax = errord2, height = .0, colour = Significance1))
  
  hp <- if (sum(ResComb$Significance1 == 'Yes', na.rm = TRUE) == nrow(ResComb)) {
    c('blue', 'blue')
  } else {
    c('#999999', 'blue')
  }
  try({ plote2$layers[[1]]$aes_params$odd <- "#00000000" }, silent = TRUE)
  
  PlotVar  <- plote2 + theme(axis.text.y = element_blank()) + theme_classic2()
  PlotVar2 <- PlotVar +
    geom_point(aes(colour = factor(Significance1)), colour = ResComb$Color1) +
    scale_color_manual(values = hp) +
    theme(legend.position = "none") +
    theme(strip.text.y = element_text(size = -Inf))
  
  # Customize facet strip fills
  g      <- ggplot_gtable(ggplot_build(PlotVar2))
  stripr <- which(grepl('strip-l', g$layout$name))
  fills  <- c("red", "green", "blue", "yellow", "brown")
  if (length(stripr) > 0) {
    fills <- rep(fills, length.out = length(stripr))
    for (i in seq_along(stripr)) {
      j <- tryCatch(which(grepl('rect', g$grobs[[stripr[i]]]$grobs[[1]]$childrenOrder)),
                    error = function(e) integer(0))
      if (length(j) > 0) {
        g$grobs[[stripr[i]]]$grobs[[1]]$children[[j]]$gp$fill <- fills[i]
      }
    }
  }
  
  ## ------------------------ Save outputs ------------------------
  # Adjust or remove setwd() if not appropriate for your environment
  setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")
  
  jpeg(paste0(name, "divi.jpg"), width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
  print(grid::grid.draw(g))
  dev.off()
  
  if (requireNamespace("daiR", quietly = TRUE)) {
    daiR::image_to_pdf(paste0(name, "divi.jpg"), pdf_name = paste0(paste0(name, "divi.jpg"), '.pdf'))
  }
  my_image <- magick::image_read(paste0(name, "divi.jpg"))
  my_svg   <- magick::image_convert(my_image, format = "svg")
  magick::image_write(my_svg, paste0(name, "divi.svg"))
  
  return(ordera)
}

groups2_df <- groups2  # or readxl::read_excel("path/to/groups2.xlsx")

name1 <- "Forest plot of All Steroid Ratios in Steatosis"

ord <- CalculateErrors(
  NonAlcoholicFattyLiverDisease = NAFLD_df,   # kept for compatibility
  OutcomeVariables = "Outcome",               # ignored by LM pipeline
  Group = "All",                              # -> matches *_both in "Gender in condition"
  name = name1,
  ordera = NULL,
  oute = NULL,
  first = TRUE,
  e = NULL,
  xlim = NULL,
  groups2 = groups2_df,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_sheet = NULL,
  lm_condition = NULL,                        # inferred from name: "Steatosis"
  use_qval = FALSE,
  ci_level = 0.95,
  alpha = 0.10
)


library(readxl)
library(ggplot2)
library(ggforce)
library(ggforestplot)
library(grid)
library(magick)
# library(daiR)  # if you use daiR::image_to_pdf inside CalculateErrors

# groups2 mapping table
groups2_df <- groups2  # or readxl::read_excel("path/to/groups2.xlsx")
stopifnot(all(c("Group","Abbreviation","Abbreviation_old","Name") %in% colnames(groups2_df)))

# Optional: set where you want outputs saved; OR remove setwd() in the function.
setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")


lm_path <- "lms_tikka19324_v2.xlsx"
lm_raw  <- readxl::read_excel(lm_path, .name_repair = "minimal")

# Normalize header names lightly to pick out the three columns we need
norm <- function(x) {x <- tolower(gsub("[^a-z0-9]", "", x)); x}
cn <- norm(colnames(lm_raw))
pick <- function(target) which(cn == target)[1]

lm_min <- data.frame(
  Steroid = lm_raw[[pick("Steroid")]],
  GenderInCond = lm_raw[[pick("Gender in Condition")]],
  Condition = lm_raw[[pick("Condition")]]
)

cat("Unique Condition values:\n")
print(sort(unique(as.character(lm_min$Condition))))
cat("\nExamples of 'Gender in condition' values:\n")
print(head(unique(as.character(lm_min$GenderInCond)), 20))


library(readxl)
library(ggplot2)
library(ggforce)
library(ggforestplot)
library(grid)
library(magick)
# library(daiR)  # if you use daiR::image_to_pdf inside CalculateErrors

# groups2 mapping table (already in your session)
groups2_df <- groups2
stopifnot(all(c("Group","Abbreviation","Abbreviation_old","Name") %in% colnames(groups2_df)))

lm_path  <- "lms_tikka19324_v2.xlsx"
lm_sheet <- "Steroids' adjusted lms"  # <-- use this sheet


# Helper: read available Condition values from the LM sheet
get_lm_conditions <- function(lm_path, lm_sheet) {
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  # Normalize headers to find "Condition"
  header_norm <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[\r\n]+"," ", x); x <- gsub('"+', "", x)
    x <- gsub("\\s+", " ", x); x <- trimws(x)
    gsub("[^a-z0-9]","", x)
  }
  cn <- header_norm(colnames(lm_raw))
  cond_col <- which(cn == "condition")[1]
  if (is.na(cond_col)) stop("No 'Condition' column found in LM sheet.")
  sort(unique(as.character(lm_raw[[cond_col]])))
}

# Normalizer for matching "HOMA-IR" vs "HOMAIR"
normalize_cond <- function(s) toupper(gsub("[^A-Za-z0-9]", "", s))

# Choose groups & display conditions you want
groups <- c("All", "Female", "Male")
conditions_display <- c("Steatosis", "Fibrosis", "Necroinflammation", "HOMA-IR")

available_conditions <- get_lm_conditions(lm_path, lm_sheet)

pick_lm_condition <- function(cond_display, available) {
  target <- normalize_cond(cond_display)
  idx <- which(normalize_cond(available) == target)
  if (length(idx)) available[idx[1]] else cond_display
}

batch_results <- list(); i <- 0
for (grp in groups) {
  for (cond_disp in conditions_display) {
    
    lm_cond <- pick_lm_condition(cond_disp, available_conditions)
    plot_title <- paste0("Forest plot of ", grp, " Steroid Coefs in ", cond_disp)
    
    message("[INFO] Group=", grp, " | Condition=", cond_disp, " (LM uses: '", lm_cond, "')")
    
    res <- try({
      CalculateErrors(
        NonAlcoholicFattyLiverDisease = NAFLD_df,
        OutcomeVariables = "Outcome",
        Group = grp,
        name = plot_title,
        ordera = NULL, oute = NULL, first = TRUE, e = NULL, xlim = NULL,
        groups2 = groups2_df,
        lm_path = lm_path,
        lm_sheet = lm_sheet,
        lm_condition = lm_cond,   # explicit for robustness
        use_qval = FALSE,         # flip TRUE if you want Q-values
        # ci_level = 0.95,
        alpha = 0.10
      )
    }, silent = TRUE)
    
    i <- i + 1
    if (inherits(res, "try-error")) {
      message("[WARN] Skipped: ", grp, " | ", cond_disp, " — ", conditionMessage(attr(res, "condition")))
      batch_results[[i]] <- data.frame(Group=grp, Condition=cond_disp, Status="skipped", stringsAsFactors=FALSE)
    } else {
      batch_results[[i]] <- data.frame(Group=grp, Condition=cond_disp, Status="ok", stringsAsFactors=FALSE)
    }
  }
}

do.call(rbind, batch_results)






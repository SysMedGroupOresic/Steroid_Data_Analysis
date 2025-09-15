
#' CalculateErrors
#' Builds ratio (case/control) with log CI, maps groups via groups2, creates forest plot, saves images.
#' @param NonAlcoholicFattyLiverDisease data.frame
#' @param OutcomeVariables character; binary outcome column in NAFLD data (0 vs >0)
#' @param Group character; "Male" | "Female" | "All"
#' @param name character; output file stem (e.g., "MyPlot")
#' @param ordera character vector of abbreviations to order rows; if NULL, inferred from groups2
#' @param oute (kept for compatibility; unused in this function)
#' @param first logical; when Group=="All" and first==TRUE, reverse order
#' @param e character; reference analyte used for ratio construction (kept from your code)
#' @param xlim numeric length-2 or NULL; x axis limits (log scale). If NULL, inferred from data
#' @param groups2 data.frame with columns: Group, Abbreviation, Abbreviation_old, Name
#' @return ordera vector used for the y order

CalculateErrors <- function(NonAlcoholicFattyLiverDisease,
                            OutcomeVariables,
                            Group,
                            name,
                            ordera = NULL,
                            oute = NULL,
                            first = FALSE,
                            e,
                            xlim = NULL,
                            groups2) {
  
  ## ---- Safety checks ----
  stopifnot(is.data.frame(NonAlcoholicFattyLiverDisease))
  if (!OutcomeVariables %in% colnames(NonAlcoholicFattyLiverDisease)) {
    stop("OutcomeVariables='", OutcomeVariables, "' not found in data.")
  }
  if (!'SEX.1F.2M' %in% colnames(NonAlcoholicFattyLiverDisease)) {
    stop("'SEX.1F.2M' column not found in data.")
  }
  needed_g2 <- c("Group", "Abbreviation", "Abbreviation_old", "Name")
  if (!all(needed_g2 %in% colnames(groups2))) {
    stop("groups2 must contain columns: ", paste(needed_g2, collapse = ", "))
  }
  
  ## ---- Helper: name normalization used for matching ----
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])", "", x, perl = TRUE) # drop leading X before digits
    gsub("[^A-Z0-9]", "", x)                     # strip non-alphanumerics
  }
  
  ## ---- 1) Filter by Group ----
  NAFLDo <- switch(Group,
                   "Male"   = NonAlcoholicFattyLiverDisease[NonAlcoholicFattyLiverDisease[, 'SEX.1F.2M'] == 2, ],
                   "Female" = NonAlcoholicFattyLiverDisease[NonAlcoholicFattyLiverDisease[, 'SEX.1F.2M'] == 1, ],
                   "All"    = NonAlcoholicFattyLiverDisease,
                   stop("Group must be one of 'Male', 'Female', 'All'."))
  if (nrow(NAFLDo) == 0) stop("No rows after filtering by Group = '", Group, "'.")
  
  ## ---- 2) Find analyte (steroid) columns dynamically via groups2 ----
  keys   <- unique(na.omit(c(groups2$Abbreviation, groups2$Abbreviation_old, groups2$Name)))
  keys_n <- norm_key(keys)
  cn     <- colnames(NAFLDo)
  cn_n   <- norm_key(cn)
  
  match_idx    <- which(cn_n %in% keys_n)
  measure_cols <- cn[match_idx]
  # Exclude non-measure columns
  measure_cols <- setdiff(measure_cols, c(OutcomeVariables, "SEX.1F.2M"))
  # Keep numeric
  measure_cols <- measure_cols[sapply(measure_cols, function(v) is.numeric(NAFLDo[[v]]))]
  
  if (length(measure_cols) == 0) {
    stop(
      "Could not find any analyte columns by matching groups2 to your data.\n",
      "Check that your data column names correspond to groups2 Abbreviation/Abbreviation_old/Name."
    )
  }
  
  ## ---- 3) Split by outcome and summarize ----
  sample_data <- vector("list", 2)
  n0 <- n1 <- 0
  
  for (i in 1:2) {
    SG0 <- if (i == 1) {
      NAFLDo[NAFLDo[[OutcomeVariables]] == 0, , drop = FALSE]
    } else {
      NAFLDo[NAFLDo[[OutcomeVariables]] > 0, , drop = FALSE]
    }
    if (i == 1) n0 <- nrow(SG0) else n1 <- nrow(SG0)
    if (nrow(SG0) == 0) stop("Outcome split ", i, " has zero rows. Check '", OutcomeVariables, "' coding.")
    
    means <- sapply(measure_cols, function(v) median(SG0[[v]], na.rm = TRUE))
    sds   <- sapply(measure_cols, function(v) sd(SG0[[v]],     na.rm = TRUE))
    
    sample_data[[i]] <- data.frame(
      study  = measure_cols,
      index  = measure_cols,
      result = as.numeric(means),
      error  = as.numeric(sds),
      stringsAsFactors = FALSE
    )
  }
  
  # Merge (control arm i=1 with case arm i=2) by analyte name
  df <- merge(sample_data[[1]], sample_data[[2]], by = c("study", "index"), suffixes = c("", ".1"))
  
  ## ---- 4) Wilcoxon p-values per analyte (vector interface, avoids formula pitfalls) ----
  ov <- NAFLDo[[OutcomeVariables]]
  ctrl_rows <- ov == 0
  case_rows <- ov > 0
  
  ps <- sapply(measure_cols, function(v) {
    x <- NAFLDo[ctrl_rows, v]
    y <- NAFLDo[case_rows, v]
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    if (length(x) < 1 || length(y) < 1) return(NA_real_)
    tryCatch(wilcox.test(x, y, exact = FALSE)$p.value, error = function(e) NA_real_)
  })
  names(ps) <- measure_cols
  ## ---- 5) Ratio/log values & cleanup ----
  # Keep your 'e' reference ratio (not used downstream, but retained)
  suppressWarnings({
    a <- tryCatch(
      df[df[, "study"] == e, 'result.1'] / df[df[, "study"] == e, 'result'],
      error = function(e) NA_real_
    )
  })
  
  ResComb <- data.frame(log(df$result.1 / df$result))
  ResComb$result   <- ResComb[, 1]
  ResComb$name_raw <- df$study
  ResComb          <- ResComb[, c("result", "name_raw")]
  
  # Pretty names for plotting (match your prior cleanup)
  ResComb$name <- ResComb$name_raw
  ResComb$name <- gsub("\\.", "-", ResComb$name)
  ResComb$name <- sub("^X11", "11", ResComb$name)
  ResComb$name <- sub("^X17", "17", ResComb$name)
  ResComb$name[ResComb$name == "T-Epi-T"] <- "T/Epi-T"
  
  # Attach p‑values by original raw colname
  ResComb$pval <- ps[match(ResComb$name_raw, names(ps))]
  
  ## ---- 6) Error / CI on ratio (as in your code) ----
  ResComb$result_pure <- df$result.1 / df$result
  ResComb$error <- (abs((1 / df$result) * df$error.1) +
                      abs((df$result.1 / (df$result^2)) * df$error)) / nrow(NAFLDo) * 1.64
  
  # Cap extremes
  medE <- median(ResComb$error, na.rm = TRUE)
  sdE  <- sd(ResComb$error,     na.rm = TRUE)
  ResComb$error <- ifelse(ResComb$error > (medE + sdE), medE * 1.25, ResComb$error)
  
  # Bounds + log transform (guard negative/zero to avoid -Inf)
  ResComb$errord1a <- pmax(ResComb$result_pure - ResComb$error, .Machine$double.eps)
  ResComb$errord2a <- pmax(ResComb$result_pure + ResComb$error, .Machine$double.eps)
  ResComb$errord1  <- log(ResComb$errord1a)
  ResComb$errord2  <- log(ResComb$errord2a)
  ResComb$result   <- log(ResComb$result_pure)
  ResComb$Control  <- df$result
  ResComb$Case     <- df$result.1
  # P-values & flags
  ResComb$pval0 <- ResComb$pval
  ResComb$pval1 <- ResComb$pval
  ResComb$Significance0 <- ifelse(ResComb$pval0 < 0.1, 'Yes', 'No')
  ResComb$Color0        <- ifelse(ResComb$pval0 < 0.1, 'blue', 'grey')
  ResComb$Significance1 <- ifelse(ResComb$pval1 < 0.1, 'Yes', 'No')
  ResComb$Color1        <- ifelse(ResComb$pval1 < 0.1, 'blue', 'grey')
  
  ## ---- 7) Map Group via groups2 (match by Abbrev / old / Name) ----
  key_tbl <- unique(do.call(rbind, list(
    data.frame(key = as.character(groups2$Abbreviation),     Group = groups2$Group, stringsAsFactors = FALSE),
    data.frame(key = as.character(groups2$Abbreviation_old), Group = groups2$Group, stringsAsFactors = FALSE),
    data.frame(key = as.character(groups2$Name),             Group = groups2$Group, stringsAsFactors = FALSE)
  )))
  key_tbl <- key_tbl[!is.na(key_tbl$key) & nzchar(key_tbl$key), ]
  key_tbl$key_norm <- norm_key(key_tbl$key)
  ResComb$key_norm <- norm_key(ResComb$name)
  
  map_df <- merge(ResComb[, c("name", "key_norm")],
                  unique(key_tbl[, c("key_norm", "Group")]),
                  by = "key_norm", all.x = TRUE)
  ResComb$Group <- map_df$Group[match(ResComb$key_norm, map_df$key_norm)]
  
  # Warn on unmapped
  unmapped <- sort(unique(ResComb$name[is.na(ResComb$Group)]))
  if (length(unmapped)) {
    warning("Some analytes could not be mapped to a group in groups2: ",
            paste(unmapped, collapse = ", "))
  }
  
  # Facet order: as in groups2
  group_levels <- unique(groups2$Group)
  ResComb$Group <- factor(ResComb$Group, levels = group_levels)
  
  ## ---- 8) Order y-axis levels (names) ----
  if (is.null(ordera)) {
    g2_abbr  <- groups2$Abbreviation
    ordera_n <- norm_key(g2_abbr)
    present  <- g2_abbr[ordera_n %in% unique(ResComb$key_norm)]
    ordera   <- present
    if (identical(Group, "All") && isTRUE(first)) ordera <- rev(ordera)
  } else {
    if (identical(Group, "All") && isTRUE(first)) ordera <- rev(ordera)
  }
  
  # Convert ordera (abbreviations) to displayed levels present in ResComb$name
  ordera_norm     <- norm_key(ordera)
  display_levels  <- ResComb$name[match(ordera_norm, ResComb$key_norm)]
  display_levels  <- display_levels[!is.na(display_levels)]
  if (length(display_levels) == 0) display_levels <- ResComb$name
  ResComb$name <- factor(ResComb$name, levels = unique(display_levels))
  
  # Numeric group index for strip fills
  ResComb$Group2 <- as.numeric(as.factor(ResComb$Group))
  
  ## ---- 9) xlim ----
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb$errord1, ResComb$errord2), na.rm = TRUE)
  }
  
  ## ---- 10) Build forest plot ----
  plote2 <- forestplot(
    df       = ResComb,
    estimate = result,
    se       = 0,
    pvalue   = pval1,
    psignif  = 0.1,
    xlim     = xlim,
    xlab     = 'Logged Ratio between Raw Concentrations of Case and Control with 90% CI',
    ylab     = 'Steroid Groups',
    title    = '',
    colour   = Significance1
  ) +
    ggforce::facet_col(facets = ~Group, scales = "free_y", space = "free", strip.position = 'left') +
    geom_errorbarh(aes(xmin = errord1, xmax = errord2, height = .0, colour = Significance1))
  
  # Point color palette
  hp <- if (sum(ResComb$Significance1 == 'Yes', na.rm = TRUE) == length(levels(ResComb$name))) {
    c('blue', 'blue')
  } else {
    c('#999999', 'blue')
  }
  
  # Try stripes transparency (guard in case layer indexing differs)
  try({ plote2$layers[[1]]$aes_params$odd <- "#00000000" }, silent = TRUE)
  
  PlotVar  <- plote2 + theme(axis.text.y = element_blank()) + theme_classic2()
  PlotVar2 <- PlotVar +
    geom_point(aes(colour = factor(Significance1)), colour = ResComb$Color1) +
    scale_color_manual(values = hp) +
    theme(legend.position = "none") +
    theme(strip.text.y = element_text(size = -Inf))
  
  # Customize facet strip colors
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
  
  ## ---- 11) Save outputs ----
  setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")
  
  jpeg(paste0(name, "divi.jpg"), width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
  print(grid::grid.draw(g))
  dev.off()
  
  daiR::image_to_pdf(paste0(name, "divi.jpg"), pdf_name = paste0(paste0(name, "divi.jpg"), '.pdf'))
  my_image <- magick::image_read(paste0(name, "divi.jpg"))
  my_svg   <- magick::image_convert(my_image, format = "svg")
  magick::image_write(my_svg, paste0(name, "divi.svg"))
  
  return(ordera)
}



# This is with first(!!). Use it. 
OutcomeVariables='Steatosis.Grade.0.To.3';Out='Steatosis'; oute='Steatosis';first=TRUE; e='P4';ordera=c();
Group='All';name1=paste("Forest plot ofu",Group, "Steroid Ratios in",Out);
hel=CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name1,ordera,oute,first,e,xlim, groups2)
# #Afterwards:
first=FALSE;
Group='Female';name2=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name2,ordera=hel,oute,first,e,xlim)
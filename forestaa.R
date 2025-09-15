# Making Forest Plots  
# ```{r, warning=FALSE,message=FALSE,fig.width=6.0,fig.align="left",results='hide'} 

# Define the NonAlcoholicFattyLiverDisease dataset by selecting the first 28 columns from CombinedData
NonAlcoholicFattyLiverDisease <- CombinedData[, 1:28]
# Convert specific columns to binary values using vectorized operations
cols_to_binary <- c(5, 6, 7)
NonAlcoholicFattyLiverDisease[, cols_to_binary] <- (NonAlcoholicFattyLiverDisease[, cols_to_binary] > 0) * 1
# Convert column 8 to binary based on the threshold of 1.5
NonAlcoholicFattyLiverDisease[, 8] <- (NonAlcoholicFattyLiverDisease[, 8] > 1.5) * 1
# Clean column names to remove special characters and make uniqueBMIValues consistent
patterns <- c("-", "/", "11", "17", "#")
replacements <- c(".", ".", "X11", "X17", ".") #?
# Ensure patterns and replacements are correctly paired
if (length(patterns) == length(replacements)) {
  for (i in seq_along(patterns)) {
    colnames(NonAlcoholicFattyLiverDisease) <- gsub(patterns[i], replacements[i], colnames(NonAlcoholicFattyLiverDisease))}} else {
      stop("Patterns and replacements vectors must be of the same length.")}

steroidGroups[steroidGroups[,1]=='DOC',2]='Mineralocorticoids'


# This works with the autoscaled (raw if loge=1 and remove 1 in the means) data NonAlcoholicFattyLiverDisease as well...
CalculateErrors=function(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name,ordera,oute,first,e,xlim) { # Group='Female'
  
  # Filter data based on the 'Group' variable
  NAFLDo <- switch(Group,
                   "Male" = NonAlcoholicFattyLiverDisease[NonAlcoholicFattyLiverDisease[,'SEX.1F.2M'] == 2, ],
                   "Female" = NonAlcoholicFattyLiverDisease[NonAlcoholicFattyLiverDisease[,'SEX.1F.2M'] == 1, ],
                   "All" = NonAlcoholicFattyLiverDisease)
  
  # Initialize vectors to store sample data and counts
  sample_data <- list();
  n0 <- n1 <- 0
  
  # Loop through the two steroidGroups (OutcomeVariables == 0 and OutcomeVariables > 0)
  for (i in 1:2) {
    SG0 <- if (i == 1) {
      NAFLDo[NAFLDo[, OutcomeVariables] == 0, ]
    } else {
      NAFLDo[NAFLDo[, OutcomeVariables] > 0, ]}
    
    # Store the count of samples in each group
    if (i == 1) {
      n0 <- nrow(SG0)
    } else {
      n1 <- nrow(SG0)}
    
    # Calculate medians and standard deviations for columns 9 to 28
    means <- apply(SG0[, 9:28], 2, median, na.rm = TRUE)
    sds <- apply(SG0[, 9:28], 2, sd, na.rm = TRUE)
    
    # Calculate error margins
    error_lower <- means - sds
    error_upper <- means + sds
    error <- sds
    
    # Append results to sample_data
    sample_data[[i]] <- data.frame(study = colnames(NonAlcoholicFattyLiverDisease[, 9:28]),
                                   index = colnames(NonAlcoholicFattyLiverDisease[, 9:28]),
                                   result = means,
                                   error = error)}
  df=data.frame(sample_data) #
  
  # Calculate p-values using Wilcoxon test for columns 9 to 28
  ps <- sapply(9:28, function(j) {
    xnam <- colnames(NAFLDo)[j]
    fmla <- as.formula(paste(xnam, "~", OutcomeVariables))
    wilcox.test(fmla, data = NAFLDo, exact = FALSE)$p.value})
  #https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test
  
  # Calculate the ratio of results and log-transform
  a <- df[df[, 1] == e, 'result.1'] / df[df[, 1] == e, 'result']
  ResComb <- data.frame(log(df$result.1 / df$result))
  
  # Rename columns and clean up variable names
  ResComb$result <- ResComb[, 1]
  ResComb$name <- df$study
  ResComb <- ResComb[, 2:3]
  ResComb$name <- gsub("\\.", "-", ResComb$name)
  ResComb$name <- gsub("X11", "11", ResComb$name)
  ResComb$name <- gsub("X17", "17", ResComb$name)
  ResComb$name[ResComb$name == "T-Epi-T"] <- "T/Epi-T"
  ResComb$pval <- ps
  
  # Calculate result_pure and error
  ResComb$result_pure <- df$result.1 / df$result
  ResComb$error <- (abs((1 / df$result) * df$error.1) + abs((df$result.1 / df$result^2) * df$error)) / nrow(NAFLDo) * 1.64
  
  # Adjust error values
  ResComb$error <- ifelse(ResComb$error > (median(ResComb$error) + sd(ResComb$error)), median(ResComb$error) * 1.25, ResComb$error)
  
  # Calculate error bounds and log-transformed values
  ResComb$errord1a <- ResComb$result_pure - ResComb$error
  ResComb$errord2a <- ResComb$result_pure + ResComb$error
  ResComb$errord1 <- log(ResComb$errord1a)
  ResComb$errord2 <- log(ResComb$errord2a)
  ResComb$result <- log(ResComb$result_pure)
  ResComb$Control <- df$result
  ResComb$Case <- df$result.1
  
  # Add p-values and significance
  ResComb$pval0 <- ResComb$pval
  ResComb$pval1 <- ResComb$pval
  ResComb$Significance0 <- ifelse(ResComb$pval0 < 0.1, 'Yes', 'No')
  ResComb$Color0 <- ifelse(ResComb$pval0 < 0.1, 'blue', 'grey')
  ResComb$Significance1 <- ifelse(ResComb$pval1 < 0.1, 'Yes', 'No')
  ResComb$Color1 <- ifelse(ResComb$pval1 < 0.1, 'blue', 'grey')
  
  # Merge with group data and sort
  gn <- steroidGroups[steroidGroups$Abbreviation != 'F', c('Group', 'Abbreviation')]
  gn <- gn[order(gn$Abbreviation), ]
  ResComb <- ResComb[order(ResComb$name), ]
  ResComb <- cbind(ResComb, gn[order(gn$Abbreviation), ])
  ResComb <- ResComb[order(-ResComb$result), ]
  
  xlab = "Autoscaled Concentrations (SE)"
  xlim=c(min(ResComb$errord1),max(ResComb$errord2))
  
  # Create forest plot
  plote2 <- forestplot(df = ResComb,
                       estimate = result,
                       se = 0,
                       pvalue = pval1,
                       psignif = 0.1,
                       xlim = xlim,
                       xlab = 'Logged Ratio between Raw Concentrations of Case and Control with 90% CI',
                       ylab = 'Steroid Groups',
                       title = '',
                       colour = Significance1) +
    ggforce::facet_col(facets = ~Group, scales = "free_y", space = "free", strip.position = 'left') +
    geom_errorbarh(aes(xmin = errord1, xmax = errord2, height = .0, colour = Significance1))
  
  # Set color palette
  hp <- if (sum(ResComb$Significance1 == 'Yes') == 20) c('blue', 'blue') else c('#999999', 'blue')
  
  # Order factor levels based on Group and first
  if (Group=='All' & first==TRUE) {ordera=rev(steroidGroups$Abbreviation)#ResComb$name[order(ResComb$result)]; #
  plote2[["data"]][["name"]]=factor(plote2[["data"]][["name"]], levels = ordera)} else if
  (Group=='All' & first==FALSE) {plote2[["data"]][["name"]]=factor(plote2[["data"]][["name"]], levels = ordera)} else if
  (Group=='Female') {plote2[["data"]][["name"]]=factor(plote2[["data"]][["name"]], levels = ordera)} else if
  (Group=='Male') {plote2[["data"]][["name"]]=factor(plote2[["data"]][["name"]], levels = ordera)}
  #https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
  plote2$layers[[1]]$aes_params$odd <- "#00000000" #https://stackoverflow.com/questions/71745719/how-to-control-stripe-transparency-using-ggforestplot-geom-stripes
  
  ResComb$Group2=ResComb$Group
  ResComb <- transform(ResComb,Group2 = as.numeric(as.factor(Group2)))
  ResComb$facet_fill_color <- c("red", "green", "blue", "yellow", "brown")[ResComb$Group2]
  
  # Create plot with custom themes
  PlotVar <- plote2 + theme(axis.text.y = element_blank()) + theme_classic2()
  PlotVar2 <- PlotVar + geom_point(aes(colour = factor(Significance1)), colour = ResComb$Color1) +
    scale_color_manual(values = hp) + theme(legend.position = "none") + theme(strip.text.y = element_text(size = -Inf))
  
  # Customize facet strip colors
  g <- ggplot_gtable(ggplot_build(PlotVar2))
  stripr <- which(grepl('strip-l', g$layout$name))
  fills <- c("red", "green", "blue", "yellow", "brown")
  for (i in seq_along(stripr)) {
    j <- which(grepl('rect', g$grobs[[stripr[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[stripr[i]]]$grobs[[1]]$children[[j]]$gp$fill <- fills[i]}
  # grid::grid.draw(g)
  
  # Save plot as JPEG and convert to PDF and SVG
  setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")
  jpeg(paste(name, "divi.jpg"), width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
  print(grid::grid.draw(g))
  dev.off()
  
  daiR::image_to_pdf(paste(name, "divi.jpg"), pdf_name = paste0(paste(name, "divi.jpg"), '.pdf'))
  my_image <- image_read(paste(name, "divi.jpg"))
  my_svg <- image_convert(my_image, format = "svg")
  image_write(my_svg, paste(name, "divi.svg"))
  
  return(ordera) #If you do not want to have 'null' to the Rmarkdown/html take this away
} 


# This is with first(!!). Use it. 
OutcomeVariables='Steatosis.Grade.0.To.3';Out='Steatosis'; oute='Steatosis';first=TRUE; e='P4';ordera=c();
Group='All';name1=paste("Forest plot of",Group, "Steroid Ratios in",Out);
hel=CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name1,ordera,oute,first,e,xlim)
# #Afterwards:
first=FALSE;
Group='Female';name2=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name2,ordera=hel,oute,first,e,xlim)
Group='Male'; name3=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name3,ordera=hel,oute,first,e,xlim)
# 
OutcomeVariables='Fibrosis.Stage.0.to.4'; Out='Fibrosis';oute='Fibrosis';
Group='All'; name4=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name4,ordera=hel,oute,first,e,xlim)
Group='Female';name5=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name5,ordera=hel,oute,first,e,xlim)
Group='Male'; name6=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name6,ordera=hel,oute,first,e,xlim)
# 
OutcomeVariables='Necroinflammation'; Out='Necroinflammation';oute='Necroinflammation';
Group='All'; name7=paste("Forest plot of",Group, "Steroid Ratios in",Out); 
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name7,ordera=hel,oute,first,e,xlim) #not the very first though...
Group='Female';name8=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name8,ordera=hel,oute,first,e,xlim)
Group='Male'; name9=paste("Forest plot of",Group, "Steroid Ratios in",Out); 
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name9,ordera=hel,oute,first,e,xlim)
# 
OutcomeVariables='HOMA.IR';Out='HOMA-IR';oute='HOMAIR';
Group='All';name10=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name10,ordera=hel,oute,first,e,xlim) #not the very first though...
Group='Female';name11=paste("Forest plot of",Group, "Steroid Ratios in",Out); 
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name11,ordera=hel,oute,first,e,xlim)
Group='Male'; name12=paste("Forest plot of",Group, "Steroid Ratios in",Out); 
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name12,ordera=hel,oute,first,e,xlim)
# Fyi: I was able to revise some of the above codes with Copilot...




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
                            lm_path = "lms_tikka1943.xlx",
                            lm_sheet = NULL,
                            ci_level = 0.90,
                            alpha = 0.10,
                            analyte_col_candidates = c("Analyte","analyte","index","study","name","Abbreviation","Metabolite","Steroid","Compound"),
                            estimate_col_candidates = c("estimate","Estimate","beta","coef","Coefficient","logFC"),
                            se_col_candidates = c("se","SE","Std.Error","std.error","Std_error","stdError"),
                            pval_col_candidates = c("p","pvalue","p.value","Pr(>|t|)")) {
  # ----------------------- Safety checks -----------------------
  stopifnot(is.data.frame(groups2))
  needed_g2 <- c("Group", "Abbreviation", "Abbreviation_old", "Name")
  if (!all(needed_g2 %in% colnames(groups2))) {
    stop("groups2 must contain columns: ", paste(needed_g2, collapse = ", "))
  }
  if (!file.exists(lm_path)) {
    stop("LM results file not found: ", lm_path, 
         "\nTip: ensure the extension is correct (.xls/.xlsx). If your file is '.xlx', consider renaming to '.xls'.")
  }
  
  # ----------------------- Helpers -----------------------
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])", "", x, perl = TRUE) # drop leading X before digits
    gsub("[^A-Z0-9]", "", x)                     # strip non-alphanumerics
  }
  pick_col <- function(df, candidates, required = TRUE, what = "column") {
    hit <- candidates[candidates %in% colnames(df)]
    if (length(hit) > 0) return(hit[1])
    if (required) stop("Could not find ", what, " among: ", paste(candidates, collapse = ", "),
                       "\nAvailable columns: ", paste(colnames(df), collapse = ", "))
    return(NA_character_)
  }
  
  # ----------------------- Read LM results -----------------------
  lm_df <- readxl::read_excel(lm_path, sheet = lm_sheet)
  # Try to guess essential columns
  analyte_col <- pick_col(lm_df, analyte_col_candidates, required = TRUE, what = "analyte column")
  estimate_col <- pick_col(lm_df, estimate_col_candidates, required = TRUE, what = "estimate column")
  se_col <- pick_col(lm_df, se_col_candidates, required = TRUE, what = "SE column")
  pval_col <- pick_col(lm_df, pval_col_candidates, required = FALSE, what = "p-value column")
  
  # Optional: warn if Group argument is set to 'Male'/'Female' but no sex column in LM file
  lm_has_sex <- any(colnames(lm_df) %in% c("SEX.1F.2M","Sex","SEX","Gender"))
  if ((Group %in% c("Male","Female")) && !lm_has_sex) {
    message("Note: 'Group'='", Group, "' was provided, but LM file has no sex/SEX column; using all rows of LM results.")
  } else if (lm_has_sex) {
    # If you want sex-specific filtering here, tell me the exact column and codes; 
    # leaving as-is to avoid accidental misuse of a 'Group' column that might mean steroid group.
  }
  
  # Keep only required columns, coerce numerics
  LM <- lm_df[, unique(na.omit(c(analyte_col, estimate_col, se_col, pval_col))), drop = FALSE]
  colnames(LM)[match(c(analyte_col, estimate_col, se_col, pval_col), colnames(LM), nomatch = 0)] <-
    c("Analyte", "Estimate", "SE", if (!is.na(pval_col)) "PValue" else NULL)
  
  # Coerce to numeric
  LM$Estimate <- suppressWarnings(as.numeric(LM$Estimate))
  LM$SE <- suppressWarnings(as.numeric(LM$SE))
  if ("PValue" %in% colnames(LM)) LM$PValue <- suppressWarnings(as.numeric(LM$PValue))
  
  # Drop rows without estimate or SE
  LM <- LM[is.finite(LM$Estimate) & is.finite(LM$SE), , drop = FALSE]
  if (nrow(LM) == 0) stop("LM file has no rows with finite Estimate and SE.")
  
  # Compute p-value if missing
  if (!"PValue" %in% colnames(LM) || all(is.na(LM$PValue))) {
    z <- LM$Estimate / LM$SE
    LM$PValue <- 2 * pnorm(-abs(z))
  }
  
  # ----------------------- Prepare ResComb from LM -----------------------
  # Pretty display name (match your earlier cleanup)
  name_display <- LM$Analyte
  name_display <- gsub("\\.", "-", name_display)
  name_display <- sub("^X11", "11", name_display)
  name_display <- sub("^X17", "17", name_display)
  name_display[name_display == "T-Epi-T"] <- "T/Epi-T"
  
  ResComb <- data.frame(
    name      = name_display,
    name_raw  = LM$Analyte,
    result    = LM$Estimate,  # the point estimate from LM
    se_lm     = LM$SE,        # keep the LM SE (not used by forestplot's 'se' arg, see below)
    pval      = LM$PValue,
    stringsAsFactors = FALSE
  )
  
  # 90% CI by default
  z_ci <- qnorm(1 - (1 - ci_level)/2)
  ResComb$errord1 <- ResComb$result - z_ci * ResComb$se_lm
  ResComb$errord2 <- ResComb$result + z_ci * ResComb$se_lm
  
  # Flags/colors at alpha
  ResComb$pval0 <- ResComb$pval
  ResComb$pval1 <- ResComb$pval
  ResComb$Significance0 <- ifelse(ResComb$pval0 < alpha, 'Yes', 'No')
  ResComb$Color0        <- ifelse(ResComb$pval0 < alpha, 'blue', 'grey')
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, 'Yes', 'No')
  ResComb$Color1        <- ifelse(ResComb$pval1 < alpha, 'blue', 'grey')
  
  # ----------------------- Map steroid Groups from groups2 -----------------------
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
  
  # Warn on unmapped analytes
  unmapped <- sort(unique(ResComb$name[is.na(ResComb$Group)]))
  if (length(unmapped)) {
    warning("Some analytes could not be mapped to a steroid Group via groups2: ",
            paste(unmapped, collapse = ", "))
  }
  
  # Facet order: use order in groups2
  group_levels <- unique(groups2$Group)
  ResComb$Group <- factor(ResComb$Group, levels = group_levels)
  
  # ----------------------- Y-axis ordering -----------------------
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
  
  # Numeric group index for custom strip coloring (like your previous code)
  ResComb$Group2 <- as.numeric(as.factor(ResComb$Group))
  
  # ----------------------- xlim -----------------------
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb$errord1, ResComb$errord2), na.rm = TRUE)
  }
  
  # ----------------------- Forest plot -----------------------
  plote2 <- forestplot(
    df       = ResComb,
    estimate = result,
    se       = 0,                 # we provide our own CI via geom_errorbarh
    pvalue   = pval1,             # uses ResComb$pval1
    psignif  = alpha,
    xlim     = xlim,
    xlab     = paste0('LM Estimate with ', round(ci_level*100), '% CI'),
    ylab     = 'Steroid Groups',
    title    = '',
    colour   = Significance1
  ) +
    ggforce::facet_col(facets = ~Group, scales = "free_y", space = "free", strip.position = 'left') +
    geom_errorbarh(aes(xmin = errord1, xmax = errord2, height = .0, colour = Significance1))
  
  # Point color palette
  hp <- if (sum(ResComb$Significance1 == 'Yes', na.rm = TRUE) == nrow(ResComb)) {
    c('blue', 'blue')
  } else {
    c('#999999', 'blue')
  }
  
  # Make stripes transparent if applicable
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
  
  # ----------------------- Save outputs -----------------------
  # Adjust working directory to your environment if needed
  setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")
  
  jpeg(paste0(name, "divi.jpg"), width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
  print(grid::grid.draw(g))
  dev.off()
  
  # PDF and SVG
  daiR::image_to_pdf(paste0(name, "divi.jpg"), pdf_name = paste0(paste0(name, "divi.jpg"), '.pdf'))
  my_image <- magick::image_read(paste0(name, "divi.jpg"))
  my_svg   <- magick::image_convert(my_image, format = "svg")
  magick::image_write(my_svg, paste0(name, "divi.svg"))
  
  return(ordera)
}



#############



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
                            lm_condition = NULL,        # <- preferred explicit condition (e.g. "Steatosis")
                            use_qval = FALSE,           # <- if TRUE, use Qval for significance; else Pval
                            ci_level = 0.90,
                            alpha = 0.10) {
  
  # ----------------------- Safety checks -----------------------
  stopifnot(is.data.frame(groups2))
  needed_g2 <- c("Group", "Abbreviation", "Abbreviation_old", "Name")
  if (!all(needed_g2 %in% colnames(groups2))) {
    stop("groups2 must contain columns: ", paste(needed_g2, collapse = ", "))
  }
  if (!file.exists(lm_path)) {
    stop("LM results file not found: ", lm_path,
         "\nTip: ensure it is .xls/.xlsx ('.xlx' is unusual; renaming to .xls often helps).")
  }
  
  # ----------------------- Helpers -----------------------
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])", "", x, perl = TRUE)   # drop leading X before digits
    gsub("[^A-Z0-9]", "", x)                       # strip non-alphanumerics
  }
  clean_col <- function(x) {
    x <- gsub("[\n\r]+", " ", x)
    x <- gsub('["]+', "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  parse_num <- function(x) {
    # robust numeric: replaces decimal commas, trims spaces
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x <- gsub("\\s+", "", x)
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  }
  
  # ----------------------- Read & tidy LM Excel (wide → long) -----------------------
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  if (nrow(lm_raw) == 0) stop("LM file is empty.")
  
  # Clean column names (keep originals to detect duplicated blocks with suffixes like ...1, ...2)
  cn_orig <- colnames(lm_raw)
  cn_base <- clean_col(sub("\\.{3}[0-9]+$", "", cn_orig))
  cn_suf  <- sub(".*(\\.{3}[0-9]+)$", "\\1", cn_orig)
  cn_suf[!grepl("\\.{3}[0-9]+$", cn_orig)] <- "...0"
  
  required_bases <- c("Steroid", "Gender in condition", "Condition", "Coef", "Stderr", "Pval", "Qval", "N", "N not zero")
  # The excel shows "Gender in \ncondition" and "N not\nzero" — normalize both
  cn_base <- gsub("Gender in\\s+condition", "Gender in condition", cn_base, ignore.case = TRUE)
  cn_base <- gsub("N not\\s*zero", "N not zero", cn_base, ignore.case = TRUE)
  
  # locate all suffix blocks that contain a 'Steroid' base column
  steroid_cols_idx <- which(tolower(cn_base) == "steroid")
  suf_blocks <- unique(cn_suf[steroid_cols_idx])
  
  make_block_df <- function(suf) {
    # for each base, find column with same suffix
    pick <- function(base) {
      idx <- which(cn_base == base & cn_suf == suf)
      if (length(idx)) cn_orig[idx[1]] else NA_character_
    }
    cols <- vapply(required_bases, pick, "", USE.NAMES = TRUE)
    if (any(is.na(cols))) return(NULL)
    block <- lm_raw[, cols, drop = FALSE]
    colnames(block) <- required_bases
    block
  }
  
  blocks <- lapply(suf_blocks, make_block_df)
  blocks <- blocks[!vapply(blocks, is.null, logical(1))]
  if (length(blocks) == 0) stop("Could not detect any complete LM blocks (Steroid/Condition/Coef/Stderr/...).")
  
  LM_long <- do.call(rbind, blocks)
  
  # Coerce numerics (handle decimal commas)
  LM_long$Coef   <- parse_num(LM_long$Coef)
  LM_long$Stderr <- parse_num(LM_long$Stderr)
  LM_long$Pval   <- parse_num(LM_long$Pval)
  LM_long$Qval   <- parse_num(LM_long$Qval)
  LM_long$N      <- suppressWarnings(as.integer(parse_num(LM_long$N)))
  LM_long$`N not zero` <- suppressWarnings(as.integer(parse_num(LM_long$`N not zero`)))
  
  # Clean “Gender in condition” and Condition
  LM_long$`Gender in condition` <- clean_col(LM_long$`Gender in condition`)
  LM_long$Condition             <- clean_col(LM_long$Condition)
  LM_long$Steroid               <- clean_col(LM_long$Steroid)
  
  # Drop rows missing estimate/SE
  LM_long <- LM_long[is.finite(LM_long$Coef) & is.finite(LM_long$Stderr), , drop = FALSE]
  if (nrow(LM_long) == 0) stop("No rows with finite Coef and Stderr after parsing the LM file.")
  
  # ----------------------- Determine Condition & Gender from args/name -----------------------
  # Prefer explicit lm_condition; else try to infer from `name`
  infer_condition <- function(s) {
    if (is.null(s)) return(NA_character_)
    s <- tolower(as.character(s))
    if (grepl("steatosis", s)) return("Steatosis")
    if (grepl("fibrosis", s)) return("Fibrosis")
    if (grepl("necroinflammation|necro-inflammation|necro inflammation", s)) return("Necroinflammation")
    NA_character_
  }
  
  if (is.null(lm_condition)) {
    lm_condition <- infer_condition(name)
    if (is.na(lm_condition)) {
      # fallback: if only one condition exists, use it; else stop
      conds <- sort(unique(LM_long$Condition))
      if (length(conds) == 1) {
        lm_condition <- conds[1]
      } else {
        stop("Please set lm_condition explicitly (e.g., 'Steatosis'). Available: ",
             paste(conds, collapse = ", "))
      }
    }
  }
  
  # Gender filter based on Group argument
  # The file uses "Gender in condition" like "Steatosis_both", "Steatosis_male", "Steatosis_female" (by your Fibrosis example).
  target_gender_token <- switch(tolower(Group),
                                "all"    = "both",
                                "male"   = "male",
                                "female" = "female",
                                "both") # default
  
  # Filter rows by Condition and gender token
  rows_cond <- tolower(LM_long$Condition) == tolower(lm_condition)
  rows_gender <- grepl(target_gender_token, tolower(LM_long$`Gender in condition`))
  LM_sel <- LM_long[rows_cond & rows_gender, , drop = FALSE]
  
  if (nrow(LM_sel) == 0) {
    # If nothing found, try condition-only
    LM_sel <- LM_long[rows_cond, , drop = FALSE]
    if (nrow(LM_sel) == 0) {
      stop("No LM rows found for Condition='", lm_condition, "' (and Group='", Group, "').")
    } else {
      warning("No rows matched Gender token '", target_gender_token,
              "' for Condition='", lm_condition, "'. Using all genders for this condition.")
    }
  }
  
  # Deduplicate if the wide file yields duplicates
  LM_sel <- unique(LM_sel)
  
  # ----------------------- Build ResComb from LM (Coef/SE) -----------------------
  # Pretty display name (match your prior normalizations)
  name_display <- LM_sel$Steroid
  name_display <- gsub("\\.", "-", name_display)
  name_display <- sub("^X11", "11", name_display)
  name_display <- sub("^X17", "17", name_display)
  name_display[name_display == "T-Epi-T"] <- "T/Epi-T"
  
  # p-value to use
  p_used <- if (isTRUE(use_qval) && any(is.finite(LM_sel$Qval))) LM_sel$Qval else LM_sel$Pval
  
  ResComb <- data.frame(
    name      = name_display,
    name_raw  = LM_sel$Steroid,
    result    = LM_sel$Coef,     # LM point estimate
    se_lm     = LM_sel$Stderr,   # LM SE
    pval      = p_used,
    stringsAsFactors = FALSE
  )
  
  # CI
  z_ci <- qnorm(1 - (1 - ci_level)/2)
  ResComb$errord1 <- ResComb$result - z_ci * ResComb$se_lm
  ResComb$errord2 <- ResComb$result + z_ci * ResComb$se_lm
  
  # Flags/colors at alpha
  ResComb$pval0 <- ResComb$pval
  ResComb$pval1 <- ResComb$pval
  ResComb$Significance0 <- ifelse(ResComb$pval0 < alpha, 'Yes', 'No')
  ResComb$Color0        <- ifelse(ResComb$pval0 < alpha, 'blue', 'grey')
  ResComb$Significance1 <- ifelse(ResComb$pval1 < alpha, 'Yes', 'No')
  ResComb$Color1        <- ifelse(ResComb$pval1 < alpha, 'blue', 'grey')
  
  # ----------------------- Map steroid Groups from groups2 -----------------------
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
  
  # Warn on unmapped analytes
  unmapped <- sort(unique(ResComb$name[is.na(ResComb$Group)]))
  if (length(unmapped)) {
    warning("Unmapped analytes (not found in groups2): ", paste(unmapped, collapse = ", "))
  }
  
  # Facet order: from groups2 order
  group_levels <- unique(groups2$Group)
  ResComb$Group <- factor(ResComb$Group, levels = group_levels)
  
  # ----------------------- Y-axis ordering -----------------------
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
  
  # Numeric group index for custom strip coloring
  ResComb$Group2 <- as.numeric(as.factor(ResComb$Group))
  
  # ----------------------- xlim -----------------------
  if (is.null(xlim) || length(xlim) != 2 || any(!is.finite(xlim))) {
    xlim <- range(c(ResComb$errord1, ResComb$errord2), na.rm = TRUE)
  }
  
  # ----------------------- Forest plot -----------------------
  plote2 <- forestplot(
    df       = ResComb,
    estimate = result,
    se       = 0,  # CI drawn via geom_errorbarh below
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
  
  # ----------------------- Save outputs -----------------------
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

name1 <- "Forest plot of All Steroid Ratios in Steatosis"
ord <- CalculateErrors(
  NonAlcoholicFattyLiverDisease = NAFLD_df,   # kept for signature compat (not used)
  OutcomeVariables = "Outcome",               # ignored
  Group = "All",                              # -> matches *_both
  name = name1,
  ordera = NULL,
  oute = NULL,
  first = TRUE,
  e = NULL,
  xlim = NULL,
  groups2 = groups2_df,
  lm_path = "lms_tikka19324_v2.xlsx",             # or .xls
  lm_sheet = NULL,
  lm_condition = NULL,                        # auto-detect "Steatosis" from name
  use_qval = FALSE,
  ci_level = 0.90,
  alpha = 0.10
)

ord <- CalculateErrors(
  NonAlcoholicFattyLiverDisease = NAFLD_df,
  OutcomeVariables = "Outcome",
  Group = "Female",                           # -> matches *_female
  name = "Forest plot of Female Steroid Coefs in Fibrosis",
  groups2 = groups2,
  lm_path = "lms_tikka19324_v2.xlsx",
  lm_condition = "Fibrosis",
  use_qval = TRUE,                            # use Qval for coloring
  ci_level = 0.95,                            # 95% CI
  alpha = 0.05
)

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
  if (!file.exists(lm_path)) {
    stop("LM results file not found: ", lm_path)
  }
  
  ## ------------------------ Helpers ------------------------
  norm_key <- function(x) {
    x <- toupper(as.character(x))
    x <- gsub("^X(?=[0-9])","",x,perl=TRUE)  # drop leading X before digit
    gsub("[^A-Z0-9]","",x)
  }
  clean_col <- function(x) {
    x <- gsub("[\r\n]+"," ", x)
    x <- gsub('"+', "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
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
  # Keep original headers exactly; duplicates are fine for our position-based parsing
  lm_raw <- readxl::read_excel(lm_path, sheet = lm_sheet, .name_repair = "minimal")
  if (nrow(lm_raw) == 0) stop("LM file is empty.")
  
  cn_orig <- colnames(lm_raw)
  # If readxl added suffixes like "...1", strip to a base; otherwise it's just the original
  cn_base <- clean_col(sub("\\.{3}[0-9]+$", "", cn_orig))
  # Fix special headers with newlines
  cn_base <- gsub("Gender in\\s+condition", "Gender in condition", cn_base, ignore.case = TRUE)
  cn_base <- gsub("N not\\s*zero", "N not zero", cn_base, ignore.case = TRUE)
  
  # Find all starting positions of a block by locating every "Steroid" header
  steroid_pos <- which(tolower(cn_base) == "steroid")
  if (length(steroid_pos) == 0) {
    stop("No 'Steroid' header detected in the LM file. Headers seen: ",
         paste(unique(cn_base), collapse = " | "))
  }
  
  # Define required headers we expect inside each block
  required_bases <- c("Steroid","Gender in condition","Condition","Coef","Stderr","Pval","Qval","N","N not zero")
  
  # For each block segment, collect the first occurrence of each required header
  blocks <- list()
  for (i in seq_along(steroid_pos)) {
    start_i <- steroid_pos[i]
    end_i   <- if (i < length(steroid_pos)) steroid_pos[i+1] - 1 else length(cn_base)
    slice_idx <- start_i:end_i
    slice_base <- cn_base[slice_idx]
    
    pick_idx <- function(base_name) {
      hit <- which(tolower(slice_base) == tolower(base_name))
      if (length(hit)) slice_idx[hit[1]] else NA_integer_
    }
    col_idx <- vapply(required_bases, pick_idx, integer(1L))
    if (any(is.na(col_idx))) next  # incomplete block; skip
    
    block_df <- lm_raw[, col_idx, drop = FALSE]
    colnames(block_df) <- required_bases
    blocks[[length(blocks) + 1]] <- block_df
  }
  
  if (length(blocks) == 0) {
    stop("Could not detect any complete LM blocks (Steroid/Condition/Coef/Stderr/...).\n",
         "Headers (normalized) detected:\n  ",
         paste(unique(cn_base), collapse = " | "), "\n",
         "Tip: ensure the LM sheet has repeating columns named exactly like:\n  ",
         paste(required_bases, collapse = " | "))
  }
  
  LM_long <- do.call(rbind, blocks)
  
  ## ------------------------ Coerce numeric & clean values ------------------------
  LM_long$Coef        <- parse_num(LM_long$Coef)
  LM_long$Stderr      <- parse_num(LM_long$Stderr)
  LM_long$Pval        <- parse_num(LM_long$Pval)
  LM_long$Qval        <- parse_num(LM_long$Qval)
  LM_long$N           <- suppressWarnings(as.integer(parse_num(LM_long$N)))
  LM_long$`N not zero`<- suppressWarnings(as.integer(parse_num(LM_long$`N not zero`)))
  
  LM_long$`Gender in condition` <- clean_col(LM_long$`Gender in condition`)
  LM_long$Condition             <- clean_col(LM_long$Condition)
  LM_long$Steroid               <- clean_col(LM_long$Steroid)
  
  # Keep rows with finite Coef and Stderr
  LM_long <- LM_long[is.finite(LM_long$Coef) & is.finite(LM_long$Stderr), , drop = FALSE]
  if (nrow(LM_long) == 0) stop("No rows with finite Coef and Stderr after parsing the LM file.")
  
  ## ------------------------ Choose Condition & Gender ------------------------
  if (is.null(lm_condition)) {
    lm_condition <- infer_condition(name)
    if (is.na(lm_condition)) {
      conds <- sort(unique(LM_long$Condition))
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
  
  rows_cond   <- tolower(LM_long$Condition) == tolower(lm_condition)
  rows_gender <- grepl(target_gender_token, tolower(LM_long$`Gender in condition`))
  LM_sel <- LM_long[rows_cond & rows_gender, , drop = FALSE]
  if (nrow(LM_sel) == 0) {
    # fallback to condition-only if gender-token doesn’t exist
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
  # Pretty display name to match your plots
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
  
  z_ci <- qnorm(1 - (1 - ci_level)/2)
  ResComb$errord1 <- ResComb$result - z_ci * ResComb$se_lm
  ResComb$errord2 <- ResComb$result + z_ci * ResComb$se_lm
  
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
  # Adjust or remove setwd() if needed
  setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")
  
  jpeg(paste0(name, "divi.jpg"), width = 7500, height = 11000, quality = 100, pointsize = 16, res = 1000)
  print(grid::grid.draw(g))
  dev.off()
  
  # PDF and SVG
  if (requireNamespace("daiR", quietly = TRUE)) {
    daiR::image_to_pdf(paste0(name, "divi.jpg"), pdf_name = paste0(paste0(name, "divi.jpg"), '.pdf'))
  }
  my_image <- magick::image_read(paste0(name, "divi.jpg"))
  my_svg   <- magick::image_convert(my_image, format = "svg")
  magick::image_write(my_svg, paste0(name, "divi.svg"))
  
  return(ordera)
}

library(readxl)
lm_raw <- readxl::read_excel("lms_tikka19324_v2.xlsx", sheet = NULL, .name_repair = "minimal")
cn <- colnames(lm_raw)

clean_col <- function(x) {
  x <- gsub("[\r\n]+"," ", x)
  x <- gsub('"+', "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
cn_base <- clean_col(sub("\\.{3}[0-9]+$", "", cn))
cn_base <- gsub("Gender in\\s+condition", "Gender in condition", cn_base, ignore.case = TRUE)
cn_base <- gsub("N not\\s*zero", "N not zero", cn_base, ignore.case = TRUE)

which(tolower(cn_base) == "steroid")      # positions where each block starts
unique(cn_base)                           # normalized header values


groups2_df <- groups2 #readxl::read_excel("path/to/groups2.xlsx")
# Check required columns
stopifnot(all(c("Group","Abbreviation","Abbreviation_old","Name") %in% colnames(groups2_df)))

name1 <- "Forest plot of All Steroid Ratios in Steatosis"

ord <- CalculateErrors(
  NonAlcoholicFattyLiverDisease = NAFLD_df,   # kept for compatibility, not used
  OutcomeVariables = "Outcome",               # ignored
  Group = "All",                              # -> matches *_both in LM file
  name = name1,
  ordera = NULL,
  oute = NULL,
  first = TRUE,
  e = NULL,
  xlim = NULL,
  groups2 = groups2_df,                       # your mapping
  lm_path = "lms_tikka19324_v2.xlsx",         # LM file
  lm_sheet = NULL,                            # first sheet by default
  lm_condition = NULL,                        # auto-detects "Steatosis" from name
  use_qval = FALSE,                           # use Pval for significance
  ci_level = 0.90,                            # 90% CI
  alpha = 0.10                                # significance threshold
)





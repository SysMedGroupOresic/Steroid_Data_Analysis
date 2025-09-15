I need to have groups from the grouping of steroids from: 'groups2' file (Group column:           Group      Abbreviation Abbreviation_old                            Name
1     Androgens   11b-OH-Androst.         11b-OHA4          11b-OH-Androstenedione
4     Androgens     11-Ketodih.t.          11-KDHT      11-Ketodihydrotestosterone
5     Androgens      11-Ketotest.            11-KT             11-Ketotestosterone
8     Androgens   11-Ketoandrost.           11-KA4          11-Ketoandrostenedione
10    Androgens          Androst.               A4                 Androstenedione
11    Androgens             Andr.               AN                    Androsterone
15    Androgens   Dehydroepiandr.             DHEA          Dehydroepiandrosterone
16    Androgens      Dihydrotest.              DHT             Dihydrotestosterone
21    Androgens Test. / Epi-test.          T/Epi-T Testosterone / Epi-testosterone
17    Estrogens         Estradiol               E2                       Estradiol
18    Estrogens           Estrone               E1                         Estrone
3       Glucoc.  11-Deoxycortisol                S                11-Deoxycortisol
14      Glucoc.         Cortisone                E                       Cortisone
9     Mineralc.              Ald.                A                     Aldosterone
12    Mineralc.           Cortic.                B                  Corticosterone
2     Mineralc.   11-Deoxycortic.              DOC          11-Deoxycorticosterone
6  Progestogens   17a-OH-Pregnen.         17a-OHP5             17a-OH-Pregnenolone
7  Progestogens   17a-OH-Progest.         17a-OHP4             17a-OH-Progesterone
19 Progestogens          Pregnen.               P5                    Pregnenolone
20 Progestogens          Progest.               P4                    Progesterone), instead of: '# Define the NonAlcoholicFattyLiverDisease dataset by selecting the first 28 columns from CombinedData
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

  return(ordera) #If you do not want to have 'null' to the Rmarkdown/html take this away
  } 


# This is with first(!!). Use it. 
OutcomeVariables='Steatosis.Grade.0.To.3';Out='Steatosis'; oute='Steatosis';first=TRUE; e='P4';ordera=c();
Group='All';name1=paste("Forest plot of",Group, "Steroid Ratios in",Out);
hel=CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name1,ordera,oute,first,e,xlim)
# #Afterwards:
first=FALSE;
Group='Female';name2=paste("Forest plot of",Group, "Steroid Ratios in",Out);
CalculateErrors(NonAlcoholicFattyLiverDisease,OutcomeVariables,Group,name2,ordera=hel,oute,first,e,xlim)' 
# Load necessary libraries
library(Hmisc)
library(dplyr)
library(circlize)
library(igraph)
library(ComplexHeatmap)
library(grid)
library(knitr)
library(daiR)
library(magick)

# First the correlations for the chord diagrams (both male and female as well as total subjects)
tv_c <- tvauxe

# Match column names with groups2 and update column names
hupo <- match(colnames(tv_c)[colnames(tv_c) %in% groups2[, 3]], groups2[, 3])
colnames(tv_c)[colnames(tv_c) %in% groups2[, 3]] <- groups2[hupo, 2]
ok <- colnames(tv_c)

# Convert to data frame and remove specific columns
tv_c <- data.frame(tv_c)
tv_c <- tv_c[, !colnames(tv_c) %in% c('Total_TG', 'PFAS', "Perfluorodecyl.ethanoic.acid")]

# Split data by gender
tvf <- tv_c[tv_c[, 'Gender'] == min(tv_c[, 'Gender']), ]
tvm <- tv_c[tv_c[, 'Gender'] == max(tv_c[, 'Gender']), ]

# Clean column names
clean_colnames <- function(df) {
  colnames(df) <- gsub("\\.", "-", colnames(df))
  colnames(df) <- gsub("X11", "11", colnames(df))
  colnames(df) <- gsub("X17", "17", colnames(df))
  colnames(df)[colnames(df) == "T-Epi-T"] <- "T/Epi-T"
  colnames(df)[colnames(df) == "Steatosis-Grade"] <- "Steatosis Grade"
  colnames(df)[colnames(df) == "Fibrosis-Stage"] <- "Fibrosis Stage"
  colnames(df)[colnames(df) == "17aOH-P4"] <- "17a-OHP4"
  colnames(df)[colnames(df) == "HOMA IR"] <- "HOMA-IR"
  return(df)
}

tv_c <- clean_colnames(tv_c)
tvf <- clean_colnames(tvf)
tvm <- clean_colnames(tvm)

# Update x4 column names
x4[x4 == "X7.oxo.DCA_S"] <- "X7-oxo-DCA_S"

# Function to calculate correlation matrix
calculate_corr <- function(dat, ok) {
  dat <- dat %>% select(-c('PatientNumber', 'Gender'))
  result <- (rcorr(as.matrix(dat), type = 'spearman'))$r
  colnames(result) <- ok[3:(ncol(result) + 2)]
  rownames(result) <- ok[3:(ncol(result) + 2)]
  return(result)
}

# Calculate correlation matrices
resulta <- calculate_corr(tv_c, ok)
resultaf <- calculate_corr(tvf, ok)
resultam <- calculate_corr(tvm, ok)

# Check the columns away
at <- colnames(resulta)[1:(length(x1) - 1)] # clinicals
bt <- colnames(resulta)[(length(at) + 1):(length(at) + length(x2))] # Steroids
ct <- colnames(resulta)[(length(at) + length(bt) + 1):(length(at) + length(bt) + length(x3))] # BA_l
dt <- colnames(resulta)[(length(at) + length(bt) + length(ct) + 1):(length(at) + length(bt) + length(ct) + length(x4))] # BA_s
et <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(x5))] # PFAS
ft <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + length(et) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(et) + length(x6))] #
atl <- length(at)
btl <- length(bt)
ctl <- length(ct)
dtl <- length(dt)
etl <- length(et)
ftl <- length(ft)

# Set significance level
n_level <- 0.01

# Function to filter significant correlations
filter_significant <- function(result) {
  Nrr <- qpNrr(result, verbose = FALSE)
  Nrr[is.na(Nrr)] <- 1
  cond <- data.frame(as.matrix(Nrr < n_level))
  RN <- data.frame(result)
  tes_t <- cond * RN
  tes_t <- as.matrix(tes_t)
  return(tes_t)
}

resulta <- filter_significant(resulta)
resultaf <- filter_significant(resultaf)
resultam <- filter_significant(resultam)

# Update column and row names for results
update_names <- function(result, ok) {
  colnames(result)[colnames(result) == 'Gender'] <- 'Sex(F-M+)'
  rownames(result)[rownames(result) == 'Gender'] <- 'Sex(F-M+)'
  # colnames(result)[2:dim(result)[2]] <- ok[3:dim(result)[2]]
  # rownames(result)[2:dim(result)[2]] <- ok[3:dim(result)[2]]
  return(result)
}

resulta <- update_names(resulta, ok)
resultaf <- update_names(resultaf, ok)
resultam <- update_names(resultam, ok)



# Function to create chord diagrams
group_chords <- function(vars, n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f) {
  classes <- 5
  tot <- rownames(resulta)[2:dim(resulta)[1]]
  range <- 1:(a + b + c + e + f)
  layout(matrix(1:1, 1, 1))
  title <- 'Sex'
  genders <- gend
  windowsFonts(A = windowsFont("Calibri (Body)"))
  i <- 1
  tes_t <- vars
  if (gend == 'All') {
    colnames(tes_t) <- rownames(resulta)
    rownames(tes_t) <- rownames(resulta)
  } else {
    colnames(tes_t) <- rownames(resultaf)
    rownames(tes_t) <- rownames(resultaf)
  }
  
  g1 <- c(rep('Clinical', a), rep('Steroids', b), rep('BA_liver', c), rep('Contaminants', e), rep('Lipids', f))
  
  # Removing self-correlation
  tes_t[1:a, 1:a] <- 0
  tes_t[(a + 1):(a + b), (a + 1):(a + b)] <- 0
  tes_t[(a + b + 1):(a + b + c), (a + b + 1):(a + b + c)] <- 0
  tes_t[(a + b + c + 1):(a + b + c + e), (a + b + c + 1):(a + b + c + e)] <- 0
  tes_t[(a + b + c + e + 1):(a + b + c + e + f), (a + b + c + e + 1):(a + b + c + e + f)] <- 0
  
  group <- structure(g1, names = colnames(tes_t))
  grid.col <- structure(c(rep('#93c29f', a), rep('#a83277', b), rep('red', c), rep('grey', e), rep('black', f)),
                        names = rownames(tes_t))
  tes_t <- tes_t[range, range]
  grid.col <- grid.col[range]
  g <- graph.adjacency(tes_t, mode = "upper", weighted = TRUE, diag = FALSE)
  e <- get.edgelist(g)
  df <- as.data.frame(cbind(e, E(g)$weight))
  df[, 3] <- as.numeric(df[, 3])
  
  rango <- function(x) { ((x - min(x)) / (max(x) - min(x))) * 2 }
  col_fun <- colorRamp2(c(-1, 0, 1), c("blue", 'white', "orange"), transparency = 0.25)
  df <- df[!df$V1 %in% rem, ]
  df <- df[!df$V2 %in% rem, ]
  
  classes <- modi
  namesh <- unique(g1)
  cola <- unique(grid.col)
  
  lgd_group <- Legend(at = gend, type = "points", legend_gp = gpar(col = colors), title_position = "topleft", title = title)
  lgd_points <- Legend(at = namesh, type = "points", legend_gp = gpar(col = cola), title_position = "topleft", title = "Class")
  lgd_lines <- Legend(at = c("Positive", "Negative"), type = "points", legend_gp = gpar(col = c('orange', 'blue')), title_position = "topleft", title = "Correlation")
  lgd_edges <- Legend(at = c(-1, 1), col_fun = col_fun, title_position = "topleft", title = "Edges")
  lgd_list_vertical <- packLegend(lgd_group, lgd_points, lgd_lines, lgd_edges)
  circos.par(gap.after = 1.5, start.degree = 90)
  chordDiagram(df, annotationTrack = c("grid"), grid.col = grid.col, directional = FALSE, symmetric = TRUE, scale = FALSE,
               link.lwd = 0.3, link.border = "white", order = rownames(tes_t), preAllocateTracks = 1, col = col_fun, transparency = 0.25, big.gap = 10, small.gap = 1)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    circos.axis(h = "top", labels.cex = 0.000001, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
  windowsFonts(A = windowsFont("Calibri (Body)"))
  draw(lgd_list_vertical, x = unit(5, "mm"), y = unit(5, "mm"), just = c("left", "bottom"))
  
  dev.copy(jpeg, paste0(gend, n_level, 'hiee.jpg'), width = 12, height = 12, units = "in", res = 1000)
  dev.off()
  
  knitr::include_graphics(paste0(gend, n_level, 'hiee.jpg'))
  daiR::image_to_pdf(paste0(gend, n_level, 'hiee.jpg'), pdf_name = paste0(paste0(gend, n_level, 'hie'), '.pdf'))
  my_image <- image_read(paste0(gend, n_level, 'hiee.jpg'))
  my_svg <- image_convert(my_image, format = "svg")
  image_write(my_svg, paste(paste0(gend, n_level, 'hie.jpg'), ".svg"))
}

# All variables
n_level <- 0.01
circos.clear()
vars <- list(resulta)
big <- 'Yes'
title <- 'All Variables'
rem <- x4
modi <- 5
colt <- 'black'
a <- length(x1) - 1
b <- length(x2)
c <- length(x3)
d <- length(x4)
e <- length(x5)
f <- length(x6)
gend <- c('All')
colors <- c('blue')
group_chords(vars[[1]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)

# Genderwise
vars <- list(resultaf, resultam)
big <- 'No'
title <- 'Genders Separated'
rem <- x4
modi <- 4
colt <- 'black'
colors <- c('white', 'black')
a <- length(x1) - 2
b <- length(x2)
c <- length(x3)
d <- length(x4)
e <- length(x5)
f <- length(x6)
gend <- c('Female')
colors <- c('white')
group_chords(vars[[1]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)
gend <- c('Male')
colors <- c('black')
group_chords(vars[[2]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)







##############Ok...

# Load necessary libraries
library(Hmisc)
library(dplyr)
library(corrr)

# First the correlations for the chord diagrams (both male and female as well as total subjects)
tv_c <- tvauxe

# Match column names with groups2 and update column names
hupo <- match(colnames(tv_c)[colnames(tv_c) %in% groups2[, 3]], groups2[, 3])
colnames(tv_c)[colnames(tv_c) %in% groups2[, 3]] <- groups2[hupo, 2]
ok <- colnames(tv_c)

# Convert to data frame and remove specific columns
tv_c <- data.frame(tv_c)
tv_c <- tv_c[, !colnames(tv_c) %in% c('Total_TG', 'PFAS', "Perfluorodecyl.ethanoic.acid")]

# Remove duplicate AGE columns if they exist
tv_c <- tv_c[, !duplicated(colnames(tv_c))]

# Split data by gender
tvf <- tv_c[tv_c[, 'Gender'] == min(tv_c[, 'Gender']), ]
tvm <- tv_c[tv_c[, 'Gender'] == max(tv_c[, 'Gender']), ]

# Clean column names
clean_colnames <- function(df) {
  colnames(df) <- gsub("\\.", "-", colnames(df))
  colnames(df) <- gsub("X11", "11", colnames(df))
  colnames(df) <- gsub("X17", "17", colnames(df))
  colnames(df)[colnames(df) == "T-Epi-T"] <- "T/Epi-T"
  colnames(df)[colnames(df) == "Steatosis-Grade"] <- "Steatosis Grade"
  colnames(df)[colnames(df) == "Fibrosis-Stage"] <- "Fibrosis Stage"
  colnames(df)[colnames(df) == "17aOH-P4"] <- "17a-OHP4"
  colnames(df)[colnames(df) == "HOMA IR"] <- "HOMA-IR"
  return(df)
}

tv_c <- clean_colnames(tv_c)
tvf <- clean_colnames(tvf)
tvm <- clean_colnames(tvm)

# Update x4 column names
x4[x4 == "X7.oxo.DCA_S"] <- "X7-oxo-DCA_S"

# Function to calculate correlation matrix
calculate_corr <- function(dat, ok) {
  dat <- dat %>% select(-c('PatientNumber', 'Gender'))
  result <- (rcorr(as.matrix(dat), type = 'spearman'))$r
  colnames(result) <- ok[3:(ncol(result) + 2)]
  rownames(result) <- ok[3:(ncol(result) + 2)]
  return(result)
}

# Calculate correlation matrices
resulta <- calculate_corr(tv_c, ok)
resultaf <- calculate_corr(tvf, ok)
resultam <- calculate_corr(tvm, ok)

# Check the columns away
at <- colnames(resulta)[1:(length(x1) - 1)] # clinicals
bt <- colnames(resulta)[(length(at) + 1):(length(at) + length(x2))] # Steroids
ct <- colnames(resulta)[(length(at) + length(bt) + 1):(length(at) + length(bt) + length(x3))] # BA_l
dt <- colnames(resulta)[(length(at) + length(bt) + length(ct) + 1):(length(at) + length(bt) + length(ct) + length(x4))] # BA_s
et <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(x5))] # PFAS
ft <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + length(et) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(et) + length(x6))] #
atl <- length(at)
btl <- length(bt)
ctl <- length(ct)
dtl <- length(dt)
etl <- length(et)
ftl <- length(ft)

# Set significance level
n_level <- 0.01

# Function to filter significant correlations
filter_significant <- function(result) {
  Nrr <- qpNrr(result, verbose = FALSE)
  Nrr[is.na(Nrr)] <- 1
  cond <- data.frame(as.matrix(Nrr < n_level))
  RN <- data.frame(result)
  tes_t <- cond * RN
  tes_t <- as.matrix(tes_t)
  return(tes_t)
}

resulta <- filter_significant(resulta)
resultaf <- filter_significant(resultaf)
resultam <- filter_significant(resultam)

# Update column and row names for results
update_names <- function(result, ok) {
  colnames(result)[colnames(result) == 'Gender'] <- 'Sex(F-M+)'
  rownames(result)[rownames(result) == 'Gender'] <- 'Sex(F-M+)'
  colnames(result)[2:dim(result)[2]] <- ok[3:dim(result)[2]]
  rownames(result)[2:dim(result)[2]] <- ok[3:dim(result)[2]]
  return(result)
}

resulta <- update_names(resulta, ok)
resultaf <- update_names(resultaf, ok)
resultam <- update_names(resultam, ok)

# Remove duplicate AGE columns if they exist
resulta <- resulta[, !duplicated(colnames(resulta))]
resultaf <- resultaf[, !duplicated(colnames(resultaf))]
resultam <- resultam[, !duplicated(colnames(resultam))]

library(corrr)

# Function to create chord diagrams
group_chords <- function(vars, n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f) {
  classes <- 5
  tot <- rownames(resulta)[2:dim(resulta)[1]]
  range <- 1:(a + b + c + d + e + f)
  layout(matrix(1:1, 1, 1))
  title <- 'Sex'
  genders <- gend
  windowsFonts(A = windowsFont("Calibri (Body)"))
  i <- 1
  tes_t <- vars
  if (gend == 'All') {
    colnames(tes_t) <- rownames(resulta)
    rownames(tes_t) <- rownames(resulta)
  } else {
    colnames(tes_t) <- rownames(resultaf)
    rownames(tes_t) <- rownames(resultaf)
  }
  
  g1 <- c(rep('Clinical', a), rep('Steroids', b), rep('BA_liver', c), rep('BA_serum', d), rep('Contaminants', e), rep('Lipids', f))
  
  # Removing self-correlation using corrr package
  tes_t <- correlate(tes_t)
  tes_t <- shave(tes_t)
  tes_t <- stretch(tes_t, na.rm = TRUE)
  tes_t <- as.matrix(tes_t)
  
  group <- structure(g1, names = colnames(tes_t))
  grid.col <- structure(c(rep('#93c29f', a), rep('#a83277', b), rep('red', c), rep('blue', d), rep('grey', e), rep('black', f)),
                        names = rownames(tes_t))
  tes_t <- tes_t[range, range]
  grid.col <- grid.col[range]
  g <- graph.adjacency(tes_t, mode = "upper", weighted = TRUE, diag = FALSE)
  e <- get.edgelist(g)
  df <- as.data.frame(cbind(e, E(g)$weight))
  df[, 3] <- as.numeric(df[, 3])
  
  rango <- function(x) { ((x - min(x)) / (max(x) - min(x))) * 2 }
  col_fun <- colorRamp2(c(-1, 0, 1), c("blue", 'white', "orange"), transparency = 0.25)
  df <- df[!df$V1 %in% rem, ]
  df <- df[!df$V2 %in% rem, ]
  
  classes <- modi
  namesh <- unique(g1)
  cola <- unique(grid.col)
  
  lgd_group <- Legend(at = gend, type = "points", legend_gp = gpar(col = colors), title_position = "topleft", title = title)
  lgd_points <- Legend(at = namesh, type = "points", legend_gp = gpar(col = cola), title_position = "topleft", title = "Class")
  lgd_lines <- Legend(at = c("Positive", "Negative"), type = "points", legend_gp = gpar(col = c('orange', 'blue')), title_position = "topleft", title = "Correlation")
  lgd_edges <- Legend(at = c(-1, 1), col_fun = col_fun, title_position = "topleft", title = "Edges")
  lgd_list_vertical <- packLegend(lgd_group, lgd_points, lgd_lines, lgd_edges)
  circos.par(gap.after = 1.5, start.degree = 90)
  chordDiagram(df, annotationTrack = c("grid"), grid.col = grid.col, directional = FALSE, symmetric = TRUE, scale = FALSE,
               link.lwd = 0.3, link.border = "white", order = rownames(tes_t), preAllocateTracks = 1, col = col_fun, transparency = 0.25, big.gap = 10, small.gap = 1)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    circos.axis(h = "top", labels.cex = 0.000001, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
  windowsFonts(A = windowsFont("Calibri (Body)"))
  draw(lgd_list_vertical, x = unit(5, "mm"), y = unit(5, "mm"), just = c("left", "bottom"))
  
  dev.copy(jpeg, paste0(gend, n_level, 'hiee.jpg'), width = 12, height = 12, units = "in", res = 1000)
  dev.off()
  
  knitr::include_graphics(paste0(gend, n_level, 'hiee.jpg'))
  daiR::image_to_pdf(paste0(gend, n_level, 'hiee.jpg'), pdf_name = paste0(paste0(gend, n_level, 'hie'), '.pdf'))
  my_image <- image_read(paste0(gend, n_level, 'hiee.jpg'))
  my_svg <- image_convert(my_image, format = "svg")
  image_write(my_svg, paste(paste0(gend, n_level, 'hie.jpg'), ".svg"))
}


# All variables
n_level <- 0.01
circos.clear()
vars <- list(resulta)
big <- 'Yes'
title <- 'All Variables'
rem <- x4
modi <- 5
colt <- 'black'
a <- length(x1) - 1
b <- length(x2)
c <- length(x3)
d <- length(x4)
e <- length(x5)
f <- length(x6)
gend <- c('All')
colors <- c('blue')
group_chords(vars[[1]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)

# Genderwise
vars <- list(resultaf, resultam)
big <- 'No'
title <- 'Genders Separated'
rem <- x4
modi <- 4
colt <- 'black'
colors <- c('white', 'black')
a <- length(x1) - 2
b <- length(x2)
c <- length(x3)
d <- length(x4)
e <- length(x5)
f <- length(x6)
gend <- c('Female')
colors <- c('white')
group_chords(vars[[1]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)
gend <- c('Male')
colors <- c('black')
group_chords(vars[[2]], n_level, fig_name, big, rem, modi, colt, gend, colors, a, b, c, d, e, f)


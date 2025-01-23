# Load necessary libraries
library(dplyr)
library(Hmisc)

# Match column names and update them
hupo <- match(colnames(tvauxe)[colnames(tvauxe) %in% groups2[, 3]], groups2[, 3])
colnames(tvauxe)[colnames(tvauxe) %in% groups2[, 3]] <- groups2[hupo, 2]
ok <- colnames(tvauxe)

# Ensure tv_c is a data frame before filtering
tv_c <- as.data.frame(tv_c)

# Split data by Gender
tvf <- tv_c %>% filter(Gender == min(Gender))
tvm <- tv_c %>% filter(Gender == max(Gender))

# List of data frames
tvtest <- list(tv_c, tvf, tvm)

# Update column names
update_colnames <- function(df) {
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

tvtest <- lapply(tvtest, update_colnames)
tv_c <- tvtest[[1]]
tvf <- tvtest[[2]]
tvm <- tvtest[[3]]

# Update specific column name
x4[x4 == "X7.oxo.DCA_S"] <- "X7-oxo-DCA_S"

# Function to calculate Spearman correlation and update column names
calculate_corr <- function(dat, ok) {
  dat <- as.data.frame(dat) %>% select(-c('PatientNumber', 'Gender'))
  result <- (rcorr(as.matrix(dat), type = 'spearman'))$r
  if (ncol(result) == length(ok) - 2) {
    colnames(result) <- ok[3:length(ok)]
    rownames(result) <- ok[3:length(ok)]
  }
  return(result)
}

resulta <- calculate_corr(tv_c, ok)
resultaf <- calculate_corr(tvf, ok)
resultam <- calculate_corr(tvm, ok)

# Check columns
at <- colnames(resulta)[1:(length(x1) - 1)]
bt <- colnames(resulta)[(length(at) + 1):(length(at) + length(x2))]
ct <- colnames(resulta)[(length(at) + length(bt) + 1):(length(at) + length(bt) + length(x3))]
dt <- colnames(resulta)[(length(at) + length(bt) + length(ct) + 1):(length(at) + length(bt) + length(ct) + length(x4))]
et <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(x5))]
ft <- colnames(resulta)[(length(at) + length(bt) + length(ct) + length(dt) + length(et) + 1):(length(at) + length(bt) + length(ct) + length(dt) + length(et) + length(x6))]

# Lengths of each group
atl <- length(at)
btl <- length(bt)
ctl <- length(ct)
dtl <- length(dt)
etl <- length(et)
ftl <- length(ft)

# Apply threshold and update results
apply_threshold <- function(result, n_level) {
  Nrr <- qpNrr(result, verbose = FALSE)
  Nrr[is.na(Nrr)] <- 1
  cond <- as.matrix(Nrr < n_level)
  RN <- as.data.frame(result)
  tes_t <- cond * RN
  return(as.matrix(tes_t))
}

n_level <- 0.01
resulta <- apply_threshold(resulta, n_level)
resultaf <- apply_threshold(resultaf, n_level)
resultam <- apply_threshold(resultam, n_level)

# Update Gender column name
colnames(resulta)[colnames(resulta) == 'Gender'] <- 'Sex(F-M+)'
rownames(resulta)[rownames(resulta) == 'Gender'] <- 'Sex(F-M+)'

# # Update remaining column names
# update_remaining_colnames <- function(result, ok) {
#   if (ncol(result) == length(ok) - 2) {
#     colnames(result)[2:ncol(result)] <- ok[3:length(ok)]
#     rownames(result)[2:ncol(result)] <- ok[3:length(ok)]
#   }
#   return(result)
# }
# 
# resulta <- update_remaining_colnames(resulta, ok)
# resultaf <- update_remaining_colnames(resultaf, ok)
# resultam <- update_remaining_colnames(resultam, ok)


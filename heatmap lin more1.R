tv=tv_covscl
if (Group=='male') {NAFLDo=tv[tv[,'Gender']==max(tv[,'Gender']),]} else if (Group=='female') 
{NAFLDo=tv[tv[,'Gender']==min(tv[,'Gender']),]} else if (Group=='All') {NAFLDo=tv}
SG0=NAFLDo[,c(2:dim(tv)[2])]
#https://stackoverflow.com/questions/10688137/how-to-fix-spaces-in-column-names-of-a-data-frame-remove-spaces-inject-dots
oknames=colnames(SG0)
SG0=data.frame(SG0)
colnames(SG0)
colnames(SG0[,8:27]) <- gsub("-", ".", colnames(SG0[,8:27]))
colnames(SG0[,8:27]) <- gsub("/", ".", colnames(SG0[,8:27]))
hesh=c()

Treatment=colnames(tv_all)[52:58];
Mediator=colnames(tv_all)[9:28];
Outcome=colnames(tv_all)[c(29:51,59:71)];

xnam <- colnames(SG0)[c(4:7)]
Treatment2=Treatment
y <- Treatment2 

# Initialize variables
j <- 1
i <- 1
p.val <- c()
hesh <- data.frame()  # Assuming hesh is a data frame

# Print the column names of SG0 to verify the presence of variables
cat("Column names of SG0:\n")
print(colnames(SG0))

# Define a function to perform the repeated operations
perform_analysis <- function(xnam, y, Group, SG0, tv) {
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # Check if the variables exist in the data frame
      if (!(xnam[i] %in% colnames(SG0))) {
        stop(paste("The variable", xnam[i], "is not found in the data frame SG0"))
      }
      if (!(y[j] %in% colnames(SG0))) {
        stop(paste("The variable", y[j], "is not found in the data frame SG0"))
      }
      
      # Construct the formula based on the Group
      if (Group != 'All') {
        fmla <- as.formula(paste(y[j], " ~ ", paste(c(xnam[i], 'BMI', 'AGE'), collapse = "+")))
      } else {
        fmla <- as.formula(paste(y[j], " ~ ", paste(c(xnam[i], 'BMI', 'AGE', 'Gender'), collapse = "+")))
      }
      
      # Fit the linear model
      poissone <- lm(fmla, data = SG0)
      
      # Extract p-values and summary statistics
      p.val <- anova(poissone)$'Pr(>F)'[1]
      ps <- summary(poissone)
      pss <- ps[[4]]
      
      # Collect results
      hoesh <- c(y[j], xnam[i], Group, pss[2, 1], pss[2, 4], pss[2, 2])
      r <- as.numeric(hoesh[4])
      p <- as.numeric(hoesh[5])
      rsadj <- as.numeric(hoesh[6])
      
      # Update column names and other variables
      colnames(SG0) <- colnames(tv)[2:dim(tv)[2]]
      Treatment <- hoesh[2]
      Mediator <- hoesh[1]
      
      # Append results to hesh
      hesh <<- rbind(hesh, c(y[j], xnam[i], Group, r, pss[2, 4], rsadj))
    }
  }
}

# Define the different sets of xnam and y
xnam_sets <- list(
  colnames(SG0)[c(2)],
  colnames(SG0)[c(3)],
  Treatment2,
  c(x3, x6),
  c('AGE', 'BMI', colnames(SG0)[c(4:7)]),
  c(x3, x6)
)

y_sets <- list(
  Treatment2,
  Treatment2,
  colnames(SG0[, 8:27]),
  colnames(SG0[, 8:27]),
  colnames(SG0[, 8:27]),
  colnames(tv_all)[52:58]
)

# Check if all necessary variables are present in SG0
required_vars <- unique(c(unlist(xnam_sets), unlist(y_sets)))
missing_vars <- setdiff(required_vars, colnames(SG0))
if (length(missing_vars) > 0) {
  stop(paste("The following variables are missing in SG0:", paste(missing_vars, collapse = ", ")))
}

# Loop through the sets and perform the analysis
for (k in 1:length(xnam_sets)) {
  xnam <- xnam_sets[[k]]
  y <- y_sets[[k]]
  cat("Processing set", k, "\n")  # Debugging information
  cat("xnam:", xnam, "\n")  # Debugging information
  cat("y:", y, "\n")  # Debugging information
  perform_analysis(xnam, y, Group, SG0, tv)
}

# Clean up
rm(xnam, y)
hoesh=c()
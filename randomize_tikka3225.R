# Create a sample dataframe
df <- data.frame(
  A = c(1, 1, 1),
  B = c(1, 1, 1)
)

# Generate random numbers for each column within a range (e.g., between 1 and 10)
random_numbers <- sapply(df, function(x) runif(length(x), min = 1, max = 10))


# df_multiplied <- apply(df, 2, function(x) x * runif(length(x), min = 1, max = 10))

s1 <- sample(seq(-1, 1, by = 0.1), nrow(df), replace = FALSE)
s1


# Multiply each column in the dataframe by the corresponding random numbers
df_multiplied <- df * random_numbers

# Print the result
print(df_multiplied)

df=CovariatesScaledData

# Generate random numbers for each column within a range (e.g., between 1 and 10)
random_numbers <- sapply(df[,3:(dim(df)[2])], function(x) runif(length(x), min = 0.8, max = 1.2))

# Multiply each column in the dataframe by the corresponding random numbers
df_multiplied <- df * random_numbers


#This works
df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15,
  D = 16:20)


# Define the range
min_val <- 0.9
max_val <- 1.1

# Apply the multiplication to columns A to D
df[, c("A", "B", "C", "D")] <- lapply(df[, c("A", "B", "C", "D")], function(x) {
  x * runif(length(x), min = min_val, max = max_val)})


# Let's introduce some randomization. First, let us define the range of the random numbers:
min_val <- 0.83; max_val <- 1.17

CovariatesScaledData[,3:(dim(CovariatesScaledData)[2])]=
  lapply(CovariatesScaledData[,3:(dim(CovariatesScaledData)[2])], function(x) {x * runif(length(x), min = min_val, max = max_val)})
CurrentData[,3:(dim(CurrentData)[2])]=
  lapply(CurrentData[,3:(dim(CurrentData)[2])], function(x) {x * runif(length(x), min = min_val, max = max_val)})
tvauxe[,3:(dim(tvauxe)[2])]=
  lapply(tvauxe[,3:(dim(tvauxe)[2])], function(x) {x * runif(length(x), min = min_val, max = max_val)})
AllData[,3:(dim(AllData)[2])]=
  lapply(AllData[,3:(dim(AllData)[2])], function(x) {x * runif(length(x), min = min_val, max = max_val)})


multiply_with_random <- function(df, cols, min_val, max_val) {
  df[cols] <- lapply(df[cols], function(x) {
    x * runif(length(x), min = min_val, max = max_val)})
  return(df)}


# Create a sample data frame
df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15,
  D = 16:20
)

# Define the range
min_val <- 0.7
max_val <- 1.1

# Apply the function to columns A to D
df <- multiply_with_random(df, c("A", "B", "C", "D"), min_val, max_val)

# View the modified data frame
print(df)


multiply_with_random <- function(df, cols, min_val, max_val) {
  for (col in cols) {
    df[[col]] <- df[[col]] * runif(nrow(df), min = min_val, max = max_val)
  }
  return(df)
}
# Create a sample data frame
df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15,
  D = 16:20
)

# Define the range
min_val <- 2
max_val <- 500

# Apply the function to columns A to D
df <- multiply_with_random(df, c("B", "C", "D"), min_val, max_val)

# View the modified data frame
print(df)


multiply_with_random <- function(df, cols, min_val, max_val) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- df[[col]] * runif(nrow(df), min = min_val, max = max_val)
    } else {
      warning(paste("Column", col, "does not exist in the data frame"))
    }
  }
  return(df)
}


# Create a sample data frame
df <- data.frame(
  A = 1:5,
  B = 6:10,
  C = 11:15,
  D = 16:20
)

# Define the range
min_val <- 2
max_val <- 5

# Apply the function to columns A to D
df <- multiply_with_random(df, c("A", "B", "C", "D"), min_val, max_val)

# View the modified data frame
print(df)

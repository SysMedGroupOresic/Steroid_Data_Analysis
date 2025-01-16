the_funal <- function(tv_all, Group, ok, fn, adj, sig.level, sick, sick_group, joo) {
  # Ensure 'tv_covscl' is a data frame
  if (!is.data.frame(tv_covscl)) {
    stop("'tv_covscl' must be a data frame.")
  }

  # Ensure 'Gender' column exists and is valid
  if (!("Gender" %in% colnames(tv_covscl))) {
    stop("'Gender' column not found in the provided data.")
  }


# Subset data based on Gender (Group)
NAFLDo <- switch(Group,
                 male = tv_covscl[tv_covscl[["Gender"]] == max(tv_covscl[["Gender"]], na.rm = TRUE), ],
                 female = tv_covscl[tv_covscl[["Gender"]] == min(tv_covscl[["Gender"]], na.rm = TRUE), ],
                 All = tv_covscl
  )

# Check if the subset has rows
if (nrow(NAFLDo) == 0) {
  stop(paste("Subset data for group", Group, "is empty. Check the conditions and data."))
}

# Create SG0 by excluding the first column
SG0 <- as.data.frame(NAFLDo[, -1])  # Ensure SG0 is a data frame and exclude the first column
colnames(SG0) <- make.names(colnames(SG0))  # Sanitize column names

# Check if SG0 has been created
print(nrow(SG0))
print(head(SG0))

  hesh <- data.frame(y = character(), x = character(), Group = character(),
                     Estimate = numeric(), p_value = numeric(), Std_Error = numeric(),
                     stringsAsFactors = FALSE)

  Treatments <- make.names(colnames(tv_all)[52:58])
  Mediators <- make.names(colnames(tv_all)[9:28])
  Outcomes <- make.names(colnames(tv_all)[c(29:51, 59:71)])
  covariates <- c("BMI", "AGE")

  # Function to run linear models and store results
       run_model <- function(x, y, group) {
  if (all(is.na(SG0[[x]])) || all(is.na(SG0[[y]]))) {
    print(paste("Skipping model for", x, "and", y, "due to missing data"))
    return(list(y = y, x = x, Group = group, Estimate = NA, p_value = NA, Std_Error = NA))
  }

  formula_str <- paste(y, "~", paste(c(x, covariates, if (group == "All") "Gender"), collapse = "+"))
  model <- tryCatch(lm(as.formula(formula_str), data = SG0), error = function(e) NULL)

  if (is.null(model)) {
    print(paste("Model failed for", x, "and", y))
    return(list(y = y, x = x, Group = group, Estimate = NA, p_value = NA, Std_Error = NA))
  }

  summary_model <- summary(model)
  if (!is.null(summary_model$coefficients) && length(summary_model$coefficients) >= 2) {
    return(list(y = y, x = x, Group = group,
                Estimate = summary_model$coefficients[2, "Estimate"],
                p_value = summary_model$coefficients[2, "Pr(>|t|)"],
                Std_Error = summary_model$coefficients[2, "Std. Error"]))
  } else {
    return(list(y = y, x = x, Group = group, Estimate = NA, p_value = NA, Std_Error = NA))
  }
}


  # Apply models to each combination of variables
  for (x in colnames(SG0)[4:7]) {
    for (y in Treatments) {
      res <- run_model(x, y, Group)
      hesh <- rbind(hesh, as.data.frame(res, stringsAsFactors = FALSE))
    }
  }

  # Convert results to data frame
  colnames(hesh) <- c("y", "x", "Group", "Estimate", "p_value", "Std_Error")
  hesh$Estimate <- as.numeric(hesh$Estimate)
  hesh$p_value <- as.numeric(hesh$p_value)
  hesh$Std_Error <- as.numeric(hesh$Std_Error)

  # Adjust p-values if specified
  if (adj == 'ok') {
    hesh$p_value <- p.adjust(hesh$p_value, method = 'BH')
  }

  print("Covariate filter conditions:")
print(covariates)
print("Mediator filter conditions:")
print(Mediators)

print("Data before filtering:")
print(hesh)


  # # Filter and reshape data for output
  # covariate_filter <- hesh$x %in% covariates
  # mediator_filter <- hesh$y %in% Mediators
  # result <- hesh[covariate_filter & mediator_filter, ]
  


# Apply the filters manually
covariate_filter <- hesh$x %in% covariates
mediator_filter <- hesh$y %in% Mediators
print("Data after manual filtering:")
print(hesh[covariate_filter & mediator_filter, ])
# Relax filters to test if this impacts the number of rows
result <- hesh # without filtering


  
  print("Result after filtering:")
  print(result)
  
  print("Unique values in hesh$x:")
print(unique(hesh$x))

print("Unique values in hesh$y:")
print(unique(hesh$y))



  # Plotting
  if (nrow(result) > 0) {
    library(corrplot)
    library(magick)

    # Prepare correlation and significance matrices
    cor_matrix <- xtabs(Estimate ~ y + x, data = result)
    sig_matrix <- xtabs(p_value ~ y + x, data = result)

    # Ensure matrices are correctly formatted and filled
    cor_matrix <- as.matrix(as.data.frame.matrix(cor_matrix))
    sig_matrix <- as.matrix(as.data.frame.matrix(sig_matrix))

    # Clipping the correlation matrix values to a range
    cor_matrix[cor_matrix > 0.5] <- 0.5
    cor_matrix[cor_matrix < -0.5] <- -0.5
	
	print("Correlation matrix dimensions:")
    print(dim(cor_matrix))
    print("Significance matrix dimensions:")
    print(dim(sig_matrix))
	
	library(corrplot)
    test_matrix <- matrix(runif(100), nrow=10)
    corrplot(test_matrix, method="color")



    # Plot on the R screen (interactive)
    plot_title <- paste("Linear Model Estimate Plot", Group)
    corrplot(cor_matrix,
             method = "color",
             type = "full",
             order = "original",
             col = colorRampPalette(c('blue', 'white', 'orange'), alpha = TRUE)(100),
             p.mat = sig_matrix,
             sig.level = c(0.001, 0.01, 0.05),
             insig = "label_sig",
             tl.col = "black",
             tl.cex = 0.5,
             cl.cex = 1.09,
             pch.cex = 1.09,
             diag = TRUE)

    # Ensure the plot is visible before saving
    print("Plot displayed. Now saving the file.")

    # Plot to file
    output_file <- paste0(fn, "_", Group, ".jpg")
    jpeg(output_file, width = 2500, height = 4400, quality = 100, res = 300)

    corrplot(cor_matrix,
             method = "color",
             type = "full",
             order = "original",
             col = colorRampPalette(c('blue', 'white', 'orange'), alpha = TRUE)(100),
             p.mat = sig_matrix,
             sig.level = c(0.001, 0.01, 0.05),
             insig = "label_sig",
             tl.col = "black",
             tl.cex = 0.5,
             cl.cex = 1.09,
             pch.cex = 1.09,
             diag = TRUE)
    dev.off()
	
	
	output_file <- paste0(fn, "_", Group, ".jpg")
    print(paste("Saving plot to:", output_file))
    jpeg(output_file, width = 2500, height = 4400, quality = 100, res = 300)
    corrplot(cor_matrix, method = "color", type = "full", order = "original")
    dev.off()
	
	tryCatch({
    jpeg(output_file, width = 2500, height = 4400, quality = 100, res = 300)
    corrplot(cor_matrix, method = "color", type = "full", order = "original")
    dev.off()
    if (file.exists(output_file)) {
     print(paste("File created successfully:", output_file))
   }
 }, error = function(e) {
  print(paste("Error during plotting:", e$message))
    })


if (file.exists(output_file)) {
  print(paste("File created successfully:", output_file))
} else {
  stop("The image file was not created. Check the working directory or permissions.")
}


    # Check if image was actually created
    if (file.exists(output_file)) {
      print(paste("File created successfully:", output_file))
    } else {
      stop("The image file was not created. Check the working directory or permissions.")
    }
  } else {
    message("No valid data for plotting.")
  }

  # Return processed data
  return(list(results = result))
}


#Joku miniversio tulee, mutta not good... pitää kattoo myöhemmin..
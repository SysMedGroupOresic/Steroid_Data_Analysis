# Making Causal Mediation Analysis 

# Set library paths if needed
# .libPaths(c("C:/Program Files/R/R-4.4.1/library", .libPaths()))
# Fyi: adding echo=FALSE here (at the R markdown header) does not show this code block.

# List of libraries to load (alphabetically sorted)




packages <- c("bigsnpr", "binilib", "brickster", "car", "censReg", "circlize", "ComplexHeatmap", "correlation", 
              "corrplot", "daiR", "datarium", "dmetar", "dplyr", "effsize", "extrafont", "forcats", "fs", "FSA", 
              "ggcorrplot", "ggforce", "ggforestplot", "ggplot2", "ggplotify", "ggpubr", "ggsankey", "ggsankeyfier", 
              "ggh4x", "ggtext", "glmnet", "grid", "Hmisc", "hrbrthemes", "igraph", "insight", "lavaan", "lmtest", 
              "lme4", "lsr", "magick", "magrittr", "Maaslin2", "mdatools", "mediation", "meta", "mgcv", "mlma", 
              "MOFA2", "pheatmap", "PerformanceAnalytics", "pathviewr", "plyr", "plotrix", "ppcor", "prettydoc", 
              "psych", "quantreg", "qpgraph", "ragg", "RColorBrewer", "rcompanion", "readxl", "remotes", "reshape2", 
              "rgl", "rmarkdown", "rmdformats", "rstatix", "scales", "scater", "scatterplot3d", "sjPlot", "stringr", 
              "superb", "tibble", "tidyverse", "tint", "tufte", "viridis","WGCNA", "xlsx")

# Load all libraries
.libPaths(c("C:/Program Files/R/R-4.4.1/library", .libPaths()))
# invisible(lapply(packages, library, character.only = TRUE))

# invisible(lapply(packages, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     install.packages(pkg)
#   }
#   library(pkg, character.only = TRUE)
# }))
# 
library(scales)
invisible(lapply(setdiff(packages, "scales"), library, character.only = TRUE))

setwd("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis")

AllData=read_excel("Copy of promic_data_pfas_steroids_wo_outliers_empty removal imputing.xlsx") 
remove=c('PROMIC-013', 'PROMIC-153', 'PROMIC-213', 
         'PROMIC-032', 'PROMIC-023', 'PROMIC-056', 
         'PROMIC-073', 'PROMIC-107', 'PROMIC-116', 
         'PROMIC-127', 'PROMIC-151', 'PROMIC-220', 
         'PROMIC-230', 'PROMIC-232', 'PROMIC-281')
length(remove)
dim(AllData)
AllData=AllData[!as.vector(unlist(AllData[,1])) %in% remove,] #this works
dim(AllData)

TreatmentVariables=c("PFOA" , "PFNA"     ,  "PFDA"  ,  "PFUdA"  ,   "PFHxSK"  ,    "L-PFHpS"    ,    "PFOSK",    "6:2FTS")

OutcomeVariables='cancer_risk_category'
AllData[,2]=AllData[,2]/1000
colnames(AllData)[c(2)]='Cholesterol'
MediatorVariables=colnames(AllData)[c(2,34:53)]

HID=AllData[,c(TreatmentVariables, MediatorVariables)]

Log2TransformedData <- log2(HID);
AutoScaledData <- prep.autoscale(as.matrix(Log2TransformedData), center = TRUE, scale = TRUE);  #https://svkucheryavski.gitbooks.io/mdatools/content/preprocessing/text.html
AllData=cbind(AllData[,!colnames(AllData) %in% c(TreatmentVariables, MediatorVariables)],AutoScaledData); 
Group='All'
sick='NA'
t.valueList='minmax'
test <- 1
name='Basic Mediation Analysis for ProstateCancer_IliaS_tikka24925'
fn=''
date=''
simss <- 100

# The basic hypothesis. All are variables (y~x+m;m~x)
# RunMediationAnalysis <- function(TreatmentVariables, MediatorVariables, OutcomeVariables, AllData, Group, name, simss, t.valueList, test, sick, sick_group, date = format(Sys.Date(), "%Y-%m-%d"),fn) {
  
  # Ensure AllData is a data frame
  if (!is.data.frame(AllData)) {
    AllData <- as.data.frame(AllData)
    message("Converted AllData to data frame.")
  }
  
  # Determine the condition based on the group
  if (Group == 'female') {
    cond <- AllData[, 'Gender'] == min(AllData[, 'Gender'], na.rm = TRUE)
  } else if (Group == 'male') {
    cond <- AllData[, 'Gender'] == max(AllData[, 'Gender'], na.rm = TRUE)
  } else {
    cond <- rep(TRUE, nrow(AllData))
  }
  
  print(paste("Group:", Group, "- Rows after filtering:", sum(cond)))
  
  # Filter the dataset based on the condition and sickness status
  if (sick == 'yes') {
    tv_red <- AllData[cond & as.vector(sick_group), ]
  } else {
    tv_red <- AllData[cond, ]
  }
  
  print(paste("Group:", Group, "- Rows after sick filter:", nrow(tv_red)))
  
  # Extract variables
  X <- tv_red[, TreatmentVariables, drop = FALSE]
  M <- tv_red[, MediatorVariables, drop = FALSE]
  Y <- tv_red[, OutcomeVariables, drop = FALSE]
  
 
  
  # Combine and clean data
  Data <- na.omit(data.frame(cbind(X, M, Y)))
 
  
  # Get variable names
  x_names <- make.names(colnames(X))
  m_names <- make.names(colnames(M))
  
  Data$cancer_risk_numeric <- as.numeric(factor(Data$cancer_risk_category,
                                                  levels = c("Benign", "low risk", "intermediate risk", "high risk"),
                                                  labels = c(1, 2, 3, 4)))
  
  Data=Data[,!colnames(Data) %in% colnames(Y)]
  y_names <- 'cancer_risk_numeric'#make.names(colnames(Y))
  
  
  
  # Define control and treatment values
  # control.value <- 'Benign'#colMins(as.matrix(Data[, x_names]))
  # treat.value <- 'high risk'#colMaxs(as.matrix(Data[, x_names]))
  
  
  # Define treatment and control values based on quantiles or fixed values
  treat.value <- sapply(Data[, x_names], function(x) quantile(x, 0.75, na.rm = TRUE))
  control.value <- sapply(Data[, x_names], function(x) quantile(x, 0.25, na.rm = TRUE))
  
  # Calculate treatment and control values for each treatment variable
  # Correct way to name the treatment and control values
  treat.value <- setNames(
    sapply(x_names, function(x) quantile(Data[[x]], 0.75, na.rm = TRUE)),
    x_names
  )
  
  control.value <- setNames(
    sapply(x_names, function(x) quantile(Data[[x]], 0.25, na.rm = TRUE)),
    x_names
  )
  
  
  
  # Initialize results
  res <- data.frame()
  rn <- c()
  
  for (y in y_names) {
    for (m in m_names) {
      for (x in x_names) {
        
        # Build formulas
        fmla1 <- as.formula(paste0("`", m, "` ~ `", x, "`"))
        fmla2 <- as.formula(paste0("`", y, "` ~ ", paste0("`", c(x, m), "`", collapse = " + ")))
        
        model_m <- lm(fmla1, data = Data)
        model_y <- lm(fmla2, data = Data)
        
        
        # model_y <- glm(fmla2, data = Data[complete.cases(Data[, c(x, m, y)]), ], family = binomial)
        
        
        # Determine treatment and control values
        if (t.valueList == "no") {
          treat_val <- 1
          control_val <- 0
        } else if (t.valueList == "yes") {
          if (!is.na(Data[test, x])) {
            treat_val <- Data[test, x]
            control_val <- control.value[x]
          } else {
            print(paste("Skipping due to NA test value for:", x))
            next
          }
        } else if (t.valueList == "minmax") {
          treat_val <- treat.value[x]
          control_val <- control.value[x]
        } else {
          treat_val <- 1
          control_val <- 0
        }
        
        print(paste("Running mediation for:", x, m, y, "- Treat val:", treat_val, "- Control val:", control_val))
        
        # Run mediation
        med_out <- tryCatch({
          if (t.valueList == "no") {
            mediation::mediate(model_m, model_y, treat = x, mediator = m, sims = simss)
          } else if (!is.null(treat_val) && !is.null(control_val)) {
            mediation::mediate(model_m, model_y, treat = x, mediator = m, sims = simss,
                               control.value = control_val, treat.value = treat_val)
          } else {
            warning(paste("Skipping due to missing treatment/control values for:", x, m, y))
            return(NULL)
          }
        }, error = function(e) {
          warning(paste("Mediation failed for:", x, m, y, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(med_out)) {
          summary_out <- summary(med_out)
          tmp <- c(summary_out$d0, summary_out$d0.p, summary_out$d0.ci[1], summary_out$d0.ci[2],
                   summary_out$z0, summary_out$z0.p, summary_out$z0.ci[1], summary_out$z0.ci[2],
                   summary_out$n1, summary_out$n1.p, summary_out$n1.ci[1], summary_out$n1.ci[2],
                   summary_out$tau.coef, summary_out$tau.p, summary_out$tau.ci[1], summary_out$tau.ci[2])
          res <- rbind(res, tmp)
          rn <- c(rn, paste(x, m, y, sep = "_"))
        } else {
          print(paste("Mediation returned NULL for:", x, m, y))
        }
      }
    }
  }
  
  # Finalize results
  # if (dim(res)[1]==0 & dim(res)[2]==0) {res=t(data.frame(rep(0,16)))}
  
  # if (nrow(res) == 0) {
  #   cat("No results found — saving empty file for group:", Group, "\n")
  #   res <- data.frame(Note = paste("No mediation results for group:", Group))
  # }
  # Define column names
  col_names <- c('ACME', 'd0.p', 'd0.ci_l', 'd0.ci_u',
                 'ADE', 'z0.p', 'z0.ci_l', 'z0.ci_u',
                 'Proportion Mediated', 'n1.p', 'n1.ci_l', 'n1.ci_u',
                 'Total Effect', 'tau.p', 'tau.ci_l', 'tau.ci_u')
  
  # If results exist, finalize and sort
  if (nrow(res) > 0) {
    rownames(res) <- rn
    colnames(res) <- col_names
    res <- res[order(res[, 'd0.p']), ]
    rownames(res) <- gsub("X11", "11", rownames(res))
    rownames(res) <- gsub("X17", "17", rownames(res))
  } else {
    # Create an empty data frame with the correct column names
    res <- data.frame(matrix(ncol = length(col_names), nrow = 0))
    colnames(res) <- col_names
    rownames(res) <- "No results"
    res[1, 1] <- paste("No mediation results for group (perhaps due similar output values or colinearity):", Group, fn)
    cat("No results found — saving message row for group:", Group, "\n")
    
  }
  
  # Save to Excel
  file_path <- paste0("C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/", name, "_", fn, "_", Group, "_", date, "_mediation_results.xlsx")
  cat("Saving results to:", file_path, "\n")
  write.xlsx(res, file = file_path, append = FALSE, row.names = TRUE)
  
  return(res)
}

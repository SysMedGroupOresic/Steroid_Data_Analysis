# CombinedData[1:5,1:20]
CombinedData_f=CombinedData[CombinedData[,'SEX.1F.2M']==min(CombinedData[,'SEX.1F.2M']),]
# CombinedData_f=CombinedData
# dim(CombinedData_f)
# CombinedData_f[,c('AGE','E')]
# AllData[1:5,1:20]
AllData_f=AllData[AllData[,'Gender']==min(AllData[,'Gender']),]
# AllData_f=AllData
# dim(AllData_f)

var='E2/E1'

# CombinedData_fa=cbind(CombinedData_f[,'AGE'],AllData_f[,'E1']/AllData_f[,'E1'])
CombinedData_fa=cbind(CombinedData_f[,'AGE'],CombinedData_f[,'E2']/CombinedData_f[,'E1'])
colnames(CombinedData_fa) = c('AGE', 'E2_per_E1')
# CombinedData_fa[1:5,]
# plot(CombinedData_fa)


# Remove outliers using z-score
# Assuming CombinedData_fa is your original data frame
# z_scores <- data.frame(scale(CombinedData_fa))

# z_scores=
# Rename your data frame to avoid conflict
df <- CombinedData_fa
df_clean <- df
# Remove outliers using z-score
# df_clean <- df[apply(abs(z_scores), 1, function(row) all(row < 3)), ]


# Fit linear model
model <- lm(E2_per_E1 ~ AGE, data = data.frame(df_clean))
summary_model <- summary(model)

# Extract coefficients and R-squared
slope <- coef(model)[2]
intercept <- coef(model)[1]
r_squared <- summary_model$r.squared

# Create plot
ggplot(df_clean, aes(x = AGE, y = E2_per_E1)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatter Plot with Linear Regression (for Female Subjects)",
    subtitle = paste0("y = ", round(slope, 2), "x + ", round(intercept, 2),
                      ", R² = ", round(r_squared, 2)),
    x = "AGE (years)",
    y = paste0(var, " (pM)")
  ) + theme_minimal()

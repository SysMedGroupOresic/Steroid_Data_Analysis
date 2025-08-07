CombinedData[1:5,1:20]
CombinedData_f=CombinedData[CombinedData[,'SEX.1F.2M']==min(CombinedData[,'SEX.1F.2M']),]
dim(CombinedData_f)
CombinedData_f[,c('AGE','E')]


AllData[1:5,1:20]
AllData_f=AllData[AllData[,'Gender']==min(AllData[,'Gender']),]
dim(AllData_f)

CombinedData_fa=cbind(CombinedData_f[,'AGE'],AllData_f[,'E'])

CombinedData_fa=cbind(CombinedData_f[,'AGE'],CombinedData_f[,'E'])


colnames(CombinedData_fa) = c('AGE', 'E')                      
CombinedData_fa[1:5,]

plot(CombinedData_fa)



# Remove outliers using z-score
# Assuming CombinedData_fa is your original data frame
z_scores <- data.frame(scale(CombinedData_fa))

# Rename your data frame to avoid conflict
df <- CombinedData_fa
# Remove outliers using z-score
df_clean <- df[apply(abs(z_scores), 1, function(row) all(row < 3)), ]


# Fit linear model
model <- lm(E ~ AGE, data = data.frame(df_clean))
summary_model <- summary(model)

# Extract coefficients and R-squared
slope <- coef(model)[2]
intercept <- coef(model)[1]
r_squared <- summary_model$r.squared

# Create plot
ggplot(df_clean, aes(x = AGE, y = E)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatter Plot with Linear Regression (Outliers Removed)",
    subtitle = paste0("y = ", round(slope, 2), "x + ", round(intercept, 2),
                      ", R² = ", round(r_squared, 2)),
    x = "AGE (years)",
    y = "E (pM)"
  ) +
  theme_minimal()

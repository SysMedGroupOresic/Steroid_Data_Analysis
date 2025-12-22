# > LogCovariatesNonScaledData[1:10,1:10]
# PatientNumber Gender AGE      BMI Steatosis Grade Fibrosis Stage Necroinflammation  HOMA-IR        E     11-KT
# 24145190413   24145190413      1  45 33.76327               1              2                 1 1.617778 15.53917  9.984610
# 2459090909     2459090909      2  43 34.02500               0              0                 0 2.712889 15.43687  9.869037
# 24102261010   24102261010      1  46 36.03472               0              0                 0 3.006667 15.55027 10.127038
# 24152080813   24152080813      2  62 36.43884               0              0                 0 2.240000 15.28539  9.879408
# 2465141009     2465141009      1  29 36.47283               0              0                 0 1.328889 15.68478 10.790411
# 24139220313   24139220313      1  50 37.49664               0              0                 0 1.633333 15.88375  9.972597
# 2408221008     2408221008      1  56 37.54325               1              1                 0 1.778667 15.74763  9.941895
# 24118101212   24118101212      1  31 38.06173               0              0                 0 3.802667 15.39946  8.364428
# 2442260509     2442260509      1  57 38.10422               1              1                 1 6.417778 14.88263 10.048113
# 2488080910     2488080910      1  55 38.40703               0              0                 0 1.000000 14.41973 11.459892



Female=CombinedData[CombinedData[,2] %in% 1,] #'E2
Male=CombinedData[CombinedData[,2] %in% 2,] #'T/Epi-T'


scatterplot(Female[,c('AGE')],Female[,c('E2')])
scatterplot(Male[,c('AGE')],Male[,c('T/Epi-T')])



# Install if needed
install.packages(c("ggplot2", "ggpubr", "ggExtra", "scales"))

library(ggplot2)
library(ggpubr)   # for stat_cor / publication themes
library(ggExtra)  # for marginal boxplots
library(scales)   # for pretty axis formatting



make_scatter_with_marginals <- function(df, x, y, title,
                                        x_label = NULL, y_label = NULL,
                                        point_color = "#1f77b4",
                                        loess_color = "#FF7F0E",
                                        lm_color = "#2ca02c",
                                        point_size = 2.5) {
  # Resolve labels
  if (is.null(x_label)) x_label <- x
  if (is.null(y_label)) y_label <- y
  
  # Handle special characters in column names by using .data pronouns
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(color = point_color, size = point_size, alpha = 0.8) +
    # Linear regression line (with CI)
    geom_smooth(method = "lm", se = TRUE, color = lm_color, linewidth = 1.1) +
    # LOESS smooth (trend curve)
    geom_smooth(method = "loess", se = FALSE, color = loess_color, linewidth = 1.1, linetype = "dashed") +
    # Show Pearson correlation and p-value
    stat_cor(
      method = "pearson",
      label.x.npc = "left", label.y.npc = "top",
      aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "\n")),
      size = 6
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_pubr(base_size = 18) +  # big, clean text
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 18),
      axis.text  = element_text(size = 16),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  # Add marginal boxplots
  p_marginal <- ggMarginal(
    p,
    type = "boxplot",
    size = 6,              # thickness of marginal boxplots
    groupFill = FALSE,     # match main plot color
    margins = "both"
  )
  
  p_marginal
}



# Assuming Female is a data.frame with columns AGE and E2
p_female <- make_scatter_with_marginals(
  df = Female,
  x  = "AGE",
  y  = "E2",
  title   = "Female: Age vs Estradiol (E2)",
  x_label = "Age (years)",
  y_label = "Estradiol (pM)"
)
p_female





# Assuming Male is a data.frame with columns AGE and `T/Epi-T`
p_male <- make_scatter_with_marginals(
  df = Male,
  x  = "AGE",
  y  = "T/Epi-T",
  title   = "Male: Age vs Testosterone/Epitestosterone Ratio",
  x_label = "Age (years)",
  y_label = "Testosterone / Epitestosterone (ratio)"
)
p_male













p_female <- ggplot(Female, aes(x = AGE, y = E2)) +
  geom_point(color = "#1f77b4", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#2ca02c", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FF7F0E", linewidth = 1.1, linetype = "dashed") +
  stat_cor(
    method = "pearson",
    label.x.npc = "left",
    label.y.npc = "top",
    aes(label = paste0("r = ", ..r.. %>% round(3), "\n",
                       "p = ", ..p.. %>% format(scientific = TRUE, digits = 2))),
    size = 6
  ) +
  labs(
    title = "Female: Age vs Estradiol (E2)",
    x = "Age (years)",
    y = "Estradiol (pM)"
  ) +
  theme_pubr(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 18),
    axis.text  = element_text(size = 16)
  )

# Add marginal boxplots
p_female <- ggExtra::ggMarginal(p_female, type = "boxplot", margins = "both", size = 6, groupFill = FALSE)

# Add slope p-value
pval_lm_female <- summary(lm(E2 ~ AGE, data = Female))$coefficients[2, "Pr(>|t|)"]
p_female <- p_female +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Slope p = ", format(pval_lm_female, scientific = TRUE, digits = 2)),
           hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold")





















####
###






## ---------------------------
## Setup & Data Subsetting
## ---------------------------

# Helper: Install missing packages quietly
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install)
}

install_if_missing(c("ggplot2", "ggpubr", "ggExtra", "scales", "fs"))

library(ggplot2)
library(ggpubr)   # stat_cor, theme_pubr
library(ggExtra)  # ggMarginal for marginal boxplots
library(scales)
library(fs)

# Assuming CombinedData exists and column 2 encodes Gender: 1 = Female, 2 = Male
Female <- CombinedData[CombinedData[, 2] %in% 1, ]
Male   <- CombinedData[CombinedData[, 2] %in% 2, ]

# Optional: check column names exist
stopifnot("AGE" %in% names(Female), "E2" %in% names(Female))
stopifnot("AGE" %in% names(Male),  "T/Epi-T" %in% names(Male))


## ---------------------------
## Reusable Plot Function
## ---------------------------

make_scatter_with_marginals <- function(df, x, y, title,
                                        x_label = NULL, y_label = NULL,
                                        point_color = "#1f77b4",
                                        loess_color = "#FF7F0E",
                                        lm_color = "#2ca02c",
                                        point_size = 2.8,
                                        cor_method = "pearson") {
  # Resolve labels
  if (is.null(x_label)) x_label <- x
  if (is.null(y_label)) y_label <- y
  
  # Clean data: select and drop NA; ensure numeric
  df_clean <- df[, c(x, y)]
  names(df_clean) <- c("x", "y")
  # Try to coerce to numeric if they are factors/characters
  df_clean$x <- suppressWarnings(as.numeric(as.character(df_clean$x)))
  df_clean$y <- suppressWarnings(as.numeric(as.character(df_clean$y)))
  df_clean <- df_clean[is.finite(df_clean$x) & is.finite(df_clean$y), , drop = FALSE]
  
  # Compute slope p-value from linear model
  fit <- lm(y ~ x, data = df_clean)
  slope_p <- summary(fit)$coefficients[2, "Pr(>|t|)"]
  
  # Base scatter with LM and LOESS
  p <- ggplot(df_clean, aes(x = x, y = y)) +
    geom_point(color = point_color, size = point_size, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE, color = lm_color, linewidth = 1.2) +
    geom_smooth(method = "loess", se = FALSE, color = loess_color,
                linewidth = 1.2, linetype = "dashed") +
    # Correlation (r and p) shown at top-left
    ggpubr::stat_cor(
      method = cor_method,
      label.x.npc = "left",
      label.y.npc = "top",
      aes(label = paste0("r = ", ..r.. %>% round(3), "\n",
                         "p = ", ..p.. %>% format(scientific = TRUE, digits = 2))),
      size = 6
    ) +
    # Slope p-value (top-right)
    annotate(
      "text", x = Inf, y = Inf,
      label = paste0("Slope p = ",
                     format(slope_p, scientific = TRUE, digits = 2)),
      hjust = 1.1, vjust = 1.5, size = 5.5, fontface = "bold"
    ) +
    labs(title = title, x = x_label, y = y_label) +
    theme_pubr(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 20),
      axis.text  = element_text(size = 16),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  # Add marginal boxplots on both axes
  p_marginal <- ggMarginal(
    p,
    type = "boxplot",
    size = 6,          # thickness of marginal plots
    groupFill = FALSE,
    margins = "both"
  )
  
  return(p_marginal)
}


## ---------------------------
## Build Plots
## ---------------------------

# Female: AGE vs E2
p_female <- make_scatter_with_marginals(
  df = Female,
  x  = "AGE",
  y  = "E2",
  title   = "Female: Age vs Estradiol (E2)",
  x_label = "Age (years)",
  y_label = "Estradiol (pM)"  # adjust units if available
)

# Male: AGE vs T/Epi-T
p_male <- make_scatter_with_marginals(
  df = Male,
  x  = "AGE",
  y  = "T/Epi-T",  # handled safely inside the function
  title   = "Male: Age vs Testosterone/Epitestosterone",
  x_label = "Age (years)",
  y_label = "Testosterone / Epitestosterone (pM)"
)


## ---------------------------
## Save Outputs (PNG + PDF)
## ---------------------------

out_dir <- fs::path(getwd(), "scatterplots_output")
fs::dir_create(out_dir)

# Helper to save both PNG and PDF at high resolution
save_plot_both <- function(p, filename_base, width = 9, height = 7, dpi = 300) {
  png_file <- fs::path(out_dir, paste0(filename_base, ".png"))
  pdf_file <- fs::path(out_dir, paste0(filename_base, ".pdf"))
  
  ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_file, plot = p, width = width, height = height, device = cairo_pdf, bg = "white")
  
  message("✅ Saved: ", png_file)
  message("✅ Saved: ", pdf_file)
}

save_plot_both(p_female, "Female_Age_vs_E2")
save_plot_both(p_male,   "Male_Age_vs_T_over_EpiT")

# Print to the plotting device if desired
print(p_female)

print(p_male)



## ---------------------------
## Setup & Data Subsetting
## ---------------------------

# Ensure decimal dot globally (optional)
options(OutDec = ".")

# Helper: Install missing packages quietly
# install_if_missing <- function(pkgs) {
#   to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
#   if (length(to_install)) install.packages(to_install)
# }

# install_if_missing(c("ggplot2", "ggpubr", "ggExtra", "scales", "fs"))

library(ggplot2)
library(ggpubr)   # theme_pubr
library(ggExtra)  # ggMarginal for marginal boxplots
library(scales)
library(fs)

# Assuming CombinedData exists and column 2 encodes Gender: 1 = Female, 2 = Male
Female <- CombinedData[CombinedData[, 2] %in% 1, ]
Male   <- CombinedData[CombinedData[, 2] %in% 2, ]

# Safety checks
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
  df_clean$x <- suppressWarnings(as.numeric(as.character(df_clean$x)))
  df_clean$y <- suppressWarnings(as.numeric(as.character(df_clean$y)))
  df_clean <- df_clean[is.finite(df_clean$x) & is.finite(df_clean$y), , drop = FALSE]
  
  # Compute slope p-value from linear model
  fit <- lm(y ~ x, data = df_clean)
  slope_p <- summary(fit)$coefficients[2, "Pr(>|t|)"]
  
  # Compute correlation explicitly (to control formatting)
  ct <- suppressWarnings(cor.test(df_clean$x, df_clean$y, method = cor_method))
  r_val <- unname(ct$estimate)
  p_val <- ct$p.value
  
  # Base scatter with LM and LOESS
  p <- ggplot(df_clean, aes(x = x, y = y)) +
    geom_point(color = point_color, size = point_size, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE, color = lm_color, linewidth = 1.2) +
    geom_smooth(method = "loess", se = FALSE, color = loess_color,
                linewidth = 1.2, linetype = "dashed") +
    # Proper r and p labels (top-left)
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = paste0(
        "r = ", sprintf("%.3f", r_val), "\n",
        "p = ", format(p_val, scientific = FALSE, digits = 2)
      ),
      hjust = -0.1, vjust = 1.5, size = 6, fontface = "bold"
    ) +
    # Slope p-value (top-right)
    # annotate(
    #   "text", x = Inf, y = Inf,
    #   label = paste0("Slope p = ",
    #                  format(slope_p, scientific = TRUE, digits = 2)),
    #   hjust = 1.1, vjust = 1.5, size = 5.5, fontface = "bold"
    # ) +
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
  title   = "Female: Age vs. Estradiol (E2)",
  x_label = "Age (years)",
  y_label = "E2 (pM)"  # adjust units if available
);p_female

# Male: AGE vs T/Epi-T
p_male <- make_scatter_with_marginals(
  df = Male,
  x  = "AGE",
  y  = "T/Epi-T",
  title   = "Male: Age vs. Testosterone or Epitestosterone (T/Epi-T)",
  x_label = "Age (years)",
  y_label = "T/Epi-T (pM)"
);p_male


## ---------------------------
## Save Outputs (PNG + PDF)
## ---------------------------

out_dir <- fs::path(getwd(), "scatterplots_output")
fs::dir_create(out_dir)

save_plot_both <- function(p, filename_base, width = 9, height = 7, dpi = 300) {
  png_file <- fs::path(out_dir, paste0(filename_base, ".png"))
  pdf_file <- fs::path(out_dir, paste0(filename_base, ".pdf"))
  
  ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_file, plot = p, width = width, height = height, device = cairo_pdf, bg = "white")
  
  message("✅ Saved: ", png_file)
  message("✅ Saved: ", pdf_file)
}

save_plot_both(p_female, "Female_Age_vs_E2")
save_plot_both(p_male,   "Male_Age_vs_T_or_Epi-T")

# Display
print(p_female)
# print


print(p_male)

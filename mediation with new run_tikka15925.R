library(readxl)
library(ggplot2)
library(ggalluvial)
library(dplyr)

# Load data
all_all <- read_xlsx(path= "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/just alal_uus_All_All_tikka150925_mediation_results_mimmax.xlsx")
                     #just alal_uus_All_All_tikka150925_mediation_results_direct.xlsx") not ok
                     # just alal_uus_All_All_150925_allds_mediation_results_v2.xlsx")
#C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/total_All_All_tikka15925_mediation_results.xlsx")

# Filter and select top/bottom ACME values
filtered <- all_all %>% 
  filter(d0.p < 0.1) %>% 
  arrange(ACME)

# Select top 10 negative and top 10 positive ACME values
top_neg <- head(filtered %>% filter(ACME < 0), 10)

top_pos <- filtered %>%
  filter(ACME > 0) %>%
  arrange(-ACME) %>%
  head(10)


selected <- bind_rows(top_neg, top_pos)

# Assign color
selected <- selected %>%
  mutate(ACME_color = ifelse(ACME < 0, "blue", "orange"))



# Parse the first column into Exposure, Mediator, Outcome
labels <- sapply(as.character(selected[[1]]), function(x) {
  parts <- unlist(strsplit(x, "_"))
  if (length(parts) >= 3) {
    exposure <- parts[1]
    mediator <- parts[2]
    outcome <- paste(parts[3:length(parts)], collapse = "_")
    return(c(exposure, mediator, outcome))
  } else {
    return(c(parts, rep("NA", 3 - length(parts))))
  }
})

# Convert to data frame
labels_df <- as.data.frame(t(labels), stringsAsFactors = FALSE)
colnames(labels_df) <- c("Exposure", "Mediator", "Outcome")

# ✅ Replace "T.Epi.T" with "T/Epi-T" in Mediator column
labels_df$Mediator <- gsub("T\\.Epi\\.T", "T/Epi-T", labels_df$Mediator)
labels_df$Mediator <- gsub("17a\\.OHP5", "17a-OHP5", labels_df$Mediator)


# Combine with selected data
labels_df$ACME <- selected$ACME
labels_df$color <- selected$ACME_color
labels_df$id <- seq_len(nrow(labels_df))

# Rename for plotting
colnames(labels_df)[1:3] <- c("Contaminants", "Steroids", "Bile Acids or Lipids")

# View result
head(labels_df)


# Plot Sankey
ggplot(labels_df,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  geom_alluvium(width = 0.15, knot.pos = 0.5, alpha = 0.9) +
  geom_stratum(width = 0.15, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6, fontface = 'bold') +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name = "ACME Direction",
    values = c("blue" = "blue", "orange" = "orange"),
    labels = c("Negative", "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 20, color = "black", face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

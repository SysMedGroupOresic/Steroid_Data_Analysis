library(ggplot2)
library(ggalluvial)
library(readr)
library(readxl)
library(ggsankey)
library(dplyr)
# setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
# all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/100basic Male tikka3623.xlsx") #total Male tikka76523 hyp4b_oki.xlsx") #
#100basic Female tikka3623.xlsx 100basic Male tikka3623.xlsx 100basic All tikka3623 .xlsx
path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/"

# C:\Users\patati\Desktop\Turku\R\tests6\tests_basic
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3623 .xlsx")
# all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/total_All_All_tikka15925_mediation_results.xlsx")
  # C:\Users\patati\Documents\GitHub\Steroid_Data_Analysis

# all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/just alal_uus__All_160925_allds_mediation_results_mimmax_cov2.xlsx") #total Male tikka76523 hyp4b_oki.xlsx") #
# just alal_uus__All_160925_allds_mediation_results_mimmax_cov2.xlsx
# just alal_uus__male_160925_allds_mediation_results_mimmax_cov3.xlsx
# just alal_uus__female_160925_allds_mediation_results_mimmax_cov3.xlsx

sick='all samples';d='t'
date <- strftime(Sys.Date(), "%d%m%y") #Do not take the old date from the load...

lkm=10;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];

c1=c() #
u3=all_all

ACMEMedian=c(); ACMEpval=c(); ACMEVar=c()
ADEMedian=c(); ADEpval=c(); ADEVar=c()
c1= u3 #[u3[,'ADE'] < ADEMedian  & DV<ADEVar,] #& u3[,'z0.p']<ADEpval
ACMEMedian=0 # median(c1[,'ACME'][c1[,'ACME']>0])
c1=c1[c1[,'d0.p']<0.1, ]
c1=data.frame(c1)
a=c1[c1[,'ACME'] <0,]
b=c1[c1[,'ACME'] >0,]
a=a[order(a[,2]),];a=a[1:lkm,]
# a <- na.omit(a)
b=b[rev(order(b[,2])),];b=b[1:lkm,]
ab=rbind(a,b)
# dim(ab)
c1=ab
c1$ACME_color <- ifelse(c1$ACME < 0, "blue", "orange")

# Prepare data
RunAma <- na.omit(c1)
rt2 <- RunAma

if (d == 't') {
  hoi <- scan(text = as.character(rt2[[1]]), what = " ")
} else {
  hoi <- scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
}

hoi <- as.data.frame(matrix(hoi, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
colnames(hoi) <- c('Contaminants', 'Steroids', 'Bile Acids or Lipids')



# # Extract row names or first column depending on your format
names_vec <- as.character(rt2[[1]])  # or rownames(rt2) if needed

names_vec <- gsub("PFHxA_Branched", "PFHxA-B.",names_vec)


# Split each name by "_"
split_names <- strsplit(names_vec, "_")

# Convert to data frame
hoi <- do.call(rbind, lapply(split_names, function(x) {
  len <- length(x)
  if (len >= 3) {
    c(Contaminant = x[1], Steroid = x[2], Outcome = paste(x[3:len], collapse = "_"))
  } else {
    c(Contaminant = NA, Steroid = NA, Outcome = NA)
  }
}))

hoi <- as.data.frame(hoi, stringsAsFactors = FALSE)
colnames(hoi) <- c("Contaminants", "Steroids", "Bile Acids or Lipids")
hoi

hoi$ACME <- RunAma$ACME
hoi$color <- ifelse(hoi$ACME < 0,  "blue","orange")
hoi$id <- seq_len(nrow(hoi))

# c('#6FA3E0', 'white','#EC7B6E')

hoi$Contaminants <- gsub("PFHxA_Branched", "PFHxA_B.", hoi$Contaminants)
hoi$Steroids <- gsub("17a\\.OHP5", "17a-OHP5", hoi$Steroids)
hoi$Steroids <- gsub("17a\\.OHP4", "17a-OHP4", hoi$Steroids)
hoi$Steroids <- gsub("11\\.KT", "11-KT", hoi$Steroids)
hoi$Steroids <- gsub("11\\.KDHT", "11-KDHT", hoi$Steroids)




### ok

library(ggplot2)
library(ggalluvial)

p=ggplot(hoi,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  # geom_alluvium(
  #   aes(fill = color, colour = color),     # <- edge color follows fill
  #   width = 0.15, knot.pos = 0.5, alpha = 0.9,
  #   size = 0.5,                            # <- edge thickness
  #   lineend = "round",
  #   show.legend = c(colour = FALSE)        # <- avoid duplicate legend
  # ) +

  geom_alluvium(
    aes(fill = color),
    width = 0.15, knot.pos = 0.5, alpha = 0.9,
    colour = "grey60",   # <- same edge color for all
    size = 0.4, #lineend = "round",
  )+
  theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 23, face = "bold", family = "Times New Roman"),
    legend.title = element_text(size = 23, face = "bold", family = "Times New Roman"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 23, color = "black", face = "bold", family = "Times New Roman"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )+

  geom_stratum(width = 0.15, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 7, fontface = 'bold') +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name   = "ACME Direction",
    values = c("blue" = "blue", "orange" = "orange"),
    labels = c("Negative","Positive"),
    na.translate = FALSE
  ) +


  # geom_alluvium(
  #   aes(fill = color, colour = color),     # <- edge color follows fill
  #   width = 0.15, knot.pos = 0.5, alpha = 0.9,
  #   size = 0.5,                            # <- edge thickness
  #   lineend = "round",
  #   show.legend = c(colour = FALSE)        # <- avoid duplicate legend
  # )+



  scale_colour_manual(values = c("blue" = "blue", "orange" = "orange"),
                      guide = "none") +    # <- no separate edge legend
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 23, color = "black", face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  );p

# 80l0.6
# 60l0.4
# g40l0.1 -2
pngfile <- fs::path(path, 
                    paste0('sankey med res', 
                           "male adj_g60l0.4.png"))
ragg::agg_png(pngfile, width = 120, height = 60, units = "cm", res = 500, scaling = 2)
print(p)
invisible(dev.off())

####

p=ggplot(hoi,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  geom_alluvium(
    aes(fill = color,
        colour = after_scale(colorspace::darken(fill, 0.15))),  # darker edge from fill
    width = 0.15, knot.pos = 0.5, alpha = 0.9,
    size = 0.7, lineend = "round",
    show.legend = c(colour = FALSE)  # keep only the fill legend
  ) +
  geom_stratum(width = 0.15, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 6, fontface = 'bold') +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name   = "ACME Direction",
    values = c("blue" = "blue", "orange" = "orange"),
    labels = c("Negative", "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text  = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 23, color = "black", face = "bold"),
    panel.grid   = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank()
  )
# 60l0.4
# g40l0.1
pngfile <- fs::path(path, 
  paste0('sankey med res', 
         "all nonadj_g40l0.1.png"))
ragg::agg_png(pngfile, width = 120, height = 60, units = "cm", res = 500, scaling = 2)
print(p)
invisible(dev.off())
p

#####












# ===== PACKAGES =====
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggalluvial)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(fs)
  library(ragg)
  # colorspace is used via after_scale() — part of ggplot2 v3.4+, colorspace enhances palettes
  # If you don't have colorspace installed, comment out the after_scale() line or install.packages("colorspace")
})

# ===== PARAMETERS =====
# Input Excel path
excel_path <- "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3623 .xlsx"

# Output directory (make sure it exists)
out_dir <- "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/"

# How many top/bottom links to keep for negative/positive ACME
lkm <- 10

# Plot file name stem
file_stem <- "sankey_med_res_all_nonadj_g40l0.1"

# Font family (Windows: "Times New Roman" usually works; otherwise pick an installed font)
base_family <- "Times New Roman"

# ===== 1) LOAD =====
# all_all <- readxl::read_xlsx(path = excel_path)

# ===== 2) FILTER BY d0.p AND SELECT EXTREMES BY ACME =====
# Ensure the needed columns exist
required_cols <- c("ACME", "d0.p")
missing_cols <- setdiff(required_cols, names(all_all))
if (length(missing_cols)) {
  stop("Missing required columns in the Excel file: ", paste(missing_cols, collapse = ", "))
}

c1 <- as.data.frame(all_all)

# Keep rows with d0.p < 0.1
c1 <- c1 %>% filter(.data[["d0.p"]] < 0.1)

# Defensive: drop rows with missing ACME
c1 <- c1 %>% filter(!is.na(.data[["ACME"]]))

# Split negative/positive and take extremes by ACME
# (Use explicit dplyr:: namespace to avoid desc() masking issues)
neg <- c1 %>%
  dplyr::filter(.data[["ACME"]] < 0) %>%
  dplyr::arrange(.data[["ACME"]]) %>%
  dplyr::slice_head(n = lkm)

pos <- c1 %>%
  dplyr::filter(.data[["ACME"]] > 0) %>%
  dplyr::arrange(dplyr::desc(.data[["ACME"]])) %>%
  dplyr::slice_head(n = lkm)

c1_ext <- dplyr::bind_rows(neg, pos)

# Color by ACME sign
c1_ext <- c1_ext %>%
  mutate(color = ifelse(.data[["ACME"]] < 0, "blue", "orange"))

# ===== 3) PARSE THE FIRST COLUMN INTO THREE PARTS =====
# Assumption: the FIRST column of the sheet contains the composite name.
# If your composite label is in a different column, change [[1]] to the name, e.g. [["name_col"]].
name_col <- names(c1_ext)[1]
names_vec <- as.character(c1_ext[[name_col]])

# Light cleanup pass before splitting
names_vec <- str_trim(names_vec)
names_vec <- gsub("PFHxA_Branched", "PFHxA-B.", names_vec)

# Try underscore-first parsing; if a row doesn't have >= 2 underscores, fall back to space split.
split_underscore <- strsplit(names_vec, "_", fixed = TRUE)

parsed_mat <- vapply(split_underscore, function(x) {
  len <- length(x)
  if (len >= 3) {
    # Contaminant = first token, Steroid = second, Outcome = rest
    c(Contaminants = x[1],
      Steroids     = x[2],
      `Bile Acids or Lipids` = paste(x[3:len], collapse = "_"))
  } else {
    # Fallback: split by spaces if underscores are not present
    xs <- strsplit(paste(x, collapse = "_"), "\\s+")[[1]]
    len2 <- length(xs)
    if (len2 >= 3) {
      c(Contaminants = xs[1],
        Steroids     = xs[2],
        `Bile Acids or Lipids` = paste(xs[3:len2], collapse = " "))
    } else {
      c(Contaminants = NA_character_,
        Steroids     = NA_character_,
        `Bile Acids or Lipids` = NA_character_)
    }
  }
}, FUN.VALUE = c(Contaminants = "", Steroids = "", `Bile Acids or Lipids` = ""))

hoi <- as.data.frame(t(parsed_mat), stringsAsFactors = FALSE)

# Optional label normalizations
hoi$Contaminants <- gsub("PFHxA_Branched", "PFHxA_B.", hoi$Contaminants)
hoi$Steroids     <- gsub("17a\\.OHP5", "17a-OHP5", hoi$Steroids)
hoi$Steroids     <- gsub("17a\\.OHP4", "17a-OHP4", hoi$Steroids)
hoi$Steroids     <- gsub("11\\.KT",    "11-KT",    hoi$Steroids)
hoi$Steroids     <- gsub("11\\.KDHT",  "11-KDHT",  hoi$Steroids)

# Attach ACME & color back; keep only rows that parsed successfully
good <- complete.cases(hoi$Contaminants, hoi$Steroids, hoi$`Bile Acids or Lipids`)
hoi <- hoi[good, , drop = FALSE]
hoi$ACME  <- c1_ext$ACME[good]
hoi$color <- ifelse(hoi$ACME < 0, "blue", "orange")

# Optional: if you want to verify parsing results
# print(head(hoi, 20))

# ===== 4) BUILD THE ALLUVIAL PLOT =====
p <- ggplot(
  hoi,
  aes(
    axis1 = Contaminants,
    axis2 = Steroids,
    axis3 = `Bile Acids or Lipids`,
    y     = abs(ACME),
    fill  = color
  )
) +
  # Flows (no `lineend` — not supported in some ggalluvial versions)
  geom_alluvium(
    aes(
      fill   = color,
      # If colorspace is installed, this creates a darker edge from fill; otherwise comment it out.
      colour = after_scale(colorspace::darken(fill, 0.15))
    ),
    width = 0.15,
    knot.pos = 0.5,
    alpha = 0.9,
    size  = 0.6,
    show.legend = c(colour = FALSE)   # keep only the fill legend
  ) +
  # Strata (blocks)
  geom_stratum(width = 0.15, fill = "white", color = "black") +
  # Stratum labels
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 7, fontface = "bold", family = base_family
  ) +
  scale_x_discrete(
    limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
    expand = c(.05, .05)
  ) +
  scale_fill_manual(
    name   = "ACME Direction",
    values = c("blue" = "blue", "orange" = "orange"),
    labels = c("Negative", "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal(base_family = base_family) +
  theme(
    legend.position   = "right",
    legend.text       = element_text(size = 23, face = "bold", family = base_family),
    legend.title      = element_text(size = 23, face = "bold", family = base_family),
    axis.title.y      = element_blank(),
    axis.text.y       = element_blank(),
    axis.ticks.y      = element_blank(),
    axis.text.x       = element_text(size = 23, color = "black", face = "bold", family = base_family),
    panel.grid        = element_blank(),
    panel.background  = element_blank(),
    plot.background   = element_blank()
  )

# Show interactively
print(p)

# ===== 5) SAVE HIGH-RES PNG =====
if (!dir_exists(out_dir)) dir_create(out_dir)
pngfile <- fs::path(out_dir, paste0(file_stem, ".png"))

ragg::agg_png(
  filename = pngfile,
  width = 120, height = 60, units = "cm",
  res = 500, scaling = 2
)
suppressWarnings(print(p))  # silences harmless "Some strata appear at multiple axes" messages
invisible(dev.off())

message("Saved plot: ", pngfile)


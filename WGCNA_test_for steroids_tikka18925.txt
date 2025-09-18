# ================================
# Libraries
# ================================
suppressPackageStartupMessages({
  library(WGCNA)
  library(dplyr)
  # library(Hmisc)            # rcorr for Spearman p-values
  library(ComplexHeatmap)   # Heatmap plotting
  library(circlize)
  library(grid)})

options(stringsAsFactors = FALSE)
allowWGCNAThreads()

# ================================
# USER CONFIG
# ================================
# FDR / effect-size thresholds
# alpha       <- 0.25   # BH-FDR cutoff for protein × module cells, 0.05 in the beginning
# min_abs_r   <- 0.3   # minimum absolute Spearman rho, recomended was 0.3 more the less metabolites, tested now with 0.05/0.1

# Module filtering (Option A)
min_sig_prot   <- 3   # keep modules with >= this many significant proteins (after protein filter), 5 was recomended 7
top_m_modules  <- 10   # fallback if none pass min_sig_prot: keep top M modules by support, 8 was recomended 13/16

# Plot density control
max_rows <- length(MediatorVariables)        # << requested: keep top 60 proteins by highest mean expression

# WGCNA parameters (metabolite network)
minModuleSize <- 3 #5 was in the beginning 18
mergeCutHeight <- 0.13 #0.25, lower the similar, 0.13
networkType <- "unsigned"  # use "signed" if you want sign-specific modules
deepSplit = 1.5 #1.5

the_best=MediatorVariables#rownames(rs1n_prot)

# ================================
# 0) Prepare input matrices
# ================================
# Inputs expected:
#   df           : samples x features (metabolites + proteins)
#   metab_names  : character vector of metabolite column names in df
#   prot_names   : character vector of protein column names in df

df=AllData
dim(df)
prot_names=MediatorVariables
metab_names=OutcomeVariables

# TreatmentVariables

df=df[,c(prot_names,metab_names)] 

stopifnot(all(metab_names %in% colnames(df)))
stopifnot(all(prot_names  %in% colnames(df)))

df=data.frame(df,check.names = FALSE) #this latter is important.. otherwise:
# heps=c( "X11.KT"  ,  "X17a.OHP5" ,     "X11b.OHA4", "X11.KDHT" , "X11.KA4"   ,  "T.Epi.T" ,  "X17a.OHP4")
# colnames(df)[colnames(df) %in% heps] = c( "11-KT"  ,  "17a-OHP5" ,     "11b-OHA4", "11-KDHT" , "11-KA4"   ,  "T/Epi-T" ,  "17a-OHP4")

datMet0  <- df[, metab_names, drop = FALSE] %>% select(where(is.numeric))
datProt0 <- df[, prot_names,  drop = FALSE] %>% select(where(is.numeric))

# Align samples (rows) across both layers
common_samples <- intersect(rownames(datMet0), rownames(datProt0))
datMet0  <- datMet0[common_samples, , drop = FALSE]
datProt0 <- datProt0[common_samples, , drop = FALSE]

# Drop features with zero variance
nzv_met <- apply(datMet0,  2, function(x) stats::sd(x, na.rm = TRUE) > 0)
nzv_pro <- apply(datProt0, 2, function(x) stats::sd(x, na.rm = TRUE) > 0)
datMet0  <- datMet0[,  nzv_met, drop = FALSE]
datProt0 <- datProt0[, nzv_pro, drop = FALSE]

# ================================
# 1) WGCNA on metabolites -> module eigengenes
# ================================
gsg <- goodSamplesGenes(datMet0, verbose = 3)
datMet <- if (gsg$allOK) datMet0 else datMet0[gsg$goodSamples, gsg$goodGenes, drop = FALSE]

# Re-align proteins to the possibly reduced sample set
datProt <- datProt0[rownames(datMet), , drop = FALSE]

# pick soft threshold
powers <- 1:20
sft <- pickSoftThreshold(datMet, powerVector = powers, networkType = networkType, verbose = 5)
power <- if (!is.na(sft$powerEstimate)) sft$powerEstimate else 6

net <- blockwiseModules(
  datMet,
  power = power,
  networkType = networkType,
  TOMType = networkType,   # "unsigned" or "signed" consistent with networkType
  minModuleSize = minModuleSize,
  reassignThreshold = 0,
  mergeCutHeight = mergeCutHeight,
  numericLabels = TRUE,
  pamRespectsDendro = FALSE,
  saveTOMs = FALSE,
  # corOptions = list(use = "p"),
  # corFnc = "cor",
  verbose = 3)

moduleLabels <- net$colors
moduleColors <- labels2colors(moduleLabels)

# Module eigengenes (MEs): samples x modules
MEs0 <- moduleEigengenes(datMet, colors = moduleColors)$eigengenes
MEs  <- orderMEs(MEs0)

# Rename columns from color names to module_1, module_2, etc.
colnames(MEs) 
colnames(MEs) <- paste0("Module ", seq_len(ncol(MEs)))
colnames(MEs)
rownames(MEs)
# rownames(MEs)=rownames(MEs0)

# Get unique module colors in the same order as MEs
unique_colors <- names(table(moduleColors))[order(as.numeric(factor(names(table(moduleColors)))))]
module_names <- paste0("Module ", seq_along(unique_colors))

# Create a mapping from color to "Module X"
color_to_module <- setNames(module_names, unique_colors)

# Assign each metabolite to its module name
metab_to_module <- color_to_module[moduleColors]

# Assign a color to each module
module_colors <- RColorBrewer::brewer.pal(length(module_names), "Set3")
names(module_colors) <- module_names


# ================================
# 2) Proteins × MEs: Spearman correlations and FDR
# ================================
stopifnot(identical(rownames(datProt), rownames(MEs)))

# Correlations (proteins as rows, MEs as columns)
cor_PM <- cor(datProt, MEs, use = "pairwise.complete.obs", method = "spearman")

# p-values via rcorr on the combined matrix, then extract cross-block p
get_cross_pvals <- function(A, B, type = "spearman") {
  # A, B are samples x variables
  mat <- cbind(A, B)
  rc  <- rcorr(as.matrix(mat), type = type)
  p   <- rc$P
  p[seq_len(ncol(A)), (ncol(A) + 1):ncol(mat), drop = FALSE]}

library(Hmisc)


p_PM <- get_cross_pvals(datProt, MEs, type = "spearman")

# Global BH across all protein×module tests
p_adj_PM <- matrix(
  p.adjust(as.vector(p_PM), method = "BH"),
  nrow = nrow(p_PM), ncol = ncol(p_PM),
  dimnames = dimnames(p_PM))

# ================================
# 3) Protein-level FDR filtering (any significant ME with |rho| >= min_abs_r)
# ================================
sig_mask_all <- (p_adj_PM < alpha) & (abs(cor_PM) >= min_abs_r)
sel_proteins <- rownames(cor_PM)[rowSums(sig_mask_all, na.rm = TRUE) > 0]

# Fallback if none pass: take top 60 by mean expression (requested behavior)
expr_mean_all <- colMeans(datProt, na.rm = TRUE)   # mean expression per protein across samples

if (length(sel_proteins) == 0) {
  message("No proteins pass FDR<", alpha, " & |rho|>=", min_abs_r,
          ". Falling back to top ", max_rows, " by mean expression.")
  
  expr_mean_all <- expr_mean_all[names(expr_mean_all) %in% rownames(cor_PM)]
  sel_proteins <- names(sort(expr_mean_all, decreasing = TRUE))[1:min(max_rows, length(expr_mean_all))]}

# Subset to selected proteins
# cor_sel   <- cor_PM[sel_proteins, , drop = FALSE]
# p_adj_sel <- p_adj_PM[sel_proteins, , drop = FALSE]

# Force inclusion of the_best proteins
sel_proteins <- unique(c(sel_proteins, the_best))
sel_proteins <- intersect(sel_proteins, rownames(cor_PM))  # Ensure they exist

# Subset to selected proteins
cor_sel   <- cor_PM[sel_proteins, , drop = FALSE]
p_adj_sel <- p_adj_PM[sel_proteins, , drop = FALSE]


# ================================
# 4) Module-level FDR filtering (Option A on the selected proteins)
#    Keep modules with >= min_sig_prot significant proteins
#    else fallback to top_m_modules by support
# ================================
sig_mask_sel <- (p_adj_sel < alpha) & (abs(cor_sel) >= min_abs_r)
sig_count <- colSums(sig_mask_sel, na.rm = TRUE)

keep_modules <- names(sig_count)[sig_count >= min_sig_prot]

if (length(keep_modules) == 0) {
  message("No modules have ≥ ", min_sig_prot, " significant proteins. ",
          "Keeping top ", top_m_modules, " modules by support.")
  # rank modules by support, then by stronger effects, then smaller FDR
  max_abs_r_by_module <- apply(abs(cor_sel), 2, max, na.rm = TRUE)
  min_fdr_by_module   <- apply(p_adj_sel, 2, min, na.rm = TRUE)
  rk <- order(-sig_count, -max_abs_r_by_module, min_fdr_by_module)
  keep_modules <- colnames(cor_sel)[head(rk, min(top_m_modules, ncol(cor_sel)))]
}

# Subset to kept modules
cor_sel   <- cor_sel[, keep_modules, drop = FALSE]
p_adj_sel <- p_adj_sel[, keep_modules, drop = FALSE]

# ================================
# 5) Re-prune proteins by FDR within kept modules,
#    then keep the TOP 60 by highest mean expression (requested)
# ================================
sig_mask_final <- (p_adj_sel < alpha) & (abs(cor_sel) >= min_abs_r)
final_proteins <- rownames(cor_sel)[rowSums(sig_mask_final, na.rm = TRUE) > 0]
length(final_proteins)

# If none remain significant, use top 60 by mean expression among all currently select-able proteins
if (length(final_proteins) == 0) {
  message("After module filter, no proteins remain significant. ",
          "Showing top ", max_rows, " by mean expression among selectable proteins.")
  pool <- intersect(colnames(datProt), rownames(cor_sel))
  expr_pool <- expr_mean_all[pool]
  final_proteins <- names(sort(expr_pool, decreasing = TRUE))[1:min(max_rows, length(expr_pool))]
} else {
  # Keep top 60 by mean expression among the FDR-passing proteins
  pool <- intersect(final_proteins, names(expr_mean_all))
  expr_pool <- expr_mean_all[pool]
  if (length(pool) > max_rows) {
    final_proteins <- names(sort(expr_pool, decreasing = TRUE))[1:max_rows]
  } else {
    final_proteins <- names(sort(expr_pool, decreasing = TRUE))
  }
}


final_proteins <- unique(c(rownames(cor_sel), the_best))
final_proteins <- intersect(final_proteins, rownames(cor_sel))  # Ensure they exist in cor_sel

cor_mat_small <- cor_sel[final_proteins, , drop = FALSE]
p_adj_small   <- p_adj_sel[final_proteins, , drop = FALSE]

sum(rownames(cor_mat_small) %in% the_best)
dim(cor_mat_small)
length(the_best)


# Build final matrices
# cor_mat_small <- cor_sel[final_proteins, , drop = FALSE]
# p_adj_small   <- p_adj_sel[final_proteins, , drop = FALSE]

# ================================
# 6) Stars from FDR for Heatmap overlay
# ================================
stars_small <- matrix("", nrow = nrow(p_adj_small), ncol = ncol(p_adj_small),
                      dimnames = dimnames(p_adj_small))
stars_small[p_adj_small < 0.05]  <- "*"
stars_small[p_adj_small < 0.01]  <- "**"
stars_small[p_adj_small < 0.001] <- "***"
# Remove stars for very small |rho|
stars_small[abs(cor_mat_small) < min_abs_r] <- ""

# ================================
# 7) Plot with ComplexHeatmap::Heatmap
# ================================
# cor_mat_small_=cor_mat_small
# p_adj_small_=p_adj_small
# stars_small_=stars_small

# cor_mat_small=cor_mat_small_
# p_adj_small=p_adj_small_
# stars_small=stars_small_

# After you have renamed columns to module_1, module_2, etc.
# Convert them to "Module 1", "Module 2", etc.
# colnames(cor_mat_small) <- gsub("^Module (\\d+)$", "Module \\1", colnames(cor_mat_small))
# colnames(p_adj_small)   <- gsub("^Module (\\d+)$", "Module \\1", colnames(p_adj_small))
# colnames(stars_small)   <- gsub("^Module (\\d+)$", "Module \\1", colnames(stars_small))

module_names_ok=colnames(cor_mat_small) 

module_names <- paste0("Module ", seq_len(ncol(cor_mat_small)))
colnames(cor_mat_small) <- module_names
colnames(p_adj_small)   <- module_names
colnames(stars_small)   <- module_names

# 
# cor_mat_small=cor_mat_small[rownames(cor_mat_small) %in% the_best,]
# p_adj_small=p_adj_small[rownames(p_adj_small) %in% the_best,]
# stars_small=stars_small[rownames(stars_small) %in% the_best,]

# How many "extra" non-best proteins you want
N_EXTRA <- 30

# 1) Expression means per protein: datProt is samples x proteins
expr_mean_all <- colMeans(datProt, na.rm = TRUE)

# 2) Universe of available proteins in your current correlation matrix
avail <- rownames(cor_mat_small)

# 3) 'the_best' that actually exist in the matrix
keep_best <- intersect(the_best, avail)

# 4) Candidate pool for "top expressing other than the_best"
cand <- setdiff(intersect(names(expr_mean_all), avail), keep_best)

# 5) Top-N by expression from the candidates
top_other <- head(names(sort(expr_mean_all[cand], decreasing = TRUE)), N_EXTRA)

# 6) Final rows to keep (best first, then the others)
keep <- unique(c(keep_best, top_other))

if (length(keep) == 0) {
  stop("No overlap with 'the_best' and no candidates found for top expression.")
}

# 7) Subset matrices consistently
cor_mat_small <- cor_mat_small[keep, , drop = FALSE]
p_adj_small   <- p_adj_small  [keep, , drop = FALSE]
stars_small   <- stars_small  [keep, , drop = FALSE]



# 2) Assign consistent colors


library(ComplexHeatmap)
library(grid)
library(RColorBrewer)
library(stringr)

# --- assume 'cor_mat_small', 'stars_small', 'col_fun' already defined ---
# --- and columns already renamed to module_# as in previous steps ---

# 1) Module names + colors
module_names <- stringr::str_sort(unique(colnames(cor_mat_small)), numeric = TRUE)

# install.packages("Polychrome")
library(Polychrome)

module_colors <- setNames(
  createPalette(length(module_names), seedcolors = c("#FF0000", "#00FF00", "#0000FF")),
  module_names)


# module_colors <- setNames(
#   colorRampPalette(brewer.pal(8, "Set2"))(length(module_names)),
#   module_names
# )


# 2) Top annotation (thin strip, legend settings)
# Tune these to control legend compactness:
N_PER_ROW <- 10  # max items per legend row (adjust to your taste)
nrow_lgd  <- max(1, ceiling(length(module_names) / N_PER_ROW))


# module_colors <- module_colors[!is.na(names(module_colors))]


top_anno <- HeatmapAnnotation(
  Metabolites = factor(colnames(cor_mat_small), levels = module_names),
  col = list(Metabolites = module_colors),
  
  # Make the color strip thin
  simple_anno_size = unit(3, "mm"),
  
  # Hide the tiny label above the strip (you already have a left "Metabolites" header)
  annotation_name_gp = gpar(col = NA),
  
  # Compact, multi-row legend at top
  annotation_legend_param = list(
    Metabolites = list(
      title = "Metabolite Classes",
      legend_direction = "horizontal",
      nrow = nrow_lgd,
      by_row = TRUE,
      grid_width  = unit(3, "mm"),
      grid_height = unit(3, "mm"),
      labels_gp   = gpar(fontsize = 8)
    )
  )
)


# Example: blue for negative, white for zero, red for positive


extremes <- data.frame(
  Module = colnames(cor_mat_small),
  Max_cor = apply(cor_mat_small, 2, max, na.rm = TRUE),
  Min_cor = apply(cor_mat_small, 2, min, na.rm = TRUE),
  Max_pval = apply(p_adj_small, 2, max, na.rm = TRUE),
  Min_pval = apply(p_adj_small, 2, min, na.rm = TRUE)
)

oj=max(extremes[,2])
oj2=min(extremes[,3])
ui=round(max(c(abs(oj),abs(oj2))),1)

col_fun <- colorRamp2(c(-ui, 0, ui), c("blue", "white", "red"))

# cor_mat_smalle=cor_mat_small[rownames(cor_mat_small) %in% the_best,]

# colnames(cor_mat_small) <- paste0("Metabolite Class ", seq_len(ncol(cor_mat_small)))
# colnames(p_adj_small)   <- colnames(cor_mat_small)
# colnames(stars_small)   <- colnames(cor_mat_small)
# 
# module_names <- colnames(cor_mat_small)
# module_colors <- setNames(
#   createPalette(length(module_names), seedcolors = c("#FF0000", "#00FF00", "#0000FF")),
#   module_names
# )

library(ComplexHeatmap)
library(Polychrome)
library(circlize)
library(grid)

# 1. Clustered column order
ht_temp <- Heatmap(cor_mat_small, cluster_columns = TRUE)
clustered_order <- column_order(ht_temp)

# 2. Rename columns based on clustered order
new_names <- paste0("Metabolite Class ", seq_along(clustered_order))
colnames(cor_mat_small)[clustered_order] <- new_names
colnames(p_adj_small) <- colnames(cor_mat_small)
colnames(stars_small) <- colnames(cor_mat_small)

# 3. Module names and colors
module_names <- colnames(cor_mat_small)
module_colors <- setNames(
  createPalette(length(module_names), seedcolors = c("#FF0000", "#00FF00", "#0000FF")),
  module_names)

# 4. Top annotation
top_anno <- HeatmapAnnotation(
  Metabolites = factor(colnames(cor_mat_small), levels = module_names[order(as.numeric(gsub("[^0-9]", "", module_names)))]),
  col = list(Metabolites = module_colors),
  simple_anno_size = unit(3, "mm"),
  annotation_name_gp = gpar(col = NA),
  annotation_legend_param = list(
    Metabolites = list(
      title = "Metabolite Classes",
      legend_direction = "vertical",
      ncol = 1,  # one column for top-to-bottom order
      grid_width  = unit(4, "mm"),
      grid_height = unit(4, "mm"),
      labels_gp   = gpar(fontsize = 10)
    )
  )
)


# 5. Color function for correlation
extremes <- data.frame(
  Module = colnames(cor_mat_small),
  Max_cor = apply(cor_mat_small, 2, max, na.rm = TRUE),
  Min_cor = apply(cor_mat_small, 2, min, na.rm = TRUE)
)
ui <- round(max(abs(extremes$Max_cor), abs(extremes$Min_cor)), 1)
col_fun <- colorRamp2(c(-ui, 0, ui), c("blue", "white", "red"))

# 6. Heatmap
ht <- Heatmap(
  cor_mat_small,
  name = "Spearman r",
  col = col_fun,
  top_annotation = top_anno,
  heatmap_legend_param = list(
    at = seq(-ui, ui, by = 0.2),
    labels = sprintf("%.1f", seq(-ui, ui, by = 0.2)),  # rounded labels,
    title = "Correlation"
  ),
  show_row_names   = TRUE,
  show_column_names = TRUE,
  cluster_rows     = TRUE,
  cluster_columns  = TRUE,
  show_row_dend    = FALSE,
  show_column_dend = FALSE,
  column_names_gp = gpar(fontsize = 10, fontface = "bold"),
  row_title = "Proteins",
  row_title_gp = gpar(fontsize = 12, fontface = "bold"),
  row_title_side = "left",
  row_names_gp = gpar(fontsize = 8.6),
  rect_gp = gpar(col = "grey90"),
  na_col = "#F2F2F2",
  cell_fun = function(j, i, x, y, width, height, fill) {
    s <- stars_small[i, j]
    if (nzchar(s)) grid.text(s, x, y, gp = gpar(fontsize = 10, col = "black"))
  }
)

# 7. Left-side label
left_label <- rowAnnotation(
  Metabolites = anno_empty(border = FALSE),
  annotation_name_side = "top",
  annotation_name_rot  = 90,
  annotation_name_gp   = gpar(fontsize = 12, fontface = "bold"),
  width = unit(5, "mm")
)

# 8. Combine and draw
ht_list <- left_label + ht

draw(
  ht_list,
  heatmap_legend_side = "right",
  annotation_legend_side = "right",
  merge_legends = FALSE
)


## ================================================================
## Resequence "Metabolite Class <n>" -> "Metabolite Class 1..K"
## and export metabolite-to-class Excel (using moduleLabels)
## ================================================================

# install.packages(c("ComplexHeatmap", "writexl"))
library(ComplexHeatmap)
library(writexl)

## ---- Inputs expected in the environment ----
# cor_mat_small : matrix [features x classes]
# p_adj_small   : matrix same dims as cor_mat_small
# stars_small   : matrix same dims as cor_mat_small
# moduleLabels  : named numeric vector; names = metabolite IDs; 0 = grey
stopifnot(exists("cor_mat_small"), is.matrix(cor_mat_small))
stopifnot(exists("p_adj_small"),   is.matrix(p_adj_small))
stopifnot(exists("stars_small"),   is.matrix(stars_small))
stopifnot(ncol(cor_mat_small) == ncol(p_adj_small),
          ncol(cor_mat_small) == ncol(stars_small))

stopifnot(exists("moduleLabels"))
stopifnot(is.numeric(moduleLabels) || is.integer(moduleLabels))
stopifnot(!is.null(names(moduleLabels)))  # must be named by metabolite IDs

## ---- Helper ----
as_int <- function(x) suppressWarnings(as.integer(x))

## ================================================================
## 1) Build OLD -> NEW class mapping
##    OLD are your current colnames, e.g., "Metabolite Class 4"
##    NEW will be "Metabolite Class 1..K" in chosen order
## ================================================================

old_names <- colnames(cor_mat_small)
stopifnot(!is.null(old_names))

# Extract the numeric tag from the current names
old_label_num <- as_int(gsub("\\D", "", old_names))
if (any(is.na(old_label_num))) {
  stop("Could not extract numeric labels from current column names. Expected names like 'Metabolite Class 4'.")
}

# Choose order: "cluster" (recommended) or "current"
ORDER_BY <- "cluster"  # change to "current" to keep the current left-to-right order

if (ORDER_BY == "cluster") {
  ht_tmp <- Heatmap(cor_mat_small, cluster_columns = TRUE)
  ord <- unlist(column_order(ht_tmp))
  old_in_order <- old_names[ord]
} else if (ORDER_BY == "current") {
  old_in_order <- old_names
} else {
  stop("ORDER_BY must be 'cluster' or 'current'.")
}

# New sequential class names following the chosen order
new_in_order <- paste0("Metabolite Class ", seq_along(old_in_order))

# Map old -> new
old_to_new <- setNames(new_in_order, old_in_order)

# Apply mapping to all matrices
colnames(cor_mat_small) <- unname(old_to_new[colnames(cor_mat_small)])
colnames(p_adj_small)   <- colnames(cor_mat_small)
colnames(stars_small)   <- colnames(cor_mat_small)

# Mapping table (also keep the numeric tag we extracted from old names)
map_df <- data.frame(
  Original_Name  = old_in_order,
  Original_Label = as_int(gsub("\\D", "", old_in_order)),
  New_Name       = new_in_order,
  stringsAsFactors = FALSE
)




## ================================================================
## 2) Metabolites per NEW class (via moduleLabels)
##    Route: module label number -> OLD name -> NEW name
## ================================================================
# Convert moduleLabels to integer and name by metabolite
lab_vec <- as_int(moduleLabels)
names(lab_vec) <- names(moduleLabels)

# Split metabolites by module label (including 0)
met_by_label <- split(names(lab_vec), as.character(lab_vec))

# Rename "0" to "0 (Unassigned)" for clarity
# names(met_by_label)[names(met_by_label) == "0"] <- "0 (Unassigned)"

# Add mapping for module 0 to map_df
map_df <- rbind(
  data.frame(
    Original_Name  = "Unassigned",
    Original_Label = 0,
    New_Name       = "Metabolite Class 0",
    stringsAsFactors = FALSE
  ),
  map_df
)


# Extend new_in_order to include class 0
new_in_order <- c("Metabolite Class 0", new_in_order)

# Map: label number -> OLD name (now includes "0")
label_to_old <- setNames(map_df$Original_Name, as.character(map_df$Original_Label))

# Keep only labels present in the heatmap columns OR module 0
keep_labels <- intersect(names(met_by_label), names(label_to_old))

# Build list keyed by OLD names
met_by_old <- met_by_label[match(keep_labels, names(met_by_label))]
names(met_by_old) <- label_to_old[keep_labels]

# Convert OLD names to NEW names
old_to_new_full <- setNames(map_df$New_Name, map_df$Original_Name)
met_by_new <- met_by_old
names(met_by_new) <- unname(old_to_new_full[names(met_by_old)])

# Order classes as in the heatmap (plus "Metabolite Class 0")
met_by_new <- met_by_new[new_in_order[new_in_order %in% names(met_by_new)]]


## ================================================================
## 3) Build export tables
## ================================================================

# Long table
metabolite_df <- do.call(
  rbind,
  lapply(names(met_by_new), function(cls) {
    if (length(met_by_new[[cls]]) == 0L) return(NULL)
    data.frame(
      Class      = cls,
      Metabolite = met_by_new[[cls]],
      stringsAsFactors = FALSE
    )
  })
)
if (is.null(metabolite_df)) {
  metabolite_df <- data.frame(Class = character(), Metabolite = character(), stringsAsFactors = FALSE)
} else {
  metabolite_df$Class <- factor(metabolite_df$Class, levels = new_in_order, ordered = TRUE)
}

# Classes sheet: include every class even if count=0
counts <- setNames(integer(length(new_in_order)), new_in_order)
if (length(met_by_new)) counts[names(met_by_new)] <- lengths(met_by_new)

# Align Original_Name/Label to the new order
orig_by_new <- setNames(map_df$Original_Name, map_df$New_Name)
origlab_by_new <- setNames(map_df$Original_Label, map_df$New_Name)

classes_tbl <- data.frame(
  Class           = new_in_order,
  Index           = as_int(gsub("\\D", "", new_in_order)),
  Original_Name   = unname(orig_by_new[new_in_order]),
  Original_Label  = as_int(unname(origlab_by_new[new_in_order])),
  Count           = as.integer(counts[new_in_order]),
  stringsAsFactors = FALSE
)

## ================================================================
## 4) Write Excel workbook
## ================================================================
out_file <- "metabolites_by_classes_steroids4.xlsx"
write_xlsx(list(
  "Metabolites by Class" = metabolite_df,
  "Classes"              = classes_tbl
), path = out_file)

message("✅ Wrote: ", normalizePath(out_file))

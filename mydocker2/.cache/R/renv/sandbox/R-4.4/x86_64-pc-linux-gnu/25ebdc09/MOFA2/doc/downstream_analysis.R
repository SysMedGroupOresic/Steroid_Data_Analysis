## ----message=FALSE------------------------------------------------------------
library(ggplot2)
library(MOFA2)

## -----------------------------------------------------------------------------
filepath <- system.file("extdata", "model.hdf5", package = "MOFA2")
model <- load_model(filepath)

## -----------------------------------------------------------------------------
plot_data_overview(model)

## -----------------------------------------------------------------------------
Nsamples = sum(get_dimensions(model)[["N"]])

sample_metadata <- data.frame(
  sample = samples_names(model)[[1]],
  condition = sample(c("A","B"), size = Nsamples, replace = TRUE),
  age = sample(1:100, size = Nsamples, replace = TRUE)
)

samples_metadata(model) <- sample_metadata
head(samples_metadata(model), n=3)

## -----------------------------------------------------------------------------
# Total variance explained per view
head(get_variance_explained(model)$r2_total[[1]])

# Variance explained for every factor in per view
head(get_variance_explained(model)$r2_per_factor[[1]])

## -----------------------------------------------------------------------------
plot_variance_explained(model, x="view", y="factor")

## -----------------------------------------------------------------------------
plot_variance_explained(model, x="view", y="factor", plot_total = TRUE)[[2]]

## -----------------------------------------------------------------------------
plot_factor(model, 
  factor = 1:3,
  color_by = "age",
  shape_by = "condition"
)

## -----------------------------------------------------------------------------
p <- plot_factor(model, 
  factors = c(1,2,3),
  color_by = "condition",
  dot_size = 3,        # change dot size
  dodge = TRUE,           # dodge points with different colors
  legend = FALSE,          # remove legend
  add_violin = TRUE,      # add violin plots,
  violin_alpha = 0.25  # transparency of violin plots
)

# The output of plot_factor is a ggplot2 object that we can edit
p <- p + 
  scale_color_manual(values=c("A"="black", "B"="red")) +
  scale_fill_manual(values=c("A"="black", "B"="red"))

print(p)

## ----message=FALSE------------------------------------------------------------
plot_factors(model, 
  factors = 1:3,
  color_by = "condition"
)

## -----------------------------------------------------------------------------
plot_weights(model,
  view = "view_0",
  factor = 1,
  nfeatures = 10,     # Number of features to highlight
  scale = TRUE,          # Scale weights from -1 to 1
  abs = FALSE             # Take the absolute value?
)

## -----------------------------------------------------------------------------
plot_top_weights(model,
  view = "view_0",
  factor = 1,
  nfeatures = 10
)

## -----------------------------------------------------------------------------
plot_data_heatmap(model,
  view = "view_1",         # view of interest
  factor = 1,             # factor of interest
  features = 20,          # number of features to plot (they are selected by weight)
  
  # extra arguments that are passed to the `pheatmap` function
  cluster_rows = TRUE, cluster_cols = FALSE,
  show_rownames = TRUE, show_colnames = FALSE
)

## -----------------------------------------------------------------------------
plot_data_scatter(model,
  view = "view_1",         # view of interest
  factor = 1,             # factor of interest
  features = 5,           # number of features to plot (they are selected by weight)
  add_lm = TRUE,          # add linear regression
  color_by = "condition"
)

## -----------------------------------------------------------------------------
set.seed(42)
model <- run_umap(model)
model <- run_tsne(model)

## -----------------------------------------------------------------------------
plot_dimred(model,
  method = "TSNE",  # method can be either "TSNE" or "UMAP"
  color_by = "condition",
  dot_size = 5
)

## -----------------------------------------------------------------------------
views_names(model) <- c("Transcriptomics", "Proteomics")
factors_names(model) <- paste("Factor", 1:get_dimensions(model)$K, sep=" ")

## -----------------------------------------------------------------------------
views_names(model)

## -----------------------------------------------------------------------------
# "factors" is a list of matrices, one matrix per group with dimensions (nsamples, nfactors)
factors <- get_factors(model, factors = "all")
lapply(factors,dim)

## -----------------------------------------------------------------------------
# "weights" is a list of matrices, one matrix per view with dimensions (nfeatures, nfactors)
weights <- get_weights(model, views = "all", factors = "all")
lapply(weights,dim)

## -----------------------------------------------------------------------------
# "data" is a nested list of matrices, one matrix per view and group with dimensions (nfeatures, nsamples)
data <- get_data(model)
lapply(data, function(x) lapply(x, dim))[[1]]

## -----------------------------------------------------------------------------
factors <- get_factors(model, as.data.frame = TRUE)
head(factors, n=3)

## -----------------------------------------------------------------------------
weights <- get_weights(model, as.data.frame = TRUE)
head(weights, n=3)

## -----------------------------------------------------------------------------
data <- get_data(model, as.data.frame = TRUE)
head(data, n=3)

## -----------------------------------------------------------------------------
sessionInfo()


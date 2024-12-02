## ----message=FALSE, warning=FALSE---------------------------------------------
library(MOFA2)
library(tidyverse)
library(pheatmap)

## -----------------------------------------------------------------------------
set.seed(2020)

# set number of samples and time points
N <- 200
time <- seq(0,1,length.out = N)

# generate example data
dd <- make_example_data(sample_cov = time, n_samples = N,
                        n_factors = 4, n_features = 200, n_views = 4,
                        lscales = c(0.5, 0.2, 0, 0))
# input data
data <- dd$data

# covariate matrix with samples in columns
time <- dd$sample_cov
rownames(time) <- "time"

## -----------------------------------------------------------------------------
df <- data.frame(dd$Z, t(time))
df <- gather(df, key = "factor", value = "value", starts_with("simulated_factor"))
ggplot(df, aes(x = time, y = value)) + geom_point() + facet_grid(~factor)

## -----------------------------------------------------------------------------
sm <- create_mofa(data = dd$data)

## ----message=FALSE, warning=FALSE---------------------------------------------
sm <- set_covariates(sm, covariates = time)
sm

## ----message=FALSE, warning=FALSE---------------------------------------------
data_opts <- get_default_data_options(sm)

model_opts <- get_default_model_options(sm)
model_opts$num_factors <- 4

train_opts <- get_default_training_options(sm)
train_opts$maxiter <- 100

mefisto_opts <- get_default_mefisto_options(sm)

sm <- prepare_mofa(sm, model_options = model_opts,
                   mefisto_options = mefisto_opts,
                   training_options = train_opts,
                   data_options = data_opts)

## ----warning=FALSE, message=FALSE---------------------------------------------
outfile = file.path(tempdir(),"model.hdf5")
sm <- run_mofa(sm, outfile, use_basilisk = TRUE)

## ----fig.width=5, fig.height=4------------------------------------------------
plot_variance_explained(sm)
r <- plot_factor_cor(sm)

## -----------------------------------------------------------------------------
get_scales(sm)

## -----------------------------------------------------------------------------
plot_factors_vs_cov(sm, color_by = "time")

## -----------------------------------------------------------------------------
df <- plot_factors_vs_cov(sm, color_by = "time",
                    legend = FALSE, return_data = TRUE)
head(df)

## ----fig.width=5, fig.height=4------------------------------------------------
plot_weights(sm, factors = 4, view = 1)
plot_top_weights(sm, factors = 3, view = 2)

## -----------------------------------------------------------------------------
plot_data_vs_cov(sm, factor=3,
                         features = 2,
                         color_by = "time",
                         dot_size = 1)

## -----------------------------------------------------------------------------
sm <- interpolate_factors(sm, new_values = seq(0,1.1,0.01))
plot_interpolation_vs_covariate(sm, covariate = "time",
                                factors = "Factor3")

## -----------------------------------------------------------------------------
sessionInfo()


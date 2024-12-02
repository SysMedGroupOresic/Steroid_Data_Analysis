## ----message=FALSE------------------------------------------------------------
library(data.table)
library(MOFA2)

## -----------------------------------------------------------------------------
data <- make_example_data(
  n_views = 2, 
  n_samples = 200, 
  n_features = 1000, 
  n_factors = 10
)[[1]]

lapply(data,dim)

## ----message=FALSE------------------------------------------------------------
MOFAobject <- create_mofa(data)

## -----------------------------------------------------------------------------
plot_data_overview(MOFAobject)

## ----message=FALSE------------------------------------------------------------
N = ncol(data[[1]])
groups = c(rep("A",N/2), rep("B",N/2))

MOFAobject <- create_mofa(data, groups=groups)

## -----------------------------------------------------------------------------
plot_data_overview(MOFAobject)

## -----------------------------------------------------------------------------
filepath <- system.file("extdata", "test_data.RData", package = "MOFA2")
load(filepath)

head(dt)

## -----------------------------------------------------------------------------
MOFAobject <- create_mofa(dt)
print(MOFAobject)

## ----out.width = "80%"--------------------------------------------------------
plot_data_overview(MOFAobject)

## -----------------------------------------------------------------------------
data_opts <- get_default_data_options(MOFAobject)
head(data_opts)

## -----------------------------------------------------------------------------
model_opts <- get_default_model_options(MOFAobject)
model_opts$num_factors <- 10
head(model_opts)

## -----------------------------------------------------------------------------
train_opts <- get_default_training_options(MOFAobject)
head(train_opts)

## ----message=FALSE------------------------------------------------------------
MOFAobject <- prepare_mofa(
  object = MOFAobject,
  data_options = data_opts,
  model_options = model_opts,
  training_options = train_opts
)

## -----------------------------------------------------------------------------
outfile = file.path(tempdir(),"model.hdf5")
MOFAobject.trained <- run_mofa(MOFAobject, outfile, use_basilisk=TRUE)

## -----------------------------------------------------------------------------
sessionInfo()


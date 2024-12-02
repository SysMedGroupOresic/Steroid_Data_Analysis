## ----echo=FALSE, results="hide"-----------------------------------------------
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)
library(BiocStyle)

## -----------------------------------------------------------------------------
library(basilisk)
my_env <- BasiliskEnvironment(envname="my_env_name",
    pkgname="ClientPackage",
    packages=c("pandas==1.4.3", "scikit-learn==1.1.1")
)

second_env <- BasiliskEnvironment(envname="second_env_name",
    pkgname="ClientPackage",
    packages=c("scipy=1.9.1", "numpy==1.22.1") 
)

## ----eval=FALSE---------------------------------------------------------------
#  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

## -----------------------------------------------------------------------------
if (.Platform$OS.type != "windows") {
tmp <- createLocalBasiliskEnv("basilisk-vignette-test",
    packages=c("scikit-learn=1.1.1", "numpy=1.22.1"))
}

## ----error=FALSE, message=FALSE-----------------------------------------------
if (.Platform$OS.type != "windows") {
x <- matrix(rnorm(1000), ncol=10)
basiliskRun(env=tmp, fun=function(mat) {
    module <- reticulate::import("sklearn.decomposition")
    runner <- module$TruncatedSVD()
    output <- runner$fit(mat) 
    output$singular_values_
}, mat = x, testload="scipy.optimize")
}

## ----error=FALSE, message=FALSE-----------------------------------------------
if (.Platform$OS.type != "windows") {
library(reticulate)

# In this case, we'll use reticulate directly to construct our conda
# environment; though we'll cheat a little and use basilisk's conda
# installation, otherwise reticulate will try to install its own miniconda.
tmp2 <- file.path(getwd(), "basilisk-vignette-test2")
if (!file.exists(tmp2)) {
    conda.bin <- file.path(
        basilisk.utils::getCondaDir(), 
        basilisk.utils::getCondaBinary()
    )
    conda_install(tmp2, 
        packages=c("scipy==1.9.1"), 
        python_version="3.10", 
        channels="conda-forge",
        additional_create_args="--override-channels",
        additional_install_args="--override-channels",
        conda=conda.bin
    )
}

basiliskRun(env=tmp2, fun=function(mat) {
    module <- reticulate::import("scipy.stats")
    norm <- module$norm
    norm$cdf(c(-1, 0, 1))
}, mat = x, testload="scipy.optimize")
}

## -----------------------------------------------------------------------------
sessionInfo()


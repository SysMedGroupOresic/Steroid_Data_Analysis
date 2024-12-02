## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Biocpkg("alabaster.sce")
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(SingleCellExperiment)
mat <- matrix(rpois(10000, 10), ncol=10)
colnames(mat) <- letters[1:10]
rownames(mat) <- sprintf("GENE_%i", seq_len(nrow(mat)))

sce <- SingleCellExperiment(list(counts=mat))
sce$stuff <- LETTERS[1:10]
sce$blah <- runif(10)
reducedDims(sce) <- list(
 PCA=matrix(rnorm(ncol(sce)*10), ncol=10),
 TSNE=matrix(rnorm(ncol(sce)*2), ncol=2)
)
altExps(sce) <- list(spikes=SummarizedExperiment(list(counts=mat[1:2,])))
sce

library(alabaster.sce)
tmp <- tempfile()
saveObject(sce, tmp)

list.files(tmp, recursive=TRUE)

## -----------------------------------------------------------------------------
roundtrip <- readObject(tmp)
class(roundtrip)

## -----------------------------------------------------------------------------
sessionInfo()


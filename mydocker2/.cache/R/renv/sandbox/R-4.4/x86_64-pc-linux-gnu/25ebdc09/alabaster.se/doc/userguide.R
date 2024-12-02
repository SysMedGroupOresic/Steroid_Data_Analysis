## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Biocpkg("alabaster.se")
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
# Example taken from ?SummarizedExperiment
library(SummarizedExperiment)
nrows <- 200
ncols <- 6
counts <- matrix(rpois(nrows * ncols, 10), nrows, ncols)
rowRanges <- GRanges(
    rep(c("chr1", "chr2"), c(50, 150)),
    IRanges(floor(runif(200, 1e5, 1e6)), width=100),
    strand=sample(c("+", "-"), 200, TRUE)
)
colData <- DataFrame(
    Treatment=rep(c("ChIP", "Input"), 3), 
    row.names=LETTERS[1:6]
)
rse <- SummarizedExperiment(
    assays=SimpleList(counts=counts),
    rowRanges=rowRanges, 
    colData=colData
)
rownames(rse) <- sprintf("GENE_%03d", 1:200)
rse

library(alabaster.se)
tmp <- tempfile()
saveObject(rse, tmp)

list.files(tmp, recursive=TRUE)

## -----------------------------------------------------------------------------
roundtrip <- readObject(tmp)
roundtrip

## -----------------------------------------------------------------------------
sessionInfo()


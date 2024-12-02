## ----echo=FALSE, results="hide", message=FALSE--------------------------------
require(knitr)
opts_chunk$set(error=FALSE, message=FALSE, warning=FALSE)
library(BiocStyle)
self <- Biocpkg("BiocNeighbors")
set.seed(99999)

## -----------------------------------------------------------------------------
# Mocking up a matrix with 'nobs' points in the rows
# and 'ndim' dimensions in the columns.
nobs <- 1000
ndim <- 20
data <- matrix(runif(nobs*ndim), ncol=ndim)

library(BiocNeighbors)
fout <- findKNN(data, k=10)
str(fout)

## -----------------------------------------------------------------------------
fout$index[3,]
fout$distance[3,]

## -----------------------------------------------------------------------------
aout <- findKNN(data, k=10, BNPARAM=AnnoyParam())
str(aout)

## -----------------------------------------------------------------------------
vout <- findKNN(data, k=10, BNPARAM=VptreeParam(distance="Manhattan"))
str(vout)

## -----------------------------------------------------------------------------
var.k <- sample(10, nrow(data), replace=TRUE)

# use I() to distinguish between scalar and length-1 vectors.
var.out <- findKNN(data, k=I(var.k))

head(var.out$index)
head(var.out$distance)

## -----------------------------------------------------------------------------
nquery <- 100
ndim <- 20
query <- matrix(runif(nquery*ndim), ncol=ndim)

qout <- queryKNN(data, query, k=5)
str(qout)

## -----------------------------------------------------------------------------
prebuilt <- buildIndex(data)
out1 <- findKNN(prebuilt, k=5)
out2 <- queryKNN(prebuilt, query, k=5)

## -----------------------------------------------------------------------------
nobs <- 8000
ndim <- 20
data <- matrix(runif(nobs*ndim), ncol=ndim)

fout <- findNeighbors(data, threshold=1)
head(fout$index)
head(fout$distance)

## -----------------------------------------------------------------------------
fout$index[[3]]
fout$distance[[3]]

## -----------------------------------------------------------------------------
vparam <- VptreeParam(distance="Manhattan")
vout <- findNeighbors(data, threshold=1, BNPARAM=vparam)

## -----------------------------------------------------------------------------
nquery <- 100
ndim <- 20
query <- matrix(runif(nquery*ndim), ncol=ndim)
qout <- queryNeighbors(data, query, threshold=1)
head(qout$index)

## -----------------------------------------------------------------------------
prebuilt <- buildIndex(data)
out1 <- findNeighbors(prebuilt, threshold=1)
out2 <- queryNeighbors(prebuilt, query,threshold=1)

## -----------------------------------------------------------------------------
num <- findNeighbors(data, threshold=1, get.index=FALSE, get.distance=FALSE)
head(num)

## ----results="hide"-----------------------------------------------------------
system.file("include", "BiocNeighbors.h", package="BiocNeighbors")

## -----------------------------------------------------------------------------
sessionInfo()


## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Githubpkg("ArtifactDB/alabaster.matrix")
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(Matrix)
y <- rsparsematrix(1000, 100, density=0.05)

library(alabaster.matrix)
tmp <- tempfile()
saveObject(y, tmp)

list.files(tmp, recursive=TRUE)

## -----------------------------------------------------------------------------
roundtrip <- readObject(tmp)
class(roundtrip)

## ----eval=FALSE---------------------------------------------------------------
# library(DelayedArray)
# y <- DelayedArray(rsparsematrix(1000, 100, 0.05))
# y <- log1p(abs(y) / 1:100) # adding some delayed ops.
# 
# preserveDelayedOperations(TRUE)
# meta <- stageObject(y, tmp, "delayed")
# .writeMetadata(meta, tmp)
# 
# meta <- acquireMetadata(tmp, "delayed/delayed.h5")
# roundtrip <- loadObject(meta, tmp)
# class(roundtrip)

## -----------------------------------------------------------------------------
sessionInfo()


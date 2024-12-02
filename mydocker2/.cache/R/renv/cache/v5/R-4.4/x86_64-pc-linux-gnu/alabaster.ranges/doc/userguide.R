## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Biocpkg("alabaster.ranges")
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(GenomicRanges)
gr <- GRanges("chrA", IRanges(sample(100), width=sample(100)))
mcols(gr)$score <- runif(length(gr))
metadata(gr)$genome <- "Aaron"
seqlengths(gr) <- c(chrA=1000)

library(alabaster.ranges)
tmp <- tempfile()
saveObject(gr, tmp)

list.files(tmp, recursive=TRUE)

## -----------------------------------------------------------------------------
roundtrip <- readObject(tmp)
roundtrip

## -----------------------------------------------------------------------------
metadata(roundtrip)
mcols(roundtrip)
seqinfo(roundtrip)

## -----------------------------------------------------------------------------
sessionInfo()


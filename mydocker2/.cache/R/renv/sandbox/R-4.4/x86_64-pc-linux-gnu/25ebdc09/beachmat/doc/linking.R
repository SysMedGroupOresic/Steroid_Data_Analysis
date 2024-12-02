## ----echo=FALSE, results="hide", message=FALSE--------------------------------
require(knitr)
opts_chunk$set(error=FALSE, message=FALSE, warning=FALSE)

## -----------------------------------------------------------------------------
# Mocking up some kind of matrix-like object.
library(Matrix)
x <- round(rsparsematrix(1000, 10, 0.2))

# Initializing it in C++.
library(beachmat)
ptr <- initializeCpp(x)

## ----eval=FALSE---------------------------------------------------------------
#  browseURL(system.file("include", "Rtatami.h", package="beachmat"))

## -----------------------------------------------------------------------------
column_sums(ptr)

## -----------------------------------------------------------------------------
parallel_column_sums(ptr, 2)

## -----------------------------------------------------------------------------
sessionInfo()


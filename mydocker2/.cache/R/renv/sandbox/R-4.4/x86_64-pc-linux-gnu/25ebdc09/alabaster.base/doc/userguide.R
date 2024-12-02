## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Biocpkg("alabaster.base");
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(S4Vectors)
df <- DataFrame(X=1:10, Y=letters[1:10])
df

## -----------------------------------------------------------------------------
tmp <- tempfile()
library(alabaster.base)
saveObject(df, tmp)

## -----------------------------------------------------------------------------
readObject(tmp)

## -----------------------------------------------------------------------------
tmp <- tempfile()
saveObject(df, tmp)
list.files(tmp, recursive=TRUE)

## -----------------------------------------------------------------------------
readObject(tmp)

## -----------------------------------------------------------------------------
validateObject(tmp)

## -----------------------------------------------------------------------------
tmp <- tempfile()
saveObject(df, tmp)

tmp2 <- tempfile()
file.rename(tmp, tmp2)
readObject(tmp2)

## -----------------------------------------------------------------------------
# Creating a nested DF to be a little spicy:
df2 <- DataFrame(Z=factor(1:5), AA=I(DataFrame(B=runif(5), C=rnorm(5))))
tmp <- tempfile()
meta2 <- saveObject(df2, tmp)

# Now reading in the nested DF:
list.files(tmp, recursive=TRUE)
readObject(file.path(tmp, "other_columns/1"))

## -----------------------------------------------------------------------------
library(Matrix)
setMethod("saveObject", "dgTMatrix", function(x, path, ...) {
    # Create a directory to stash our contents.
    dir.create(path)

    # Saving a DataFrame with the triplet data.
    df <- DataFrame(i = x@i, j = x@j, x = x@x)
    write.csv(df, file.path(path, "matrix.csv"), row.names=FALSE)

    # Adding some more information.
    write(dim(x), file=file.path(path, "dimensions.txt"), ncol=1)

    # Creating an object file.
    saveObjectFile(path, "triplet_sparse_matrix")
})

## -----------------------------------------------------------------------------
readSparseTripletMatrix <- function(path, metadata, ...) {
    df <- read.table(file.path(path, "matrix.csv"), header=TRUE, sep=",")
    dims <- readLines(file.path(path, "dimensions.txt"))
    sparseMatrix(
         i=df$i + 1L, 
         j=df$j + 1L, 
         x=df$x, 
         dims=as.integer(dims),
         repr="T"
    )
}
registerReadObjectFunction("triplet_sparse_matrix", readSparseTripletMatrix)

validateSparseTripletMatrix <- function(path, metadata) {
    df <- read.table(file.path(path, "matrix.csv"), header=TRUE, sep=",")
    dims <- as.integer(readLines(file.path(path, "dimensions.txt")))
    stopifnot(is.integer(df$i), all(df$i >= 0 & df$i < dims[1]))
    stopifnot(is.integer(df$j), all(df$j >= 0 & df$j < dims[2]))
    stopifnot(is.numeric(df$x))
}
registerValidateObjectFunction("triplet_sparse_matrix", validateSparseTripletMatrix)

## -----------------------------------------------------------------------------
x <- sparseMatrix(
    i=c(1,2,3,5,6), 
    j=c(3,6,1,3,8), 
    x=runif(5), 
    dims=c(10, 10), 
    repr="T"
)
x

tmp <- tempfile()
saveObject(x, tmp)
list.files(tmp, recursive=TRUE)
readObject(tmp)

## -----------------------------------------------------------------------------
setGeneric("appSaveObject", function(x, path, ...) {
    ans <- standardGeneric("appSaveObject")

    # File names with leading underscores are reserved for application-specific
    # use, so they won't clash with anything produced by saveObject.
    metapath <- file.path(path, "_metadata.json")
    write(jsonlite::toJSON(ans, auto_unbox=TRUE), file=metapath)
})

setMethod("appSaveObject", "ANY", function(x, path, ...) {
    saveObject(x, path, ...) # does the real work
    list(authors=I(Sys.info()[["user"]])) # adds the desired metadata
})

# We can specialize the behavior for specific classes like DataFrames.
setMethod("appSaveObject", "DFrame", function(x, path, ...) {
    ans <- callNextMethod()
    ans$columns <- I(colnames(x))
    ans
})

## -----------------------------------------------------------------------------
# Create a friendly user-visible function to handle the generic override; this
# is reversed on function exit to avoid interfering with other applications. 
saveForApplication <- function(x, path, ...) { 
    old <- altSaveObjectFunction(appSaveObject)
    on.exit(altSaveObjectFunction(old)) 
    altSaveObject(x, path, ...)
}

# Saving our mocked up DataFrame with our overrides active.
df2 <- DataFrame(Z=factor(1:5), AA=I(DataFrame(B=runif(5), C=rnorm(5))))
tmp <- tempfile()
saveForApplication(df2, tmp)

# Both the parent and child DataFrames have new metadata.
cat(readLines(file.path(tmp, "_metadata.json")), sep="\n")
cat(readLines(file.path(tmp, "other_columns/1/_metadata.json")), sep="\n")

## -----------------------------------------------------------------------------
# Defining the override for altReadObject().
appReadObject <- function(path, metadata=NULL, ...) {
    if (is.null(metadata)) {
        metadata <- readObjectFile(path)
    }

    # Print custom message based on the type and application-specific metadata.
    appmeta <- jsonlite::fromJSON(file.path(path, "_metadata.json"))
    cat("I am a ", metadata$type, " created by ", appmeta$authors[1], ".\n", sep="")
    if (metadata$type == "data_frame") {
        all.cols <- paste(appmeta$columns, collapse=", ")
        cat("I have the following columns: ", all.cols, ".\n", sep="")
    }

    readObject(path, metadata=metadata, ...)
}

# Creating a user-friendly function to set the override before the read.
readForApplication <- function(path, metadata=NULL, ...) {
    old <- altReadObjectFunction(appReadObject)
    on.exit(altReadObjectFunction(old))
    altReadObject(path, metadata, ...)
}

# This diverts to the override with printing of custom messages.
readForApplication(tmp)

## -----------------------------------------------------------------------------
sessionInfo()


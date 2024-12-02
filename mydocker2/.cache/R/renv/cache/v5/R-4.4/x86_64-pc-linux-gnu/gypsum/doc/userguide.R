## ----echo=FALSE---------------------------------------------------------------
library(BiocStyle)
self <- Biocpkg("gypsum")
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(gypsum)
listAssets("test-R")
listVersions("test-R", "basic")
listFiles("test-R", "basic", "v1")

out <- saveFile("test-R", "basic", "v1", "blah.txt")
readLines(out)

dir <- saveVersion("test-R", "basic", "v1")
list.files(dir, all.files=TRUE, recursive=TRUE)

## -----------------------------------------------------------------------------
fetchManifest("test-R", "basic", "v1")
fetchSummary("test-R", "basic", "v1")

## -----------------------------------------------------------------------------
fetchLatest("test-R", "basic")

## -----------------------------------------------------------------------------
tmp <- tempfile()
dir.create(tmp)
write(file=file.path(tmp, "foo"), letters)
write(file=file.path(tmp, "bar"), LETTERS)
write(file=file.path(tmp, "whee"), 1:10)

## -----------------------------------------------------------------------------
dest <- tempfile()
cloneVersion("test-R", "basic", "v1", destination=dest)

# Do some modifications in 'dest' to create a new version, e.g., add a file.
# However, users should treat symlinks as read-only - so if you want to modify
# a file, instead delete the symlink and replace it with a new file.
write(file=file.path(dest, "BFFs"), c("Aaron", "Jayaram"))

to.upload <- prepareDirectoryUpload(dest)
to.upload

## -----------------------------------------------------------------------------
fetchPermissions("test-R")

## -----------------------------------------------------------------------------
fetchQuota("test-R")

## -----------------------------------------------------------------------------
fetchUsage("test-R")

## -----------------------------------------------------------------------------
schema <- fetchMetadataSchema()
cat(head(readLines(schema)), sep="\n")

## -----------------------------------------------------------------------------
metadata <- list(                           
    title="Fatherhood",
    description="Luke ich bin dein Vater.",
    sources=list(
       list(provider="GEO", id="GSE12345")
    ),
    taxonomy_id=list("9606"),
    genome=list("GRCm38"),
    maintainer_name="Darth Vader",
    maintainer_email="vader@empire.gov",
    bioconductor_version="3.10"
)

validateMetadata(metadata, schema)

## -----------------------------------------------------------------------------
sessionInfo()


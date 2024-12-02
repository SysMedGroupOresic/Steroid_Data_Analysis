## ----style, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)

## -----------------------------------------------------------------------------
library(scRNAseq)
all.ds <- surveyDatasets()
all.ds

## -----------------------------------------------------------------------------
# Find all datasets involving pancreas.
searchDatasets("pancreas")[,c("name", "title")]

# Find all mm10 datasets involving pancreas or neurons.
searchDatasets(
   defineTextQuery("GRCm38", field="genome") &
   (defineTextQuery("neuro%", partial=TRUE) | 
    defineTextQuery("pancrea%", partial=TRUE))
)[,c("name", "title")]

## -----------------------------------------------------------------------------
sce <- fetchDataset("zeisel-brain-2015", "2023-12-14")
sce

## -----------------------------------------------------------------------------
sce <- fetchDataset("baron-pancreas-2016", "2023-12-14", path="human")
sce

## -----------------------------------------------------------------------------
assay(sce)
sce <- fetchDataset("baron-pancreas-2016", "2023-12-14", path="human", realize.assays=TRUE)
class(assay(sce))

## -----------------------------------------------------------------------------
str(fetchMetadata("zeisel-brain-2015", "2023-12-14"))

## -----------------------------------------------------------------------------
library(SingleCellExperiment)
sce <- SingleCellExperiment(list(counts=matrix(rpois(1000, lambda=1), 100, 10)))
rownames(sce) <- sprintf("GENE_%i", seq_len(nrow(sce)))
colnames(sce) <- head(LETTERS, 10)

## -----------------------------------------------------------------------------
meta <- list(
    title="My dataset",
    description="This is my dataset",
    taxonomy_id="10090",
    genome="GRCh38",
    sources=list(
        list(provider="GEO", id="GSE12345"),
        list(provider="PubMed", id="1234567")
    ),
    maintainer_name="Chihaya Kisaragi",
    maintainer_email="kisaragi.chihaya@765pro.com"
)

## -----------------------------------------------------------------------------
# Simple case: you only have one dataset to upload.
staging <- tempfile()
saveDataset(sce, staging, meta)
list.files(staging, recursive=TRUE)

# Complex case: you have multiple datasets to upload.
staging <- tempfile()
dir.create(staging)
saveDataset(sce, file.path(staging, "foo"), meta)
saveDataset(sce, file.path(staging, "bar"), meta) # etc.

## -----------------------------------------------------------------------------
alabaster.base::readObject(file.path(staging, "foo"))

## ----eval=FALSE---------------------------------------------------------------
#  gypsum::uploadDirectory(staging, "scRNAseq", "my_dataset_name", "my_version")

## ----eval=FALSE---------------------------------------------------------------
#  fetchDataset("my_dataset_name", "my_version")

## ----eval=FALSE---------------------------------------------------------------
#  gypsum::rejectProbation("scRNAseq", "my_dataset_name", "my_version")

## -----------------------------------------------------------------------------
sessionInfo()


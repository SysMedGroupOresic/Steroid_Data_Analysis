## ----style, echo=FALSE--------------------------------------------------------
library(BiocStyle)
knitr::opts_chunk$set(error=FALSE, warning=FALSE, message=FALSE)
self <- Biocpkg("assorthead")

## ----results="asis", echo=FALSE-----------------------------------------------
fname <- system.file("manifest.csv", package='assorthead')
manifest <- read.csv(fname)

cat("|Name|Version|Description|\n")
cat("|----|-------|-----------|\n")
for (i in seq_len(nrow(manifest))) {
    cat(sprintf("|[**%s**](%s)|%s|%s|\n", manifest$name[i], manifest$url[i], manifest$version[i], manifest$description[i]))
}


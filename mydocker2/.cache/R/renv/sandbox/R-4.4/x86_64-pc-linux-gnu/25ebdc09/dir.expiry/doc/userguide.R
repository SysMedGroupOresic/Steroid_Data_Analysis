## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(error=FALSE, message=FALSE, warnings=FALSE)
library(BiocStyle)

## -----------------------------------------------------------------------------
cache.path <- tempfile(pattern="expired_demo")
dir.create(cache.path)

version <- package_version("1.0.0")
version.dir <- file.path(cache.path, version)
dir.create(version.dir)

## -----------------------------------------------------------------------------
version.dir

## -----------------------------------------------------------------------------
library(dir.expiry)
touchDirectory(version.dir)

## -----------------------------------------------------------------------------
list.files(cache.path)
cat(readLines(file.path(cache.path, "1.0.0_dir.expiry")), sep="\n")

## -----------------------------------------------------------------------------
v <- package_version("1.0.0")
v.dir <- file.path(cache.path, v)

lock <- lockDirectory(v.dir)
# on.exit(unlockDirectory(lock)), in a function context.

# Do stuff with the versioned cache 'v.dir' here...
dir.create(v.dir, showWarnings=FALSE)

# Finally, touch the directory on successful completion.
touchDirectory(v.dir)

## -----------------------------------------------------------------------------
unlockDirectory(lock) 

## -----------------------------------------------------------------------------
cache.path <- tempfile(pattern="expired_demo")

old.version <- package_version("0.99.0")
old.version.dir <- file.path(cache.path, old.version)

lck <- lockDirectory(old.version.dir)
dir.create(old.version.dir)
touchDirectory(old.version.dir, date=Sys.Date() - 100)
unlockDirectory(lck, clear=FALSE)

list.files(cache.path)

## -----------------------------------------------------------------------------
new.version <- package_version("1.0.0")
new.version.dir <- file.path(cache.path, new.version)

lck <- lockDirectory(new.version.dir)
dir.create(new.version.dir)
touchDirectory(new.version.dir)
unlockDirectory(lck)

list.files(cache.path)

## -----------------------------------------------------------------------------
new.version2 <- package_version("1.0.1")
new.version.dir2 <- file.path(cache.path, new.version2)

# Newer version but earlier access.
lck2 <- lockDirectory(new.version.dir2)
dir.create(new.version.dir2)
touchDirectory(new.version.dir2, date=Sys.Date() - 100) 
unlockDirectory(lck2)

# Re-accessing the older version.
lck <- lockDirectory(new.version.dir)
touchDirectory(new.version.dir)
unlockDirectory(lck)

list.files(cache.path)

## -----------------------------------------------------------------------------
sessionInfo()


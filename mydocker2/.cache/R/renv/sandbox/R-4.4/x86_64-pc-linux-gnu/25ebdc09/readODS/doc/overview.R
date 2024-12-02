## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(readODS)

## ----write_ods1---------------------------------------------------------------
write_ods(PlantGrowth, "plant.ods")

## ----read_ods1----------------------------------------------------------------
read_ods("plant.ods")

## ----write_ods_append---------------------------------------------------------
write_ods(mtcars, "plant.ods", sheet = "mtcars_ods", append = TRUE)

## ----read_ods_mtcars----------------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods")

## ----read_ods_mtcars2---------------------------------------------------------
read_ods("plant.ods", sheet = 2)

## ----write_ods_update---------------------------------------------------------
write_ods(mtcars, "plant.ods", sheet = "mtcars_ods", update = TRUE, row_names = TRUE)

## ----read_ods_mtcars3---------------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods")

## ----read_ods_mtcars_range----------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods", range = "A1:C10")

## ----writelist----------------------------------------------------------------
write_ods(list("iris" = iris, "plant" = PlantGrowth), "plant_multi.ods")
read_ods("plant_multi.ods", sheet = "plant")

## ----read fods, eval = file.exists("plant.fods")------------------------------
#  read_fods("plant.fods")

## ----write_fods---------------------------------------------------------------
write_ods(PlantGrowth, "plant.fods")

## ----list_ods_sheets----------------------------------------------------------
list_ods_sheets("plant.ods")

## ----empty1-------------------------------------------------------------------
PlantGrowth2 <- tibble::as_tibble(PlantGrowth)
PlantGrowth2[1,1] <- NA
PlantGrowth2$group <- as.character(PlantGrowth2$group)

## NA is preseved; weight is still <dbl>
read_ods(write_ods(PlantGrowth2))

## ----empty2-------------------------------------------------------------------
## NA is preseved; but weight is now <chr>
read_ods(write_ods(PlantGrowth2, na_as_string = TRUE))

## ----empty3-------------------------------------------------------------------
## NA is preseved; but weight is now <chr>
read_ods(write_ods(PlantGrowth2, na_as_string = TRUE),
         col_types = readr::cols(weight = readr::col_double()))

## ----list_ods_sheets20--------------------------------------------------------
## ods_sheets("plant.ods")
list_ods_sheets("plant.ods")

## ----getnum20-----------------------------------------------------------------
##get_num_sheets_in_ods("plant.ods")
length(list_ods_sheets("plant.ods"))

## ----readdotods17-------------------------------------------------------------
## read.ods from 1.6 to 1.8
read_ods("plant.ods", col_names = FALSE, skip = 0, na = NULL, col_types = NA, as_tibble = FALSE)

## ----readotods16--------------------------------------------------------------
## read.ods older than 1.6
lapply(list_ods_sheets("plant.ods"),
       function(x) read_ods(path = "plant.ods", sheet = x, col_names = FALSE, skip = 0, na = NULL, col_types = NA, as_tibble = FALSE))

## ----echo = FALSE, message = FALSE--------------------------------------------
unlink("plant.ods")
unlink("plant.fods")
unlink("plant_multi.ods")


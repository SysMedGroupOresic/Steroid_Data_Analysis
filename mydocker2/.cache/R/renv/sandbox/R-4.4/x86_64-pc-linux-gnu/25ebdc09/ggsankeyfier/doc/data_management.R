## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(ggsankeyfier)

## ----pivot, echo=TRUE---------------------------------------------------------
## first get a data set with a wide format:
data(ecosystem_services)

## pivot to long format for plotting:
es_long <-
  pivot_stages_longer(
    ## the data.frame we wish to pivot:
    data        = ecosystem_services,
    ## the columns that represent the stages:
    stages_from = c("activity_type", "pressure_cat",
                    "biotic_realm", "service_division"),
    ## the column that represents the size of the flows:
    values_from = "RCSES"
  )

## ----pivot_names, echo=TRUE, include=TRUE-------------------------------------
names(es_long)


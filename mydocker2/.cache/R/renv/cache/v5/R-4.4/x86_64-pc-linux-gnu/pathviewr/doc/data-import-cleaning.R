## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package_loading, message=FALSE, warning=FALSE----------------------------
## If you do not already have pathviewr installed:
# install.packages("devtools")
# devtools::install_github("ropensci/pathviewr")

library(pathviewr)
library(ggplot2)
library(magrittr)

## ----import_motive------------------------------------------------------------
## Import the Motive example data included in 
## the package

motive_data <-
  read_motive_csv(
    system.file("extdata", "pathviewr_motive_example_data.csv",
                package = 'pathviewr')
  )

## This produces a tibble 
motive_data

## ----metadata-----------------------------------------------------------------
## E.g. to see the header of the original file:
attr(motive_data, "header")

## Names of all marked objects:
attr(motive_data, "subject_names_simple")

## Types of data included
attr(motive_data, "data_types_simple")

## Frame rate
attr(motive_data, "frame_rate")

## ----import_flydra------------------------------------------------------------
## Import the Flydra example data included in 
## the package
flydra_data <-
  read_flydra_mat(
    system.file("extdata", 
                "pathviewr_flydra_example_data.mat",
                package = 'pathviewr'),
    subject_name = "birdie_wooster"
  )

## Similarly, this produces a tibble with important 
## metadata as attributes
flydra_data

attr(flydra_data, "frame_rate")

## ----as_viewr-----------------------------------------------------------------
## Create a dummy data frame with simulated (nonsense) data
df <-
  data.frame(
    frame = seq(1, 100, by = 1),
    time_sec = seq(0, by = 0.01, length.out = 100),
    subject = "birdie_sanders",
    z = rnorm(100),
    x = rnorm(100),
    y = rnorm(100)
  )

## Use as_viewr() to convert it into a viewr object
test <-
  as_viewr(
    df,
    frame_rate = 100,
    frame_col = 1,
    time_col = 2,
    subject_col = 3,
    position_length_col = 5,
    position_width_col = 6,
    position_height_col = 4
  )

## Some metadata are stored as attributes
attr(test, "frame_rate")

## ----relabel_axes-------------------------------------------------------------
motive_relabeled <-
  motive_data %>%
  relabel_viewr_axes(
    tunnel_length = "_z",
    tunnel_width = "_x",
    tunnel_height = "_y",
    real = "_w"
  )

names(motive_relabeled)

## ----gather_and_trim----------------------------------------------------------
## First gather and show the new column names
motive_gathered <-
  motive_relabeled %>%
  gather_tunnel_data()

names(motive_gathered)

## Now trim, using ranges we know to safely include data
## and exclude artifacts. Anything outside these ranges 
## will be removed.
motive_trimmed <-
  motive_gathered %>%
  trim_tunnel_outliers(
    lengths_min = 0,
    lengths_max = 3,
    widths_min = -0.4,
    widths_max = 0.8,
    heights_min = -0.2,
    heights_max = 0.5
  )

## ----rotate_example-----------------------------------------------------------
## Rotate and center the motive data set:
motive_rotated <-
  motive_trimmed %>% 
  rotate_tunnel(
    perch1_len_min = -0.06,
    perch1_len_max = 0.06,
    perch2_len_min = 2.48,
    perch2_len_max = 2.6,
    perch1_wid_min = 0.09,
    perch1_wid_max = 0.31,
    perch2_wid_min = 0.13,
    perch2_wid_max = 0.35
  )

## ----rotate_example_plots-----------------------------------------------------
## Quick (base-R) plot of the original data
plot(motive_trimmed$position_length,
     motive_trimmed$position_width,
     asp = 1)

## Quick (base-R) plot of the rotated data
plot(motive_rotated$position_length,
     motive_rotated$position_width,
     asp = 1)

## ----redefine_tunnel_example--------------------------------------------------
## Re-center the Flydra data set:
flydra_centered <-
  flydra_data %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44)

## ----redefine_example_plots---------------------------------------------------
## Quick (base-R) plot of the original data
plot(flydra_data$position_length,
     flydra_data$position_height,
     asp = 1)

## Quick (base-R) plot of the redefined data
plot(flydra_centered$position_length,
     flydra_centered$position_height,
     asp = 1)

## ----select_x_examples--------------------------------------------------------
## Motive data: select the middle 50% of the tunnel as the region of interest
motive_selected <-
  motive_rotated %>% select_x_percent(50)

## Quick plot:
plot(motive_selected$position_length,
     motive_selected$position_width,
     asp = 1)

## Flydra data:
flydra_selected <-
  flydra_centered %>% select_x_percent(50)

## Quick plot:
plot(flydra_selected$position_length,
     flydra_selected$position_width,
     asp = 1)

## ----separate_examples--------------------------------------------------------
motive_labeled <-
  motive_selected %>% 
  separate_trajectories(max_frame_gap = "autodetect")

flydra_labeled <-
  flydra_selected %>% 
  separate_trajectories(max_frame_gap = 1)

## ----get_full_examples--------------------------------------------------------
## Motive
motive_full <-
  motive_labeled %>% 
  get_full_trajectories(span = 0.95)

plot(motive_full$position_length,
     motive_full$position_width,
     asp = 1, col = as.factor(motive_full$file_sub_traj))

## Flydra
flydra_full <-
  flydra_labeled %>% 
  get_full_trajectories(span = 0.95)

plot(flydra_full$position_length,
     flydra_full$position_width,
     asp = 1, col = as.factor(flydra_full$file_sub_traj))

## ----all-in-one---------------------------------------------------------------
motive_allinone <-
  motive_data %>%
  clean_viewr(
    relabel_viewr_axes = TRUE,
    gather_tunnel_data = TRUE,
    trim_tunnel_outliers = TRUE,
    standardization_option = "rotate_tunnel",
    select_x_percent = TRUE,
    desired_percent = 50,
    rename_viewr_characters = FALSE,
    separate_trajectories = TRUE,
    max_frame_gap = "autodetect",
    get_full_trajectories = TRUE,
    span = 0.95
  )

## Quick plot
plot(motive_allinone$position_length,
     motive_allinone$position_width,
     asp = 1, col = as.factor(motive_allinone$file_sub_traj))


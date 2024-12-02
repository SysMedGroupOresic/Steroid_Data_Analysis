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

## Import the example Motive data included in 
## the package
motive_data <-
  read_motive_csv(
    system.file("extdata", "pathviewr_motive_example_data.csv",
                package = 'pathviewr')
  )

## ----cleanup_data-------------------------------------------------------------
motive_cleaned <-
  motive_data %>%
  clean_viewr(
    desired_percent = 70,
    rename_viewr_characters = FALSE,
    separate_trajectories = FALSE,
    get_full_trajectories = FALSE
  )

## Quick plot
plot(motive_cleaned$position_length,
     motive_cleaned$position_width,
     asp = 1)

## ----max_frame_gap_1----------------------------------------------------------
motive_mfg1 <- 
  motive_cleaned %>% 
  separate_trajectories(
    max_frame_gap = 1
  )

## Quick plot
plot(motive_mfg1$position_length,
     motive_mfg1$position_width,
     asp = 1, col = as.factor(motive_mfg1$file_sub_traj))

## How many trajectories do we end up with?
length(unique(motive_mfg1$file_sub_traj))

## ----max_frame_gap_5----------------------------------------------------------
motive_mfg5 <- 
  motive_cleaned %>% 
  separate_trajectories(
    max_frame_gap = 5
  )

## Quick plot
plot(motive_mfg5$position_length,
     motive_mfg5$position_width,
     asp = 1, col = as.factor(motive_mfg5$file_sub_traj))

## How many trajectories do we end up with?
length(unique(motive_mfg5$file_sub_traj))

## ----get_full_trajectories----------------------------------------------------
motive_mfg5_full <- 
  motive_mfg5 %>% 
  get_full_trajectories(
    span = .6
  )

## Quick plot
plot(motive_mfg5_full$position_length,
     motive_mfg5_full$position_width,
     asp = 1, col = as.factor(motive_mfg5_full$file_sub_traj))

## How many trajectories do we end up with?
length(unique(motive_mfg5_full$file_sub_traj))

## Plot each trajectory
plot_viewr_trajectories(motive_mfg5_full,
                        plot_axes = c("length", "width"),
                        multi_plot = TRUE)

## ----visualize_frame_gap_choice-----------------------------------------------
motive_cleaned %>% 
  visualize_frame_gap_choice(
    loops = 20
  )

## ----max_frame_gap_auto-------------------------------------------------------
motive_auto <- 
  motive_cleaned %>% 
  separate_trajectories(
    max_frame_gap = "autodetect",
    frame_rate_proportion = 0.1,
    frame_gap_messaging = TRUE,
    frame_gap_plotting = TRUE
  )

## How many trajectories do we end up with?
length(unique(motive_auto$file_sub_traj))


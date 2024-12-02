## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, out.width="100%", fig.cap="Visual angles can be calculated using the size of a visual pattern (`stim_param`) and the distance to the pattern. Larger patterns at shorter distances produce larger visual angles. For dot stimuli, visual angles can be calculated independent of stimulus orientation."----
knitr::include_graphics("https://raw.githubusercontent.com/ropensci/pathviewr/dc728871873b92dc8412bdcb5ef58031457f6788/images/stim_param_angle.jpeg")

## ----package_loading, message=FALSE, warning=FALSE----------------------------
library(pathviewr)
library(ggplot2)
library(magrittr)

## -----------------------------------------------------------------------------
## Import motive data set
motive_data <- # import
  read_motive_csv(
    system.file("extdata", "pathviewr_motive_example_data.csv",
                package = 'pathviewr')
  )

## Clean motive data set
motive_full <-
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

## -----------------------------------------------------------------------------
## Import flydra data set
flydra_data <- 
  read_flydra_mat(
    system.file("extdata", "pathviewr_flydra_example_data.mat",
                package = 'pathviewr'),
    subject_name = "birdie_wooster")

## Clean flydra data set
flydra_full <- 
  flydra_data %>%
  clean_viewr(
    relabel_viewr_axes = FALSE,
    gather_tunnel_data = FALSE,
    trim_tunnel_outliers = FALSE,
    standardization_option = "redefine_tunnel_center",
    length_method = "middle",
    height_method = "user-defined",
    height_zero = 1.44,
    get_velocity = FALSE,
    select_x_percent = TRUE,
    desired_percent = 60,
    rename_viewr_characters = FALSE,
    separate_trajectories = TRUE,
    get_full_trajectories = TRUE
  )

## -----------------------------------------------------------------------------
motive_treatments <- 
  motive_full %>% 
  insert_treatments(tunnel_config = "v",
                    perch_2_vertex = 0.3855,
                    vertex_angle = 90,
                    tunnel_length = 2,
                    stim_param_lat_pos = 0.05,
                    stim_param_lat_neg = 0.05,
                    stim_param_end_pos = 0.1,
                    stim_param_end_neg = 0.1,
                    treatment = "latB")
names(motive_treatments)

## -----------------------------------------------------------------------------
flydra_treatments <-
  flydra_full %>%
  insert_treatments(tunnel_config = "box",
                    tunnel_width = 1,
                    tunnel_length = 3,
                    stim_param_lat_pos = 0.05,
                    stim_param_lat_neg = 0.05,
                    stim_param_end_pos = 0.1,
                    stim_param_end_neg = 0.1,
                    treatment = "latB")

## ----motive_min_dist_pos, fig.height=4, fig.width=7---------------------------
motive_min_dist <- 
  motive_treatments %>% 
  calc_min_dist_v(simplify_output = FALSE)

## Display minimum distances to the positive lateral walls 
## Viewpoint is from the end of the tunnel
motive_min_dist %>% 
  ggplot(aes(x = position_width, y = position_height)) +
  geom_point(aes(color = min_dist_pos), size = 2, shape = 1) +
  coord_fixed() +
  theme_classic() +
  geom_segment(aes(x = 0,         # positive wall
                   y = -0.3855,
                   xend = 0.5869,
                   yend = 0.2014)) +
  geom_segment(aes(x = 0,         # negative wall
                   y = -0.3855,
                   xend = -0.5869,
                   yend = 0.2014))

## ----flydra_min_dist_end, fig.height=4, fig.width=7---------------------------
flydra_min_dist <- 
  flydra_treatments %>% 
  calc_min_dist_box()

## Display minimum distances to the end walls
## Viewpoint is from above the tunnel
flydra_min_dist %>% 
  ggplot(aes(x = position_length, y = position_width)) +
  geom_point(aes(color = min_dist_end), size = 2, shape = 1) +
  coord_fixed() +
  theme_classic() +
  geom_segment(aes(x = -1,         # negative wall
                   y = -0.5,
                   xend = 1,
                   yend = -0.5)) +
  geom_segment(aes(x = -1,         # positive wall
                   y = 0.5,
                   xend = 1,
                   yend = 0.5))

## ----motive_vis_angle_pos, fig.height=4, fig.width=7--------------------------
motive_vis_angle <- 
  motive_min_dist %>% 
  get_vis_angle()

## Visualize the angles produced from stimuli on the positive wall
## Viewpoint is from the end of the tunnel
motive_vis_angle %>% 
  ggplot(aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_pos_deg), size = 2, shape = 1) +
  coord_fixed()+
  theme_classic() +
  geom_segment(aes(x = 0,         # positive wall
                   y = -0.3855,
                   xend = 0.5869,
                   yend = 0.2014)) +
  geom_segment(aes(x = 0,         # negative wall
                   y = -0.3855,
                   xend = -0.5869,
                   yend = 0.2014))

## ----flydra_vis_angle_end, fig.height=4, fig.width=7--------------------------
flydra_vis_angle <- 
  flydra_min_dist %>% 
  get_vis_angle()

## Visualize the angles produced by stimuli on the end walls
## Viewpoint is from above the tunnel
flydra_vis_angle %>% 
  ggplot(aes(x = position_length, y = position_width)) +
  geom_point(aes(color = vis_angle_end_deg), size = 2, shape = 1) +
  coord_fixed() +
  theme_classic() +
  geom_segment(aes(x = -1,        # negative wall
                   y = -0.5,
                   xend = 1,
                   yend = -0.5)) +
  geom_segment(aes(x = -1,        # positive wall
                   y = 0.5,
                   xend = 1,
                   yend = 0.5))

## ----motive_sf_pos, fig.height=4, fig.width=7---------------------------------
motive_sf <- 
  motive_vis_angle %>%
  get_sf()

## Visualize the spatial frequency of the stimulus on the positive wall 
## point is from the end of the tunnel
motive_sf %>% 
  ggplot(aes(x = position_width, y = position_height)) +
  geom_point(aes(color = sf_pos), size = 2, shape = 1) +
  coord_fixed()+
  theme_classic() +
  geom_segment(aes(x = 0,         # positive wall
                   y = -0.3855,
                   xend = 0.5869,
                   yend = 0.2014)) +
  geom_segment(aes(x = 0,         # negative wall
                   y = -0.3855,
                   xend = -0.5869,
                   yend = 0.2014))

## ----flydra_sf_end, fig.height=4, fig.width=7---------------------------------
flydra_sf <- 
  flydra_vis_angle %>% 
  get_sf()

## Visualize the spatial frequency of the stimulus on the end walls
## Viewpoint is from above the tunnel
flydra_sf %>% 
  ggplot(aes(x = position_length, y = position_width)) +
  geom_point(aes(color = sf_end), size = 2, shape = 1) +
  coord_fixed() +
  theme_classic() +
  geom_segment(aes(x = -1,        # negative wall
                   y = -0.5,
                   xend = 1,
                   yend = -0.5)) +
  geom_segment(aes(x = -1,        # positive wall
                   y = 0.5,
                   xend = 1,
                   yend = 0.5))


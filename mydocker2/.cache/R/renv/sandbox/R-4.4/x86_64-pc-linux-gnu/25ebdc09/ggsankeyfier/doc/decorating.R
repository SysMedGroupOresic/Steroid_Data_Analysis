## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.ext = "png",
  dev = "png"
)

## ----aesthetics, fig.width=6.5, fig.height=4.5--------------------------------
library(ggsankeyfier)
library(ggplot2)
theme_set(theme_light())
data("ecosystem_services")

## Let's subset the example data to create a less cluttered
## Sankey diagram
es_sub <-
  ecosystem_services |>
  subset(RCSES > 0.005) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
                      "RCSES", "service_section")

ggplot(
  data    = es_sub,
  mapping = aes(x = stage, y = RCSES, group = node,
    edge_id = edge_id, connector = connector, colour = stage)) +
  ## apply fill and alpha aesthetic only to edges (not the nodes)
  geom_sankeyedge(aes(alpha = RCSES, fill = service_section)) +
  geom_sankeynode() +
  guides(fill   = guide_legend(ncol = 1),
         alpha  = guide_legend(ncol = 1),
         colour = guide_legend(ncol = 1)) +
  theme(legend.position = "top")

## ----deco_baseplot------------------------------------------------------------
pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")

p <-
  ggplot(
  data    = es_sub,
  mapping = aes(x = stage, y = RCSES, group = node,
    edge_id = edge_id, connector = connector))

## ----layer_segment, fig.width=6, fig.height=3---------------------------------
p +
  geom_sankeynode(position = pos) +
  geom_segment(aes(col = service_section),
               position = pos, stat = "sankeyedge",
               arrow = arrow(length = unit(0.2, "cm")))

## ----layer_bar, fig.width=6, fig.height=3-------------------------------------
p +
  geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_bar(position = pos, stat = "sankeynode")

## ----curve_shape, fig.width=6, fig.height=3-----------------------------------
p +
  geom_sankeyedge(slope = 1, position = pos, mapping = aes(fill = service_section)) +
  geom_sankeynode(position = pos)

## ----curve_waist, warning=FALSE, fig.width=6, fig.height=3--------------------
p +
  geom_sankeyedge(aes(waist = RCSES, fill = service_section), position = pos) +
  geom_sankeynode(position = pos)

## ----legend_keys, fig.width=6, fig.height=3-----------------------------------
p +
  geom_sankeyedge(aes(waist = RCSES, fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  scale_waist_binned(guide = "legend") +
  scale_fill_binned(guide = "legend")

## ----theme_void, fig.width=6, fig.height=3------------------------------------
p +
  geom_sankeyedge(aes(fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  theme_void()

## ----colour_turbo, fig.width=6, fig.height=3----------------------------------
p +
  geom_sankeyedge(aes(fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  scale_fill_viridis_c(option = "turbo")


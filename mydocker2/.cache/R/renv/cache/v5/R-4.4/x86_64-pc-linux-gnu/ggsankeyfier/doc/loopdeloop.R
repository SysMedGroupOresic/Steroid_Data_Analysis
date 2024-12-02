## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.ext = "png",
  dev = "png"
)

## ----setup--------------------------------------------------------------------
library(ggsankeyfier)
library(ggplot2)
theme_set(theme_light())
set.seed(0)

pos <- position_sankey(v_space = "auto")

## Let's subset the data, to make the plot less cluttered:
es_subset <- pivot_stages_longer(
  subset(ecosystem_services, RCSES > 0.02),
  c("activity_realm", "biotic_realm", "service_section"),
  "RCSES")

# And prepare a basis for the plot
p <-
  ggplot(es_subset, aes(x = stage, y = RCSES, group = node,
                        connector = connector, edge_id = edge_id)) +
  geom_sankeynode(position = pos) +
  geom_sankeyedge(position = pos)

## ----back---------------------------------------------------------------------
es_subset_feedback <-
  es_subset |>
  rbind(
    data.frame(
      RCSES     = 0.05,
      edge_id   = max(es_subset$edge_id) + 1,
      connector = c("from", "to"),
      node      = c("Cultural", "Fish & Cephalopods"),
      stage     = c("service_section", "biotic_realm")
    )
  )

p %+% es_subset_feedback

## ----selfref------------------------------------------------------------------
es_subset_selfref <-
  es_subset |>
  rbind(
    data.frame(
      RCSES     = 0.05,
      edge_id   = max(es_subset$edge_id) + 1,
      connector = c("from", "to"),
      node      = c("Cultural", "Cultural"),
      stage     = c("service_section", "service_section")
    )
  )

p %+% es_subset_selfref


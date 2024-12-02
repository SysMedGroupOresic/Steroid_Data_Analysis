## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.ext = "png",
  dev = "png"
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(ggsankeyfier)
theme_set(theme_light())
data("ecosystem_services")

## ----pos_params1--------------------------------------------------------------
pos <- position_sankey(split_nodes = TRUE, align = "top",
                       width = 0.2, v_space = 0.15, h_space = 0.25)
serv_plot <-
  ggplot(ecosystem_services_pivot1,
         aes(x = stage, y = RCSES, group = node, connector = connector,
             edge_id = edge_id)) +
  geom_sankeyedge(position = pos) +
  geom_sankeynode(position = pos)

## ----pos_params2, results='hide', message=FALSE, echo=FALSE, fig.width=6, fig.height=3----
library(dplyr)

comp <-
  StatSankeynode$compute_panel(
    data = ecosystem_services_pivot1 |>
      rename(y = "RCSES", x = "stage", group = "node") |> mutate(PANEL = 1, x = as.numeric(x)),
    params = c(na.rm = T, pos$setup_params()))
comp <- pos$compute_layer(data = comp) |>
  mutate(node_top = y + node_size/2, node_bottom = y - node_size/2)

w <- 0.2
h_sp <- 0.25
v_sp <- 0.15

v_dat <- comp |> filter(x < 1.5 & node_order %in% 1:2)
h_dat <- comp |> filter(x > 1.5 & x < 2.5 & node_order %in% 2:3)
w_dat <- comp |> filter(x > 2.5 & node_order == 1)

serv_plot +
  geom_segment(data = data.frame(
    x    = c(v_dat$x[1],           min(h_dat$x),  w_dat$x - w/4),
    xend = c(v_dat$x[1],           max(h_dat$x),  w_dat$x + w/4),
    y    = c(v_dat$node_top[2],    mean(h_dat$y), w_dat$node_bottom - diff(range(comp$y))*.05),
    yend = c(v_dat$node_bottom[1], mean(h_dat$y), w_dat$node_bottom - diff(range(comp$y))*.05)),
    arrow = arrow(length = grid::unit(.1, "cm"), ends = "both"),
    inherit.aes = FALSE,
    aes(x = x, y = y, xend = xend, yend = yend), col = "red") +
  geom_text(data = data.frame(
    x = c(v_dat$x[1] - 0.3, mean(h_dat$x), w_dat$x[1]),
    y = c(mean(v_dat$y),
          mean(h_dat$y) + diff(range(comp$y))*.16,
          w_dat$node_bottom - diff(range(comp$y))*.1),
    label = c("v_space", "h_space", "width/2")
  ),
  inherit.aes = FALSE,
  aes(x = x, y = y, label = label), col = "red", angle = c(0, 90, 0), vjust = 0.5, hjust = 0.5)

## ----pos_align, results='hide', echo=FALSE, fig.width=6, fig.height=3.5-------
pos <- position_sankey(split_nodes = FALSE, align = "justify", width = 0.1,
                       v_space = v_sp, h_space = 0)
top <- max(comp$node_top)

plt <-
  ggplot(ecosystem_services_pivot1,
         aes(x = stage, y = RCSES, group = node, connector = connector,
             edge_id = edge_id),) +
  geom_sankeynode(position = pos) +
  geom_sankeyedge(position = pos)

plt_just <-
  plt +
  geom_hline(yintercept = c(0, top), lty = 2, col = "red")

plt_just

## ----split, results='hide', fig.width=6, fig.height=3-------------------------
es_sub <- ecosystem_services_pivot2 |> subset(RCSES > quantile(RCSES, 0.9))
ggplot(es_sub,
       aes(x = stage, y = RCSES, group = node, connector = connector, edge_id = edge_id)) +
  geom_sankeyedge(aes(fill = service_section)) +
  geom_sankeynode()

## ----stack_order--------------------------------------------------------------
es_sub <-
  ecosystem_services |>
  subset(RCSES > quantile(RCSES, 0.99)) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
                      "RCSES", "service_section")

p <- ggplot(es_sub,
       aes(x = stage, y = RCSES, group = node, connector = connector,
           edge_id = edge_id))

## ----stack_order_asc, results='hide', fig.width=6, fig.height=3---------------
pos <- position_sankey(v_space = "auto", order = "ascending")
p + geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_sankeynode(position = pos)

## ----stack_order_des, results='hide', fig.width=6, fig.height=3---------------
pos <- position_sankey(v_space = "auto", order = "descending")
p + geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_sankeynode(position = pos)

## ----text_no_nudge, results='hide', fig.width=6, fig.height=3-----------------
pos <- position_sankey(v_space = "auto", order = "descending")
p + geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_sankeynode(position = pos) +
  geom_text(aes(label = node), stat = "sankeynode", position = pos, cex = 2)

## ----text_nudge, results='hide', fig.width=6, fig.height=3--------------------
pos_text <- position_sankey(v_space = "auto", order = "descending", nudge_x = 0.1)
p + geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_sankeynode(position = pos) +
  geom_text(aes(label = node), stat = "sankeynode", position = pos_text, hjust = 0, cex = 2) +
  scale_x_discrete(expand = expansion(add = c(0.2, .6)))


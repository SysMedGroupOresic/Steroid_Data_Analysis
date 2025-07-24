library(readxl)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #
sick='all samples';d='t'
lkm=10;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];

c1=c() #
u3=all_all

ACMEMedian=c(); ACMEpval=c(); ACMEVar=c()
ADEMedian=c(); ADEpval=c(); ADEVar=c()
c1= u3 #[u3[,'ADE'] < ADEMedian  & DV<ADEVar,] #& u3[,'z0.p']<ADEpval
ACMEMedian=0 # median(c1[,'ACME'][c1[,'ACME']>0])
c1=c1[c1[,'d0.p']<0.1, ]
c1=data.frame(c1)
a=c1[c1[,'ACME'] <0,]
b=c1[c1[,'ACME'] >0,]
a=a[order(a[,2]),];a=a[1:lkm,]
b=b[rev(order(b[,2])),];b=b[1:lkm,]
ab=rbind(a,b)
dim(ab)
c1=ab

c1$ACME_color <- ifelse(c1$ACME < 0, "red", "blue")


library(ggplot2)
library(ggsankey)
library(dplyr)

# Prepare data
RunAma <- na.omit(c1)
rt2 <- RunAma

if (d == 't') {
  hoi <- scan(text = as.character(rt2[[1]]), what = " ")
} else {
  hoi <- scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
}

hoi <- as.data.frame(matrix(hoi, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
colnames(hoi) <- c('Contaminants', 'Steroids', 'Bile Acids or Lipids')

hoi$ACME <- RunAma$ACME
hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")
hoi$id <- seq_len(nrow(hoi))

# Create long format
df2 <- hoi %>%
  make_long('Contaminants', 'Steroids', 'Bile Acids or Lipids')

# Re-add ID and color
df2$id <- rep(hoi$id, each = 3)
df2$color <- rep(hoi$color, each = 3)


df2 <- df2 %>%
  arrange(x, node, color)


df2$node <- factor(df2$node, levels = unique(df2$node))

my_colors <- c("red" = "#D73027", "blue" = "#4575B4")

# Plot
p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                     label = node, group = interaction(id, x, color))) +
  geom_sankey(aes(fill = color), flow.alpha = 0.9, node.color = "black", node.width = 0.4) +
  geom_sankey_label(size = 4.5, color = "black", fill = "white", label.padding = unit(0.2, "lines")) +
  scale_fill_manual(values = my_colors)+

  theme_sankey(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 6, size = 11, colour = 'black'),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)




#Final?


library(ggplot2)
library(ggsankey)
library(dplyr)

# Prepare data
RunAma <- na.omit(c1)
rt2 <- RunAma

hoi <- if (d == 't') {
  scan(text = as.character(rt2[[1]]), what = " ")
} else {
  scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
}

hoi <- as.data.frame(matrix(hoi, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
colnames(hoi) <- c('Contaminants', 'Steroids', 'Bile Acids or Lipids')

hoi$ACME <- RunAma$ACME
hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")
hoi$id <- seq_len(nrow(hoi))

# Create long format
df2 <- hoi %>%
  make_long('Contaminants', 'Steroids', 'Bile Acids or Lipids')

# Re-add ID and color
df2$id <- rep(hoi$id, each = 3)
df2$color <- rep(hoi$color, each = 3)

# Optional: sort for better alignment
df2 <- df2 %>%
  arrange(x, node, color)

# Optional: factor levels for consistent node order
df2$node <- factor(df2$node, levels = unique(df2$node))

# Plot
p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                     label = node, group = interaction(id, x, color))) +
  geom_sankey(aes(fill = color), flow.alpha = 0.85, node.color = "black", node.width = 0.5) +
  # geom_sankey_bump(aes(fill = color), flow.alpha = 0.85, node.color = "black", node.width = 0.5)+

  geom_sankey_label(size = 4.5, color = "black", fill = "white", label.padding = unit(0.2, "lines")) +
  scale_fill_identity() +
  theme_sankey(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 6, size = 11, colour = 'black'),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)


library(networkD3)
library(dplyr)

# Define flows from contaminants to steroids
links1 <- data.frame(
  source = c("PFHpA", "PFOS", "PFNA"),
  target = c("17a-OHP5", "P4", "DHEA"),
  value = c(10, 5, 8),
  color = c("red", "blue", "red")
)

# Define flows from steroids to lipids/bile acids
links2 <- data.frame(
  source = c("17a-OHP5", "P4", "DHEA"),
  target = c("UDCA_L", "TG_PUFA", "PC"),
  value = c(6, 4, 7),
  color = c("red", "blue", "red")
)

# Combine both sets of links
links <- bind_rows(links1, links2)

# Create a unique list of nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map node names to indices
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Define color scale
my_color <- 'd3.scaleOrdinal().domain(["red", "blue"]).range(["#D73027", "#4575B4"])'

# Plot the Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 14, nodeWidth = 30,
              colourScale = my_color, LinkGroup = "color")



library(dplyr)
library(networkD3)

# Step 1: Create links from Contaminants → Steroids
# Rebuild links1: Contaminants → Steroids
links1 <- hoi %>%
  group_by(source = Contaminants, target = Steroids, color = color) %>%
  summarise(value = sum(abs(ACME)), .groups = "drop")

# Rebuild links2: Steroids → Bile Acids or Lipids
links2 <- hoi %>%
  group_by(source = Steroids, target = `Bile Acids or Lipids`, color = color) %>%
  summarise(value = sum(abs(ACME)), .groups = "drop")


# Step 3: Combine both link sets
# Combine links
links <- bind_rows(links1, links2)

# Create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Clean and map
links$source <- trimws(as.character(links$source))
links$target <- trimws(as.character(links$target))
nodes$name <- trimws(as.character(nodes$name))

links$source_id <- match(links$source, nodes$name) - 1
links$target_id <- match(links$target, nodes$name) - 1



# Step 6: Define color scale
my_color <- 'd3.scaleOrdinal().domain(["red", "blue"]).range(["#D73027", "#4575B4"])'

# Step 7: Plot the Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source_id", Target = "target_id",
              Value = "value", NodeID = "name",
              fontSize = 14, nodeWidth = 30,
              colourScale = my_color, LinkGroup = "color")






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


pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")

p <-
  ggplot(
    data    = es_sub,
    mapping = aes(x = stage, y = RCSES, group = node,
                  edge_id = edge_id, connector = connector))

p +
  geom_sankeynode(position = pos) +
  geom_segment(aes(col = service_section),
               position = pos, stat = "sankeyedge",
               arrow = arrow(length = unit(0.2, "cm")))

p +
  geom_sankeyedge(aes(fill = service_section), position = pos) +
  geom_bar(position = pos, stat = "sankeynode")



library(readxl)

library(networkD3)
library(dplyr)

# Load the Excel file
df <- read_excel("100basic All tikka3624 .xlsx")

# Filter for p < 0.1
filtered <- df %>% filter(`d0.p` < 0.1)

# Select top 15 negative and top 15 positive ACME
filtered <- as.data.frame(filtered)

top_neg <- filtered[order(filtered$ACME), ][1:15, ]
top_pos <- filtered[order(-filtered$ACME), ][1:15, ]

selected <- bind_rows(top_neg, top_pos)

# Split the first column into 3 parts
split <- strsplit(as.character(selected[[1]]), " ")
split_df <- do.call(rbind, lapply(split, function(x) {
  length(x) <- 3
  return(x)
}))
colnames(split_df) <- c("Contaminants", "Steroids", "Bile Acids or Lipids")

# Combine with ACME and color
hoi <- cbind(split_df, selected[, c("ACME", "d0.p")])
hoi <- as.data.frame(hoi)
hoi$ACME <- as.numeric(hoi$ACME)
hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")

# Create links
links1 <- hoi %>%
  group_by(source = Contaminants, target = Steroids, color) %>%
  summarise(value = sum(abs(ACME)), .groups = "drop")

links2 <- hoi %>%
  group_by(source = Steroids, target = `Bile Acids or Lipids`, color) %>%
  summarise(value = sum(abs(ACME)), .groups = "drop")

links <- bind_rows(links1, links2)

# Create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map node names to indices
links$source_id <- match(links$source, nodes$name) - 1
links$target_id <- match(links$target, nodes$name) - 1

# Define color scale
my_color <- 'd3.scaleOrdinal().domain(["red", "blue"]).range(["#D73027", "#4575B4"])'

# Plot Sankey
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source_id", Target = "target_id",
              Value = "value", NodeID = "name",
              fontSize = 14, nodeWidth = 30,
              colourScale = my_color, LinkGroup = "color")





library(networkD3)
library(readr)

# Load the filtered data
nodes <- read_csv("sankey_nodes.csv")
links <- read_csv("sankey_links.csv")





library(readxl)
library(dplyr)

# Load the Excel file
df <- read_excel("100basic All tikka3624 .xlsx")

# Ensure it's a data frame and rename the first column
df <- as.data.frame(df)
colnames(df)[1] <- "Variable"

# Filter for d0.p < 0.1 and remove missing ACME
filtered <- df %>%
  filter(`d0.p` < 0.1, !is.na(ACME))

# Select top 15 negative and top 15 positive ACME
top_neg <- filtered[order(filtered$ACME), ][1:10, ]
top_pos <- filtered[order(-filtered$ACME), ][1:10, ]

selected <- bind_rows(top_neg, top_pos)

# Split Variable into 3 parts
split <- strsplit(as.character(selected$Variable), " ")
split_df <- do.call(rbind, lapply(split, function(x) {
  length(x) <- 3
  return(x)
}))
colnames(split_df) <- c("Contaminants", "Steroids", "Bile Acids or Lipids")

# Combine with ACME and color
hoi <- cbind(split_df, selected[, c("ACME", "d0.p")])
hoi <- as.data.frame(hoi)
hoi$ACME <- as.numeric(hoi$ACME)
hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")

hoi

# From Contaminants → Steroids
links1 <- hoi %>%
  transmute(source = Contaminants,
            target = Steroids,
            color = color,
            value = abs(ACME))

links1

# From Steroids → Bile Acids or Lipids
links2 <- hoi %>%
  transmute(source = Steroids,
            target = `Bile Acids or Lipids`,
            color = color,
            value = abs(ACME))

links2

# Combine
links <- bind_rows(links1, links2)

# Create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map node names to indices
links$source_id <- match(links$source, nodes$name) - 1
links$target_id <- match(links$target, nodes$name) - 1


# View results
print(links)
print(nodes)

data.frame(links)
data.frame(nodes)

# Sort links by color and source-target to improve alignment
links <- links %>%
  arrange(color, source, target)


# Define color scale
my_color <- 'd3.scaleOrdinal().domain(["red", "blue"]).range(["#D73027", "#4575B4"])'

# Plot
# sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "source_id", Target = "target_id",
#               Value = "value", NodeID = "name",
#               fontSize = 14, nodeWidth = 30,
#               colourScale = my_color, LinkGroup = "color")
# 

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source_id", Target = "target_id",
              Value = "value", NodeID = "name",
              fontSize = 14, nodeWidth = 30,
              colourScale = my_color, LinkGroup = "color") %>%
  htmlwidgets::onRender("
    function(el, x) {
      d3.selectAll('.node rect').style('fill', 'gray');
    }
  ")







# library(readxl)
# library(dplyr)
# library(networkD3)
# library(htmlwidgets)
# 
# # Load the Excel file
# df <- read_excel("100basic All tikka3624 .xlsx")
# colnames(df)[1] <- "Variable"
# 
# # Filter for d0.p < 0.1 and valid ACME
# filtered <- df %>%
#   filter(`d0.p` < 0.1, !is.na(ACME))
# 
# # Select top 15 negative and top 15 positive ACME
# top_neg <- filtered[order(filtered$ACME), ][1:15, ]
# top_pos <- filtered[order(-filtered$ACME), ][1:15, ]
# selected <- bind_rows(top_neg, top_pos)
# 
# # Split Variable into 3 parts
# split <- strsplit(as.character(selected$Variable), " ")
# split_df <- do.call(rbind, lapply(split, function(x) {
#   length(x) <- 3
#   return(x)
# }))
# colnames(split_df) <- c("Contaminants", "Steroids", "Bile Acids or Lipids")
# 
# # Combine with ACME and color
# hoi <- cbind(split_df, selected[, c("ACME", "d0.p")])
# hoi <- as.data.frame(hoi)
# hoi$ACME <- as.numeric(hoi$ACME)
# hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")
# 
# # Create links without grouping
# links1 <- hoi %>%
#   transmute(source = Contaminants,
#             target = Steroids,
#             color = color,
#             value = abs(ACME))
# 
# links2 <- hoi %>%
#   transmute(source = Steroids,
#             target = `Bile Acids or Lipids`,
#             color = color,
#             value = abs(ACME))
# 
# links <- bind_rows(links1, links2)
# 
# # Separate red and blue flows to order nodes
# red_links <- links %>% filter(color == "red")
# blue_links <- links %>% filter(color == "blue")
# 
# red_nodes <- unique(c(red_links$source, red_links$target))
# blue_nodes <- unique(c(blue_links$source, blue_links$target))
# ordered_nodes <- unique(c(red_nodes, blue_nodes))
# 
# # Create nodes and map indices
# nodes <- data.frame(name = ordered_nodes)
# links$source_id <- match(links$source, nodes$name) - 1
# links$target_id <- match(links$target, nodes$name) - 1
# 
# # Define color scale for links
# my_color <- 'd3.scaleOrdinal().domain(["red", "blue"]).range(["#D73027", "#4575B4"])'
# 
# # Plot Sankey with grey node bars
# sankey <- sankeyNetwork(Links = links, Nodes = nodes,
#                         Source = "source_id", Target = "target_id",
#                         Value = "value", NodeID = "name",
#                         fontSize = 14, nodeWidth = 30,
#                         colourScale = my_color, LinkGroup = "color")
# 
# # Force node bars to grey
# onRender(sankey, "
#   function(el, x) {
#     d3.selectAll('.node rect').style('fill', 'gray');
#   }
# ")




library(ggplot2)
library(ggalluvial)
library(readr)

# Load the filtered data
# hoi <- read_csv("filtered_hoi.csv")


library(readxl)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #
sick='all samples';d='t'
lkm=10;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];

c1=c() #
u3=all_all

ACMEMedian=c(); ACMEpval=c(); ACMEVar=c()
ADEMedian=c(); ADEpval=c(); ADEVar=c()
c1= u3 #[u3[,'ADE'] < ADEMedian  & DV<ADEVar,] #& u3[,'z0.p']<ADEpval
ACMEMedian=0 # median(c1[,'ACME'][c1[,'ACME']>0])
c1=c1[c1[,'d0.p']<0.1, ]
c1=data.frame(c1)
a=c1[c1[,'ACME'] <0,]
b=c1[c1[,'ACME'] >0,]
a=a[order(a[,2]),];a=a[1:lkm,]
b=b[rev(order(b[,2])),];b=b[1:lkm,]
ab=rbind(a,b)
dim(ab)
c1=ab
c1$ACME_color <- ifelse(c1$ACME < 0, "red", "blue")
library(ggplot2)
library(ggsankey)
library(dplyr)
# Prepare data
RunAma <- na.omit(c1)
rt2 <- RunAma

if (d == 't') {
  hoi <- scan(text = as.character(rt2[[1]]), what = " ")
} else {
  hoi <- scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
}

hoi <- as.data.frame(matrix(hoi, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
colnames(hoi) <- c('Contaminants', 'Steroids', 'Bile Acids or Lipids')

hoi$ACME <- RunAma$ACME
hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")
hoi$id <- seq_len(nrow(hoi))


# Create the alluvial plot
# ggplot(hoi,
#        aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
#            y = abs(ACME), fill = color)) +
#   geom_alluvium(aes(fill = color), width = 1/12, knot.pos = 0.5, alpha = 0.9) +
#   geom_stratum(width = 1/12, fill = "grey", color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
#   scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
#                    expand = c(.05, .05)) +
#   scale_fill_manual(values = c("red" = "#D73027", "blue" = "#4575B4")) +
#   theme_minimal() +
#   theme(legend.position = "none")
# 


# Filter out NA values in the 'color' column
# hoi <- hoi %>% filter(!is.na(color))



# Create the alluvial plot
# ggplot(hoi,
#        aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
#            y = abs(ACME), fill = color)) +
#   geom_alluvium(aes(fill = color), width = 1/12, knot.pos = 0.5, alpha = 0.9) +
#   geom_stratum(width = 1/12, fill = "grey", color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
#   scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
#                    expand = c(.05, .05)) +
#   scale_fill_manual(name = "ACME Direction",
#                     values = c("red" = "#D73027", "blue" = "#4575B4"),
#                     labels = c("Negative", "Positive")) +
#   theme_minimal() +
#   theme(legend.position = "right",
#         axis.title.y = element_blank())
# 

# Create the alluvial plot with enhanced styling
ggplot(hoi,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  geom_alluvium(aes(fill = color), width = 0.15, knot.pos = 0.5, alpha = 0.9) +
  geom_stratum(width = 0.15, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name = "ACME Direction",
    values = c("red" = "#D73027", "blue" = "#4575B4"),
    labels = c("Negative", "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "black", face = "bold")  # Customize x-axis labels
  )




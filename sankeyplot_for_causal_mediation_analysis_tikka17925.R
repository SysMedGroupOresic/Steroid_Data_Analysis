library(ggplot2)
library(ggalluvial)
library(readr)
library(readxl)
library(ggsankey)
library(dplyr)
# setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/100basic Male tikka3624.xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #
#100basic Female tikka3624.xlsx 100basic Male tikka3624.xlsx 100basic All tikka3624 .xlsx

# all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/total_All_All_tikka15925_mediation_results.xlsx")
  # C:\Users\patati\Documents\GitHub\Steroid_Data_Analysis

# all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/just alal_uus__female_160925_allds_mediation_results_mimmax_cov3.xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #
# just alal_uus__All_160925_allds_mediation_results_mimmax_cov2.xlsx
# just alal_uus__All_160925_allds_mediation_results_mimmax_cov2.xlsx
# just alal_uus__male_160925_allds_mediation_results_mimmax_cov2.xlsx
# just alal_uus__female_160925_allds_mediation_results_mimmax_cov2
# just alal_uus__female_160925_allds_mediation_results_mimmax_cov3.xlsx

sick='all samples';d='t'
date <- strftime(Sys.Date(), "%d%m%y") #Do not take the old date from the load...

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
# dim(ab)
c1=ab
c1$ACME_color <- ifelse(c1$ACME < 0, "blue", "orange")

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

# # Extract row names or first column depending on your format
# names_vec <- as.character(rt2[[1]])  # or rownames(rt2) if needed
# 
# names_vec <- gsub("PFHxA_Branched", "PFHxA-B.",names_vec)
# 
# 
# # Split each name by "_"
# split_names <- strsplit(names_vec, "_")
# 
# # Convert to data frame
# hoi <- do.call(rbind, lapply(split_names, function(x) {
#   len <- length(x)
#   if (len >= 3) {
#     c(Contaminant = x[1], Steroid = x[2], Outcome = paste(x[3:len], collapse = "_"))
#   } else {
#     c(Contaminant = NA, Steroid = NA, Outcome = NA)
#   }
# }))
# 
# hoi <- as.data.frame(hoi, stringsAsFactors = FALSE)
# colnames(hoi) <- c("Contaminants", "Steroids", "Bile Acids or Lipids")


hoi$ACME <- RunAma$ACME
hoi$color <- ifelse(hoi$ACME < 0,  "blue","orange")
hoi$id <- seq_len(nrow(hoi))

# c('#6FA3E0', 'white','#EC7B6E')

hoi$Contaminants <- gsub("PFHxA_Branched", "PFHxA_B.", hoi$Contaminants)
hoi$Steroids <- gsub("17a\\.OHP5", "17a-OHP5", hoi$Steroids)
hoi$Steroids <- gsub("17a\\.OHP4", "17a-OHP4", hoi$Steroids)
hoi$Steroids <- gsub("11\\.KT", "11-KT", hoi$Steroids)
hoi$Steroids <- gsub("11\\.KDHT", "11-KDHT", hoi$Steroids)


# Create the alluvial plot with enhanced styling
# ggplot(hoi,
#        aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
#            y = abs(ACME), fill = color)) +
#   geom_alluvium(aes(fill = color), width = 0.15, knot.pos = 0.5, alpha = 0.9) +
#   geom_stratum(width = 0.15, fill = "white", color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6,fontface='bold') +
#   scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
#                    expand = c(.05, .05)) +
#   scale_fill_manual(
#     name = "ACME Direction",
#     values = c( "blue" = "blue","orange" = "orange"),
#     labels = c( "Negative","Positive"),
#     na.translate = FALSE
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     
#     legend.text = element_text(size = 18, face = "bold"),       # Legend item labels
#     legend.title = element_text(size = 18, face = "bold"),  # Legend title
#     
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.x = element_text(size = 20, color = "black", face = "bold"),  # Customize x-axis labels
#     panel.grid = element_blank(),         # Removes all grid lines
#     panel.background = element_blank(),   # Removes panel background
#     plot.background = element_blank()     # Removes plot background
#     
#     )
# # +geom_alluvium(
#     #   aes(fill = color),
#     #   width = 0.15, knot.pos = 0.5, alpha = 0.9,
#     #   colour = "black",   # <- same edge color for all
#     #   size = 0.5, lineend = "round"
#     # )



### ok

library(ggplot2)
library(ggalluvial)

# ggplot(hoi,
#        aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
#            y = abs(ACME), fill = color)) +
#   # geom_alluvium(
#   #   aes(fill = color, colour = color),     # <- edge color follows fill
#   #   width = 0.15, knot.pos = 0.5, alpha = 0.9,
#   #   size = 0.5,                            # <- edge thickness
#   #   lineend = "round",
#   #   show.legend = c(colour = FALSE)        # <- avoid duplicate legend
#   # ) +
#   
#   geom_alluvium(
#     aes(fill = color),
#     width = 0.15, knot.pos = 0.5, alpha = 0.9,
#     colour = "grey80",   # <- same edge color for all
#     size = 0.3, #lineend = "round",
#   )+
# 
#   geom_stratum(width = 0.15, fill = "white", color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)),
#             size = 6, fontface = 'bold') +
#   scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
#                    expand = c(.05, .05)) +
#   scale_fill_manual(
#     name   = "ACME Direction",
#     values = c("blue" = "blue", "orange" = "orange"),
#     labels = c("Negative","Positive"),
#     na.translate = FALSE
#   ) +
#   
#   
#   # geom_alluvium(
#   #   aes(fill = color, colour = color),     # <- edge color follows fill
#   #   width = 0.15, knot.pos = 0.5, alpha = 0.9,
#   #   size = 0.5,                            # <- edge thickness
#   #   lineend = "round",
#   #   show.legend = c(colour = FALSE)        # <- avoid duplicate legend
#   # )+
# 
# 
#   
#   scale_colour_manual(values = c("blue" = "blue", "orange" = "orange"),
#                       guide = "none") +    # <- no separate edge legend
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 18, face = "bold"),
#     legend.title = element_text(size = 18, face = "bold"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.x = element_text(size = 20, color = "black", face = "bold"),
#     panel.grid = element_blank(),
#     panel.background = element_blank(),
#     plot.background = element_blank()
#   )


####

p=ggplot(hoi,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  geom_alluvium(
    aes(fill = color,
        colour = after_scale(colorspace::darken(fill, 0.15))),  # darker edge from fill
    width = 0.15, knot.pos = 0.5, alpha = 0.9,
    size = 0.6, lineend = "round",
    show.legend = c(colour = FALSE)  # keep only the fill legend
  ) +
  geom_stratum(width = 0.15, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 6, fontface = 'bold') +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name   = "ACME Direction",
    values = c("blue" = "blue", "orange" = "orange"),
    labels = c("Negative", "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text  = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20, color = "black", face = "bold"),
    panel.grid   = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank()
  )

pngfile <- fs::path(path, paste0('sankey med res', "male nonadj.png"))
ragg::agg_png(pngfile, width = 120, height = 60, units = "cm", res = 500, scaling = 2)
print(p)
invisible(dev.off())
p



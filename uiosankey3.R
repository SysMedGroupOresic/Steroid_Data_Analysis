library(readxl)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #


sick='all samples';d='t'
lkm=30;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];
alma=ReduceData(all_all,Group,name,lkm);
alma = na.omit(alma)
CreateSankeyPlots(alma,date,sick,Group,d)



# To do these diagrams, you need to have a reduced dataset as well as a function that accounts for the group (male/female)
ReduceData=function(u3, Group, name, lkm, d) {
  c1=c() #
  u3=all_all
  
  ACMEMedian=c(); ACMEpval=c(); ACMEVar=c()
  ADEMedian=c(); ADEpval=c(); ADEVar=c()
  c1= u3 #[u3[,'ADE'] < ADEMedian  & DV<ADEVar,] #& u3[,'z0.p']<ADEpval
  ACMEMedian=0 # median(c1[,'ACME'][c1[,'ACME']>0])
  # c1=c1[order(c1[,'ACME']),];  # for 'negative' acmes
  # c1=c1[rev(order(c1[,'ACME'])),];  
  # c1=c1[c1[,'ACME']<ACMEMedian & ((c1[,'ADE']-c1[,'ACME']) > 0), ] # 
  c1=c1[c1[,'d0.p']<0.1, ]
  # hon=quantile(c1[,'ACME'])
  # c1=c1[c1[,'ACME'] > ACMEMedian & ((c1[,'ACME']-c1[,'ADE']) > 0), ] # c1=c1[c1[,'d0.p']<0.1, ] 
  c1=data.frame(c1)
  a=c1[c1[,'ACME'] <0,]
  b=c1[c1[,'ACME'] >0,]
  a=a[order(a[,2]),];a=a[1:lkm,]
  b=b[rev(order(b[,2])),];b=b[1:lkm,]
  ab=rbind(a,b)
  dim(ab)
  c1=ab
  
  
  # c1=tryCatch({c1[1:lkm,]}, error = function(msg){return(c1)})
  
  write.xlsx(c1, file = paste(name, Group, date, '.xlsx'), append = FALSE, row.names = TRUE)
  c1$ACME_color <- ifelse(c1$ACME < 0, "red", "blue")
  return(c1)
}

# CreateSankeyPlots=function(RunAma, date, sick, Group, d) {
#   RunAma = na.omit(c1)
#   rt2=RunAma #[,1:17]# rtot=rtot[,1:17]# rtot=data.frame(rtot) # name=paste(simss,'basic hypothesis',take)
#   # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
#   # https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/adding-covariates-to-a-linear-model
#   # https://github.com/MarioniLab/miloR
#   # https://www.nature.com/articles/s41467-023-40458-9/figures/4
#   name=paste('Contaminants_Steroids_BAs_or_Lipids_sims', date) # rtot=rtot_2000_mrct # rtot=uh5
#   
#   hoi=c(); 
# 
#   
#   if (d == 't') {
#     hoi = scan(text = as.character(rt2[[1]]), what = " ")
#   } else {
#     hoi = scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
#   }
#   
#   
#   
#   # rownames(rt2)# names(rt2[,1]) rownames(rt2
#   # hoi=scan(text=rownames(rt2), what=" ")# rownames(rt2)# names(rt2[,1]) rownames(rt2
#   hoi=as.data.frame(matrix(hoi, ncol = 3,  byrow = TRUE), stringsAsFactors = FALSE) # Check this number (ncol) 3/4
#   # hoi=cbind(hoi[,1:3],rt2[,2])
#   hoi=hoi[,1:3]
#   # colnames(hoi)=c('Contaminants','Steroids','Bile Acids or Lipids','Weight')# ,'Gender') ##
#   colnames(hoi)=c('Contaminants','Steroids','Bile Acids or Lipids')
#   # ,'Gender') ## https://stats.stackexchange.com/questions/282155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct
#   # https://www.researchgate.net/post/How_can_I_interpret_a_negative_indirect_effect_for_significant_mediation
#   # https://stackoverflow.com/questions/31518150/gsub-in-r-is-not-replacing-dot replacing dot
#   
#   hoi[,'Steroids' ][hoi[,'Steroids' ]=='17aOH.P4']='17a-OHP4'
#   hoi[,'Steroids' ][hoi[,'Steroids' ]=='17aOH-P4']='17a-OHP4'
#   hoi[,'Steroids' ]  <- gsub("\\.", "-",  hoi[,'Steroids' ] ) #:)
#   hoi[,'Steroids' ][ hoi[,'Steroids' ]=='T-Epi-T']='T/Epi-T'
#   # df2 <- hoi %>%make_long('Contaminants','Steroids','Bile Acids or Lipids','Weight') # see the sankey test file 17.10.24 for this...
#   
#   df2 <- hoi %>%make_long('Contaminants','Steroids','Bile Acids or Lipids') 
#   
# 
#   windowsFonts(A = windowsFont("Calibri (Body)")) 
# 
#   # Add ACME color to df2
#   RunAma$node_label <- as.character(RunAma[[1]])  # assuming first column matches 'node' in df2
#   RunAma$ACME_color <- ifelse(RunAma$ACME < 0, "red", "blue")
#   
#   df2 <- df2 %>%
#     left_join(RunAma[, c("node_label", "ACME_color")], by = c("node" = "node_label"))
#   
#   # Use ACME_color in fill
#   p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
#                        fill = ACME_color, label = node)) +
#     geom_sankey(flow.alpha = 1, node.color = 1) +
#     geom_sankey_label(size = 8.0, color = 1, fill = "white") +
#     scale_fill_identity() +  # use actual color values
#     theme_sankey(base_size = 28) +
#     theme(legend.position = "none") +
#     theme(axis.text.x = element_text(hjust = 0.5, vjust = 7, colour = 'black')) +
#     theme(axis.title.x = element_blank())
# 
#   library(ragg)
#   # Oh! https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
#   # https://r4ds.had.co.nz/graphics-for-communication.html#figure-sizing
#   
#   path="C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/" # oh, classical: https://forum.posit.co/t/r-markdown-html-document-doesnt-show-image/41629/2
#   pngfile <- fs::path(path, paste0(Group, 'e', d, ".png")) # fs::path(knitr::fig_path(),  "theming2.png")
#   # agg_png(pngfile, width = 30, height = 40, units = "cm", res = 300, scaling = 2) #
#   agg_png(pngfile, width = 60, height = 80, units = "cm", res = 600, scaling = 2)
#   plot(p)
#   invisible(dev.off())
#   knitr::include_graphics(pngfile)
#   eoh=paste0(Group, 'e', d, ".png"); daiR::image_to_pdf(eoh, pdf_name=paste0(eoh, '.pdf'))
#   my_image <- image_read(eoh); my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh, ".svg"))
# }

CreateSankeyPlots <- function(RunAma, date, sick, Group, d) {
  # Load required libraries
  library(ggplot2)
  library(ggsankey)
  library(dplyr)
  library(magick)
  library(fs)
  library(openxlsx)
  RunAma = na.omit(c1)
  rt2 <- RunAma
  name <- paste('Contaminants_Steroids_BAs_or_Lipids_sims', date)
  
  # Extract node names
  if (d == 't') {
    hoi <- scan(text = as.character(rt2[[1]]), what = " ")
  } else {
    hoi <- scan(text = as.character(rownames(as.data.frame(rt2))), what = " ")
  }
  
  hoi <- as.data.frame(matrix(hoi, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(hoi) <- c('Contaminants', 'Steroids', 'Bile Acids or Lipids')
  
  # Add ACME values and unique ID
  hoi$ACME <- RunAma$ACME
  hoi$color <- ifelse(hoi$ACME < 0, "red", "blue")
  hoi$id <- seq_len(nrow(hoi))
  
  # Create long format and add ID manually
  df2 <- hoi %>%
    mutate(id = row_number()) %>%
    make_long('Contaminants', 'Steroids', 'Bile Acids or Lipids')
  
  # Add color by joining with original data
  df2 <- df2 %>%
    left_join(hoi %>% select(id, color), by = "id")
  
  
  # Re-add ID and color to long format
  df2$id <- rep(hoi$id, each = 3)
  df2$color <- rep(hoi$color, each = 3)
  
  # Create a color map only for Steroids
  steroid_colors <- unique(hoi[, c("Steroids", "color")])
  names(steroid_colors$color) <- steroid_colors$Steroids
  
  # Plot
  p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                       label = node, group = node)) +
    geom_sankey(flow.alpha = 0.9, node.color = "black", node.width = 0.4,
                aes(fill = after_stat(node))) +
    geom_sankey_label(size = 4.5, color = "black", fill = "white", label.padding = unit(0.2, "lines")) +
    scale_fill_manual(
      values = steroid_colors$color,
      guide = "none"
    ) +
    theme_sankey(base_size = 16) +
    theme(
      axis.text.x = element_text(hjust = 0.5, vjust = 6, size = 11, colour = 'black'),
      axis.title.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  
  
  # Save plot
  path <- "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/"
  pngfile <- fs::path(path, paste0(Group, 'e', d, ".png"))
  message("Saving plot to: ", pngfile)
  ragg::agg_png(pngfile, width = 60, height = 80, units = "cm", res = 600, scaling = 2)
  print(p)
  dev.off()
  
  # Check if file exists before continuing
  if (!file.exists(pngfile)) {
    stop("Plot image was not saved. Check the path or permissions.")
  }
  
  # Convert to PDF and SVG
  eoh <- paste0(Group, 'e', d, ".png")
  daiR::image_to_pdf(eoh, pdf_name = paste0(eoh, '.pdf'))
  my_image <- image_read(eoh)
  my_svg <- image_convert(my_image, format = "svg")
  image_write(my_svg, paste0(eoh, ".svg"))
}



library(readxl)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #


sick='all samples';d='t'
lkm=30;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];
alma=ReduceData(all_all,Group,name,lkm);
alma = na.omit(alma)
CreateSankeyPlots(alma,date,sick,Group,d)


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

# Plot
p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                     label = node, group = interaction(id, x))) +
  geom_sankey(aes(fill = color), flow.alpha = 0.9, node.color = "black", node.width = 0.4) +
  geom_sankey_label(size = 4.5, color = "black", fill = "white", label.padding = unit(0.2, "lines")) +
  scale_fill_identity() +
  theme_sankey(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 6, size = 11, colour = 'black'),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)




#Final?

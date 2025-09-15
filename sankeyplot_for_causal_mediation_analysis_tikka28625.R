library(ggplot2)
library(ggalluvial)
library(readr)
library(readxl)
library(ggsankey)
library(dplyr)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #

all_all=read_xlsx(path = "C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/total_All_All_tikka15925_mediation_results.xlsx")
  # C:\Users\patati\Documents\GitHub\Steroid_Data_Analysis

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
c1$ACME_color <- ifelse(c1$ACME < 0, "red", "blue")

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
hoi$color <- ifelse(hoi$ACME < 0,  "blue","red")
hoi$id <- seq_len(nrow(hoi))

# c('#6FA3E0', 'white','#EC7B6E')

# Create the alluvial plot with enhanced styling
ggplot(hoi,
       aes(axis1 = Contaminants, axis2 = Steroids, axis3 = `Bile Acids or Lipids`,
           y = abs(ACME), fill = color)) +
  geom_alluvium(aes(fill = color), width = 0.15, knot.pos = 0.5, alpha = 0.9) +
  geom_stratum(width = 0.15, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6,fontface='bold') +
  scale_x_discrete(limits = c("Contaminants", "Steroids", "Bile Acids or Lipids"),
                   expand = c(.05, .05)) +
  scale_fill_manual(
    name = "ACME Direction",
    values = c( "blue" = "#6FA3E0","red" = "#EC7B6E"),
    labels = c( "Negative","Positive"),
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    
    legend.text = element_text(size = 18, face = "bold"),       # Legend item labels
    legend.title = element_text(size = 18, face = "bold"),  # Legend title
    
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 20, color = "black", face = "bold"),  # Customize x-axis labels
    panel.grid = element_blank(),         # Removes all grid lines
    panel.background = element_blank(),   # Removes panel background
    plot.background = element_blank()     # Removes plot background
    
    )

library(readxl)
setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #


sick='all samples';d='t'
lkm=10;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];
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



# library(readxl)
# setwd("C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/") #check this if needed...
# all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") #total Male tikka76524 hyp4b_oki.xlsx") #
# 
# 
# sick='all samples';d='t'
# lkm=30;Group='All'; name='just alal_uus';date=paste0(date,'_allds')#dim(all_all)[1];
# alma=ReduceData(all_all,Group,name,lkm);
# alma = na.omit(alma)
# CreateSankeyPlots(alma,date,sick,Group,d)


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

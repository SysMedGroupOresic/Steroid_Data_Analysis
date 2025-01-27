
#The data for chicken mediation analysis:
kana=read_excel("kananmaksa11123_v2.xlsx")
oknames=colnames(kana); kana=data.frame(kana)
colnames(kana)=oknames
rownames(kana)=kana[,1]

vt=kana
vte=vt[,4:dim(vt)[2]]; vte[vte == 0] <- NA; print(vte, max=nrow(vte)*ncol(vte))
vt_half <- vte %>% mutate(replace(., is.na(.), min(., na.rm = T)/2)) #https://mdatools.com/docs/preprocessing--autoscaling.html
vt_half_log2 <- log2(vt_half);
vt_auto <- prep.autoscale(vt_half, center = TRUE, scale = TRUE);  #https://svkucheryavski.gitbooks.io/mdatools/content/preprocessing/text.html
vt_all=cbind(vt[,1:3],vt_auto); vt_all[1:5,1:11];
Treatment='PFOA_L'
Mediator=colnames(vt_all)[24:dim(vt_all)[2]]
Outcome=colnames(vt_all)[6:23]

date='tikka151123' #change this...
name='The chicken data mediation analysis_1000sims'

Treatment; Mediator; Outcome

rtot=loop_med_simplified(Treatment, Mediator, Outcome,vt_all,name);

rtot=rtot[order(rtot[,2]),]
head(rtot)


loop_med_simplified=function(Treatment, Mediator, Outcome,vt_all,name) {
  # if (Group=='Female') {cond=tv_all[,'Gender']==1} else if (Group=='Male') # {cond=tv_all[,'Gender']==2} else if (Group=='All') {cond=rep(TRUE,dim(tv_all)[1])}
  tv_red=vt_all
  X <- tv_red[,Treatment]#Standard values did not five erros # hep=colnames(X)[!colnames(X) %in% c( "Benzylparaben" ,"Methylparaben")] # X=X[,hep]
  M <- tv_red[,Mediator]#
  Y <- tv_red[,Outcome] #"Steatosis.Grade.0.To.3"       "Fibrosis.Stage.0.to.4"       "Necroinflammation"            "HOMA-IR"   
  Data <- cbind(X,M,Y); #
  colnames(Data)[which(names(Data) == "X")] <- "PFOA_L"
  colnames(Data) <- gsub(" ", "_", colnames(Data))
  # colnames(Data[,1:2])[1]=Treatment
  #  https://rdrr.io/cran/mlma/man/data.org.html # https://cran.r-project.org/web/packages/mma/mma.pdf
  # this time the b and c are in the loop, and b is the model 'M', i.e. M~X (e.g. Allocholic acid ~ PFOA_L)
  # the c is the model 'Y', i.e. Y~M+X, e.g. CAR ~PFOA + Allocholic acid (note, just one mediator at the time... )
  
  #M~X
  x=c();m=c(); y=c();ye=c()
  for (i in 1:1) {x=append(x,paste("Data[, ",i , "]", sep=""))}
  for (i in 2:(dim(M)[2]+1)) {m=append(m,paste("Data[, ",i , "]", sep="")) }
  #Y~X+M
  for (i in (dim(M)[2]+2):(dim(Data)[2])) {y=append(y,paste("Data[, ",i , "]", sep="")) }
  
  med_out=c();res=c(); tmp=c();rn=c();med_oute=c();med_sense=c();resa=c()  
  
  for (j in 1:dim(M)[2]) { #control.value=mina[i]
    for (i in 1:length(y)) {
      fmla1 <- as.formula(paste(paste(m[j], collapse= "+")," ~ ", paste(x, collapse= "+"))) 
      b = lm(fmla1, Data)  
      xm=paste(c(x,m[j]), collapse= "+")
      fmla2 <- as.formula(paste(y[i]," ~ ", xm))
      c = lm(fmla2, Data) 
      med_oute=mediate(b, c, treat =  "Data[, 1]", mediator = m[j],sims = 1000)  
      med_out = summary(med_oute) #you need sims=100 min for the paper, maybe more like 1000... 10 was too little, but can get you results fast..
      tmp=c(med_out$d0, med_out$d0.p, med_out$d0.ci[1],med_out$d0.ci[2],med_out$z0, med_out$z0.p, med_out$z0.ci[1],
            med_out$z0.ci[2], med_out$n1, med_out$n1.p,med_out$n1.ci[1],med_out$n1.ci[2],med_out$tau.coef,med_out$tau.p,med_out$tau.ci[1],med_out$tau.ci[2]) 
      res <- rbind(res,tmp);
      rn=append(rn,paste('PFOA_L',colnames(M)[j],colnames(Y)[i]))
      remove(tmp)}}
  rownames(res)=rn
  colnames(res)=c('ACME', 'd0.p', 'd0.ci_l','d0.ci_u','ADE', 'z0.p', 'z0.ci_l','z0.ci_u', 
                  'Proportion Mediated', 'n1.p','n.ci_l','n1.ci_u','Total Effect','tau.p','tau.ci_l','tau.ci_u') #d0 and d1 are the same as.. 'd1', 'd1.p',
  res=res[rev(order(res[,2])),]
  write.xlsx(res, file = paste(name,date,'.xlsx'), append = FALSE, row.names = TRUE) #https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
  return(res)}

# date='151123'; 
# Treatment='PFOA_L'
# Mediator=colnames(tv_all)[24:dim(tv_all)[2]]
# Outcome=colnames(tv_all)[6:23]



# #https://stackoverflow.com/questions/19535996/avoid-rbind-cbind-conversion-from-numeric-to-factor
# res1f=cbind(res1f,'Steatosis','Female');colnames(res1f)[9]='Case';colnames(res1f)[10]='Gender';

library(ggsankey)
rtot_smootherre_100_ci=res #rtot
write.csv(data.frame(rtot_smootherre_100_ci),'mediation_chicken_rtot_smootherre_100_ci_all_tikka11123.csv')
rtot=res

write.csv(data.frame(rtot),'mediation_chicken_tikka11123.csv')

rtot=read_excel("The chicken data mediation analysis_1000sims tikka151123_.xlsx")

sankeys=function(med,rtot,cut,Group,size,div) {
  if (size=='All') {rt2=rtot} else if (size!='All')  {rt2=rtot[rtot[,'d0']>size,]}
  if (med=='Mediated') {rt2=rt2[rt2[,'d0.p']<pcut,]; rt2=rt2[rt2[,'z0.p']>pcut,]} else {rt2=rt2[rt2[,'d0.p']>pcut,]; rt2=rt2[rt2[,'z0.p']<pcut,]}
  # rt2=res
  # e=cbind(rt2[,3],rt2[,4])>0 #| sum(as.vector(rt2$d0.ci_l)<0)>1  
  # po=c(); if (med=='Mediated') {for (i in 1:dim(e)[1]) {po=append(po,sum(e[i,1:2])>1)}; rt2=rt2[po,]} else {for (i in 1:dim(e)[1]) {po=append(po,sum(e[i,7:8])>1)}; rt2=rt2[po,]}
  hoi=c();for (i in 1:dim(rt2)[1]) {hoi=append(hoi,c(scan(text=rownames(rt2)[i], what="")))}
  hoi=as.data.frame(matrix(hoi, ncol = 3,  byrow = TRUE), stringsAsFactors = FALSE)
  # hoi[,3]=rt2[,15];hoi[,4]=rt2[,16];hoi[,5]=rt2[,1]
  colnames(hoi)=c("Contaminant",'Bile Acids','Lipids')# https://stats.stackexchange.com/questions/271155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct
  # https://www.researchgate.net/post/How_can_I_interpret_a_negative_indirect_effect_for_significant_mediation
  # hoi=hoi[,1:4]
  # if (Group=='All') {hoi=hoi} else if  
  # (Group=='Female') {hoi=hoi[hoi[,'Gender']=='Female',]} else if 
  # (Group=='Male') {hoi=hoi[hoi[,'Gender']=='Male',]};hoi
  df2 <- hoi %>% make_long("Contaminant",'Bile Acids','Lipids')
  jpeg(paste(med,Group,div ,"e.jpg"), width = 7500, height = 11000, quality = 100,pointsize = 16, res=1000);
  print(
    ggplot(df2, aes(x = x,  next_x = next_x, node = node,  next_node = next_node,fill = factor(node),label = node)) +
      geom_sankey(flow.alpha = 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") +
      scale_fill_viridis_d() +
      theme_sankey(base_size = 16) + theme(legend.position = "none")+theme(axis.title.x = element_blank())
  );dev.off()}

hist(as.numeric(rtot[,'d0.p']),breaks=50)
pcut=0.05
med='Mediated' #Mediated or no
size=0
Group='All'
div='Yes_100a'
sankeys(med,rtot,cut,Group,size,div) 

# https://library.virginia.edu/data/articles/introduction-to-mediation-analysis
# https://ademos.people.uic.edu/Chapter14.html

rtot=as.data.frame(rtot)

date='27125tikka'
name=paste('Chicken_1000simsa',date) # rtot=rtot_2000_mrct
jpeg(paste("ACME_p_histograma",name,".jpg"), width = 1500, height = 2000, quality = 95,pointsize = 16, res=300); hist(as.numeric(rtot[,'d0.p']),breaks=100);dev.off()
jpeg(paste("Mediated Proportion_histograma",name,".jpg"), width = 1500, height = 2000, quality = 95,pointsize = 16, res=300); hist(as.numeric(rtot[,'Proportion.Mediated']),breaks=2000,xlim=c(-2,2));dev.off()
med.freq.cut=0.05; med.min=0; med.prop=0.5; Group='All'; med='no' #or not mediated e.g. no
# sankeys(rtot,med,Group,med.min,med.prop,name) 

# sankeys=function(rtot,med,Group,med.min,med.prop, name) {
rt2=rtot
# if (med.min=='All') {rt2=rtot} else if (med.min!='All')  {if (med=='Mediated') {rt2=rtot[rtot[,'ACME']>med.min,];
# rt2=rt2[rt2[,'Proportion.Mediated']>med.prop,]} else {rt2=rtot[rtot[,'z0.p']>med.min,];rt2=rt2[rt2[,'Proportion.Mediated']<med.prop,]}}
# if (med=='Mediated') {rt2=rt2[rt2[,'d0.p']<med.freq.cut,]; rt2=rt2[rt2[,'z0.p']>med.freq.cut,];rt2=rt2[rt2[,'ACME']>0,]} else 
# {rt2=rt2[rt2[,'d0.p']>med.freq.cut,]; rt2=rt2[rt2[,'z0.p']<med.freq.cut,]}

rt2=rt2[rev(order(rt2[,'ADE'])),]
rt2=rt2[rt2[,'ADE']>0.5,]
rt2=rt2[rt2[,'z0.p']<0.01,]
rt2=rt2[rt2[,'Proportion.Mediated']<0,]

# hoi=scan(text=rt2[,1], what=".")
hoi=c(); 
# rownames(rt2) <- gsub("Tauro-muricholic acid", "Tauro-muricholic.acid", rownames(rt2))
# rownames(rt2)
# hoi=scan(text=rownames(rt2), what="")
original_string=rt2[9,1]
modified_string <- sub("Allocholic acid", "Allocholic_acid", original_string)

# Original list of strings
original_list <-rt2[,1]

# Function to replace "sample string" with "sample_string"
replace_sample_string <- function(x) {
  gsub(" acid", "_acid", x)}

# Apply the function to each element in the list
modified_list <- lapply(original_list, replace_sample_string)
# Print the modified list
# print(modified_list)

rt2[,1]=unlist(modified_list)

hoi=scan(text=rt2[,1], what="")


hoi=as.data.frame(matrix(hoi, ncol = 3,  byrow = TRUE), stringsAsFactors = FALSE)
# hoi[,4]=rt2[,'Case'];hoi[,5]=rt2[,'Gender'];
# hoi[,4]=rt2[,2]

colnames(hoi)=c('Contaminant','Bile Acids','Lipids')# median(as.numeric(hoi[,'Mediation']),breaks=50)# hist(as.numeric(hoi[,'Mediation']),breaks=50)
# https://stats.stackexchange.com/questions/271155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct
# https://www.researchgate.net/post/How_can_I_interpret_a_negative_indirect_effect_for_significant_mediation
# hoi=hoi[,1:4]; 
# hoi[,2] <- gsub("X", "", hoi[,2]); hoi[,1]<- gsub('\\.', ' ', hoi[,1]); hoi[,2]<- gsub('\\.', '/', hoi[,2]);
# https://stackoverflow.com/questions/31518150/gsub-in-r-is-not-replacing-dot replacing dot

hoi=hoi[,c(1,3)] #check columns

df2 <- hoi %>%make_long('Contaminant','Lipids')
# df2 <- hoi %>%make_long('Bile Acids','Lipids')


meda='Sankey plot of_ADE_ok'
jpeg(paste(meda,name ,".jpg"), width = 7500, height = 11000, quality = 100,pointsize = 16, res=1000);
print(ggplot(df2, aes(x = x,  next_x = next_x, node = node,  next_node = next_node,fill = factor(node),label = node)) +
        geom_sankey(flow.alpha = 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") +
        scale_fill_viridis_d() + theme_sankey(base_size = 16) + theme(legend.position = "none")+theme(axis.title.x = element_blank()));dev.off()
# }


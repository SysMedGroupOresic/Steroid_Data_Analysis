library(readxl)
all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/tests6/tests_basic/100basic All tikka3624 .xlsx") # #_1
# all_all=read_xlsx(path = "C:/Users/patati/Desktop/Turku/R/hypo_basic/100 hypo_b_no_not sick All tikka221024 .xlsx") # #_2 :)
all_all=as.data.frame(all_all); all_all=all_all[!is.na(all_all[,1]),];rownames(all_all)=all_all[,1]; all_all=all_all[,2:dim(all_all)[2]]; all_all=all_all[rev(order(all_all[,1])),]
all_all=all_all; #all_all=all_all[all_all[,1]>0,]
#https://stats.stackexchange.com/questions/282155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct
#https://www.researchgate.net/post/How_can_I_interpret_a_negative_indirect_effect_for_significant_mediation
#https://stackoverflow.com/questions/31518150/gsub-in-r-is-not-replacing-dot replacing dot
groups[,'Abbreviation'][groups[,'Abbreviation']=='17aOH-P4']='17a-OHP4'

#Switch = 0: PFAS vs steroids; switch=1: PFAS vs BAs and lipids, switch=2: steroids vs BAs and lipids (0-2 with both ACME and ADE (z='dir'))
houdees=function(hoi, rt2, switch,mn,z,corr,date,neg) {
  indir=c(); dir=c(); ip=c();rn=c();rn2=c()
  Outcome=colnames(tv_covNS)[c(29:51,59:71)]; #The final dataframe is shorter or the like so there were less variables here...
  Treatment=colnames(tv_covNS)[52:58];
  ##https://sparkbyexamples.com/r-programming/r-remove-from-vector-with-examples/
  
  #direct...
  if (switch==1) {
    Mediator_ok=Outcome[Outcome %in% names(table(hoi[1:dim(hoi)[1],c(3)]))]
    for (i in 1:7) {for (j in 1:length(Mediator_ok)) {
      if (z=='dir') {
        ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ADE']))) {if (median(ap[,'ADE'])< quantile(rt2[,'ADE'],0.5)) {apa=min(ap[,'ADE']);ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']} else {apa=max(ap[,'ADE']);ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p']}} else {apa=0;ape=1}
        indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
        ip=append(ip,ape)} else {        ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ACME']))) {if (median(ap[,'ACME'])< quantile(rt2[,'ACME'],0.5)) {apa=min(ap[,'ACME']);ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']} else {apa=max(ap[,'ACME']);ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']}} else {apa=0;ape=1}
        indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
        ip=append(ip,ape)}
      rn=append(rn,hoi[,3][which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]]) #change this...
      rn2=append(rn2,hoi[,1][which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]])
      
      Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
      myData <- data.frame(matrix = Matrix)
      colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
      
      
    }}} else if (switch==0) {
      
      # indir:
      Mediator_ok=colnames(tv_all)[9:28][colnames(tv_all)[9:28] %in% names(table(hoi[1:dim(hoi)[1],c(2)]))]
      for (i in 1:7) {for (j in 1:length(Mediator_ok)) {
        if (z=='dir') {
          ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ADE']))) {if (median(ap[,'ADE'])< quantile(rt2[,'ADE'],0.5)) {apa=min(ap[,'ADE']);ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']} else {apa=max(ap[,'ADE']);ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p']}} else {apa=0;ape=1}
          indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)} else {        ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ACME']))) {if (quantile(ap[,'ACME'],0.50) < quantile(rt2[,'ACME'],0.5)) {apa=min(ap[,'ACME']);ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']} else 
          {apa=max(ap[,'ACME']);ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']}} else {apa=0;ape=1}
          indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)}
        rn=append(rn,hoi[,2][which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j])[1]]) #change this...
        rn2=append(rn2,hoi[,1][which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j])[1]])
        
        Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
        myData <- data.frame(matrix = Matrix)
        colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
        
        
      }}} else if (switch==2) {
        Treatment=colnames(tv_all)[9:28]; # These names are a bit mixed, by the idea is ok.
        Mediator_ok=Outcome[Outcome %in% names(table(hoi[1:dim(hoi)[1],c(3)]))]
        # df = data.frame(matrix("", nrow = length(Treatment), ncol = length(Mediator_ok))) 
        
        for (i in 1:length(Treatment)) {for (j in 1:length(Mediator_ok)) {
          if (z=='dir') {
            ap=rt2[which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ADE']))) {if (median(ap[,'ADE'])<quantile(rt2[,'ADE'],0.5)) {apa=min(ap[,'ADE']);ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']} else {apa=max(ap[,'ADE']);ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p'];}} else {apa=0;ape=1}
            indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
            ip=append(ip,ape)} else {        ap=rt2[which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; if (!is.na(median(ap[,'ACME']))) {if (median(ap[,'ACME']) < quantile(rt2[,'ACME'],0.5)) {apa=min(ap[,'ACME']);ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']} else {apa=max(ap[,'ACME']);ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']}} else {apa=0;ape=1}
            indir=append(indir,apa) #or c(1) hoi 1 or 5 (5 is orig)
            ip=append(ip,ape)}
          
          rn=append(rn,hoi[,3][which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]]) #change this...
          rn2=append(rn2,hoi[,2][which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]])
          Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
          myData <- data.frame(matrix = Matrix)
          colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
          
          
        }}} # You need three of these, yes :) ;print(ap[,'ADE']) print(rownames(ap[ap[,'ADE']==min(ap[,'ADE']),]))
  
  tot=cbind(rn2,rn,indir) #or indir or dir
  tot=tot[!is.na(tot[,1]),]
  tot=as.data.frame(tot)#
  

  tot[,3]=as.numeric(tot[,3])
  
  library(reshape2)
  jops=dcast(tot, rn2~rn, value.var='indir')
  jops[is.na(jops)]=0
  rownames(jops)=jops[,1]
  jops=jops[,2:dim(jops)[2]]
  jops=as.data.frame(jops)
  jopsr=matrix(as.numeric(unlist(jops)),nrow=dim(jops)[1],ncol=dim(jops)[2])
  colnames(jopsr)=colnames(jops);rownames(jopsr)=rownames(jops)
  
  # print(dim(jopsr)[1] == dim(myData)[1]);print(dim(jopsr)[2] == dim(myData)[2])
  
  if (sum(!rownames(myData) %in% rownames(jopsr))>0) {
    to_df=rownames(myData)[!rownames(myData) %in% rownames(jopsr)]
    jopsr=rbind(jopsr,myData[to_df,]); jopsr=jopsr[rownames(myData),]}
  if (sum(!colnames(myData) %in% colnames(jopsr))>0) {
    to_df=colnames(myData)[!colnames(myData) %in% colnames(jopsr)]
    jopsr=cbind(jopsr,myData[,to_df]); jopsr=jopsr[,colnames(myData)]}
  # 
  

  
  tot=cbind(rn2,rn,ip)
  tot=tot[!is.na(tot[,1]),]
  tot=as.data.frame(tot)
  # if (neg=='no' | neg=='yes' ) {tot=tot[uim[,2],]} else {tot=tot}
  tot[,3]=as.numeric(tot[,3])
  
  library(reshape2)
  jopsa=dcast(tot, rn2~rn, value.var='ip')
  jopsa[is.na(jopsa)]=0
  rownames(jopsa)=jopsa[,1]
  jopsa=jopsa[,2:dim(jopsa)[2]]
  jopsra=matrix(as.numeric(unlist(jopsa)),nrow=dim(jopsa)[1],ncol=dim(jopsa)[2])
  colnames(jopsra)=colnames(jopsa);rownames(jopsra)=rownames(jopsa)
  colnames(jopsra)=colnames(jopsa);rownames(jopsra)=rownames(jopsa)
  
  if (sum(!rownames(myData) %in% rownames(jopsra))>0) {
    to_df=rownames(myData)[!rownames(myData) %in% rownames(jopsra)]
    jopsra=rbind(jopsra,myData[to_df,]); jopsra=jopsra[rownames(myData),]}
  if (sum(!colnames(myData) %in% colnames(jopsra))>0) {
    to_df=colnames(myData)[!colnames(myData) %in% colnames(jopsra)]
    jopsra=cbind(jopsra,myData[,to_df]); jopsra=jopsra[,colnames(myData)]}
  
  
  # df
  
  
  if (switch==1) {
    #for direct:

    
  } else if (switch==0) {
    #for indirect
    jopsra=jopsra[,groups[,'Abbreviation'][groups[,'Abbreviation'] %in% colnames(jopsra)]]

    jopsr=jopsr[,groups[,'Abbreviation'][groups[,'Abbreviation'] %in% colnames(jopsr)]] 
  } else if (switch==2) {
    jopsra=jopsra[groups[,'Abbreviation'],]
    jopsr=jopsr[groups[,'Abbreviation'],]

  } 
  
  setwd("C:/Users/patati/Desktop/Turku/R/") #check this if needed...
  hip1='transpose';pch.cex=2; #width = 5000;height=2000 width = 2500;height=4000 width = 4000;height=2500;
  ho=paste('PFAS vs. bas and lipids_for the hypo_basic_colors_stea', switch)
  if (dim(jopsr)[1]==7) {width = 4000;height=1500} else if (dim(jopsr)[1]==20) {width = 4000;height=2500} else if (dim(jopsr)[1]==36) {width = 2500;height=5000}
  resulta1=jopsr
  p.mat.a1=jopsra

  # With the neg. you do not put these:
  if (dim(resulta1)[2]==36) {
    Outcome=colnames(tv_covNS)[c(29:51,59:71)];
    Outcome=Outcome[c(1,23,2:22,24:length(Outcome))]
    resulta1=resulta1[,Outcome[Outcome %in% colnames(resulta1) ]];p.mat.a1=p.mat.a1[,Outcome[Outcome %in% colnames(p.mat.a1) ]] }
  for (i in 1:dim(resulta1)[1]) {for (j in 1:dim(resulta1)[2]) {if (resulta1[i,j]==0) {p.mat.a1[i,j]=0.5}}} 

  heps=abs(round((min(resulta1)),1)); hepsa=abs(round((max(resulta1)),1))
  heps2=abs(round((max(resulta1)-0.01),3));heps3=abs(round((min(resulta1)+0.01),3));
  
  if (hepsa-heps >=0 ) {resulta1[resulta1 >= heps2] = hepsa; resulta1[resulta1 <= -heps3] = -hepsa; heps=hepsa} else 
    if (hepsa-heps < 0 ) {resulta1[resulta1 <= -heps] = -heps; resulta1[resulta1 >= heps2] = heps}  

  path="C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/"; setwd(path) #check this if needed...
  jpeg(paste("Heatmap of high",date, mn,neg,".jpg"), width = width, height = height, quality = 100,pointsize = 14, res=300);# par( ps=ps)# par(cex.lab=90) 22 18
  # col = brewer.pal(n = 9, name = "YlOrRd")
  order="original"; range='orig';corre='no_renormaa'; type='full'; method='color';ga='All';gf='Female';gm='Male' #color square
  cl.offset=20;cl.length=7;cl.cex = 1.45;pch.cex=2.45;pch=2;cl.pos = 'r'; #cl.offset=2;cl.length=5;cl.cex = 1.3;pch.cex=1.95;pch=14;
  
  col=colorRampPalette(c('blue', 'white','orange'), alpha = TRUE)(150)
  if (neg=='no') {col=colorRampPalette(c( 'white','orange'), alpha = TRUE)(150)} else if (neg=='yes')
  {col=colorRampPalette(c('blue', 'white'), alpha = TRUE)(150)} else {col=col}
  

  corrplot(as.matrix(resulta1), type = type, order = order,method=method, p.mat=as.matrix(p.mat.a1), tl.col = "black", #sum(COL2('RdBu')=="#FF7417")
           cl.cex = cl.cex, pch.cex=pch.cex, pch.col='black',pch=pch,#pitikö vain pch lisätä pch väriin väriin... mystistä...'#FEE12B'
           sig.level = c(0.05),cl.pos = cl.pos, insig = "label_sig", cl.offset=cl.offset,cl.length=cl.length, #.001, .05, .2
           tl.srt = 90, diag = TRUE,col=col,is.corr = corr,col.lim=c(-heps,heps)) #only in age...0.001,col.lim=c(-heps,heps) #rev(COL2('RdBu')[25:(length(COL2('RdBu'))-25)]) col.lim=c(-1,1) col.lim=c(-heps,heps)
  #non were significant in neg... but after mody yes!
  
  dev.off(); eoh=paste("Heatmap of high",date, mn,neg,".jpg"); daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'))
  my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))
  
  return(list(resulta1,p.mat.a1))
}



Demographics=function(tv_all,Clini,Group) {
  # The gender (and all) groups (could be something else as well... you can expand... like the 'sick' groups)
  if (Group=='female') {cond=tv_all[,'SEX.1F.2M']==min(tv_all[,'SEX.1F.2M'])} else if 
  (Group=='male') {cond=tv_all[,'SEX.1F.2M']==max(tv_all[,'SEX.1F.2M'])} else if (Group=='All') {cond=rep(TRUE,dim(tv_all)[1])}
  tv_red=c(); tv_red=tv_all[cond,]; Clini2=Clini[rownames(tv_red),]
  
  # The demographics. Depending what is the aim of your study, you may need more here...:
  N=dim(tv_red)[1]
  AGE=median(tv_red[,'AGE']);AGEq1=quantile(tv_red[,'AGE'],0.25);AGEq3=quantile(tv_red[,'AGE'],0.75);
  BMI=median(tv_red[,'BMI']);BMIq1=quantile(tv_red[,'BMI'],0.25);BMIq3=quantile(tv_red[,'BMI'],0.75)
  PFAS=median(tv_red[,'PFAS']);PFASq1=quantile(tv_red[,'PFAS'],0.25);PFASq3=quantile(tv_red[,'PFAS'],0.75)
  HDL=median(Clini2[,'HDL']);HDLq1=quantile(Clini2[,'HDL'],0.25);HDLq3=quantile(Clini2[,'HDL'],0.75)
  #Sometimes the numbers are 'characters' in the table...:
  LDL=median(as.numeric(Clini2[,'LDL']));LDLq1=quantile(as.numeric(Clini2[,'LDL'],0.25))[2];LDLq3=quantile(as.numeric(Clini2[,'LDL'],0.75))[4] 
  
  SGmin=min(tv_red[,'Steatosis.Grade.0.To.3']);SGmax=max(tv_red[,'Steatosis.Grade.0.To.3'])
  FSmin=min(tv_red[,'Fibrosis.Stage.0.to.4']);FSmax=max(tv_red[,'Fibrosis.Stage.0.to.4'])
  NFmin=min(tv_red[,'Necroinflammation']);NFmax=max(tv_red[,'Necroinflammation'])
  HImin=min(tv_red[,'HOMA-IR']);HImax=max(tv_red[,'HOMA-IR']);
  HI=median(tv_red[,'HOMA-IR']);HIq1=quantile(tv_red[,'HOMA-IR'],0.25);HIq3=quantile(tv_red[,'HOMA-IR'],0.75)
  
  return(c(N,AGE,AGEq1,AGEq3,BMI,BMIq1,BMIq3,PFAS,PFASq1,PFASq3,HDL,HDLq1,HDLq3,LDL,LDLq1,LDLq3,
           SGmin,SGmax,FSmin,FSmax,NFmin,NFmax,HImin,HImax,HI,HIq1,HIq3))
}

Group='All';    d_all=Demographics(tv,Clini,Group);
Group='female'; d_female=Demographics(tv,Clini,Group);
Group='male';   d_male=Demographics(tv,Clini,Group)

d_totaali=t(rbind(d_all,d_female,d_male)); 
# For the rownames, I selected the outcome variables of Demographics and added '' t to them:
rownames(d_totaali)=c('N','AGE','AGEq1','AGEq3','BMI','BMIq1','BMIq3','PFAS','PFASq1','PFASq3','HDL','HDLq1','HDLq3','LDL','LDLq1','LDLq3',
                      'SGmin','SGmax','FSmin','FSmax','NFmin','NFmax','HImin','HImax','HI','HIq1','HIq3')

d_totaali


# For determining the sample sizes in each case:
#If you have case/control as >0/0 (homa.ir was converted as per 1.5), or then see the other function if you have the division
sample_size=function(NAFLD,Outcome,Group) { 
  if (Group=='Male') {NAFLDo=NAFLD[NAFLD[,'SEX.1F.2M']==2,]} else if (Group=='Female') {NAFLDo=NAFLD[NAFLD[,'SEX.1F.2M']==1,]} else if (Group=='All') {NAFLDo=NAFLD} 
  sample_data=c();n0=c();n1=c()
  for (i in 1:2) {
    if (i==1) 
    {SG0=NAFLDo[NAFLDo[,Outcome] == 0,];n0=dim(SG0)[1]; sample_data=append(sample_data,n0)} else if (i==2) {SG0=NAFLDo[NAFLDo[,Outcome] > 0,];n1=dim(SG0)[1];sample_data=append(sample_data,n1)}
  }
  return(sample_data)} 

# Yes other one too... for continuous variables, such as HOMA-IR and age (~ menopause)
sample_size2=function(NAFLD,Outcome,Group,sick_groupe) { 
  sample_data=c();n0=c();n1=c();NAFLDo=data.frame()
  for (i in 1:2) {
    if (i==1) {if (Group=='Male') {NAFLDo=NAFLD[!sick_groupe & NAFLD[,'SEX.1F.2M']==2,]} else if (Group=='Female') 
    {NAFLDo=NAFLD[!sick_groupe & NAFLD[,'SEX.1F.2M']==1,]} else if (Group=='All') {NAFLDo=NAFLD[!sick_groupe,]}; n0=dim(NAFLDo)[1];sample_data=append(sample_data,n0)} 
    else if (i==2) {if (Group=='Male') {NAFLDo=NAFLD[sick_groupe & NAFLD[,'SEX.1F.2M']==2,]} else if (Group=='Female') 
    {NAFLDo=NAFLD[sick_groupe & NAFLD[,'SEX.1F.2M']==1,]} else if (Group=='All') {NAFLDo=NAFLD[sick_groupe,]}; n1=dim(NAFLDo)[1];sample_data=append(sample_data,n1)}}
  return(sample_data)} 

#In case you do not have NAFLD data folder already......
NAFLD=cbind(tv[,1:28])
NAFLD[NAFLD[,c(5)]>0,5]=1;NAFLD[NAFLD[,c(6)]>0,6]=1;NAFLD[NAFLD[,c(7)]>0,7]=1;
NAFLD[NAFLD[,c(8)] <= 1.5,8]=0;NAFLD[NAFLD[,c(8)]>1.5,8]=1; 
colnames(NAFLD) <- gsub("-", ".", colnames(NAFLD))
colnames(NAFLD) <- gsub("/", ".", colnames(NAFLD))
colnames(NAFLD) <- gsub("11", "X11", colnames(NAFLD))
colnames(NAFLD) <- gsub("17", "X17", colnames(NAFLD))
colnames(NAFLD) <- gsub("#", ".", colnames(NAFLD))
colnames(NAFLD)[colnames(NAFLD)=='X17aOH.P4']='X17.aOHP4' #oh...

jappend=c(); 

Outcome='Steatosis.Grade.0.To.3';
Group='All'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group)); 
Group='Female';jappend=c(jappend,sample_size(NAFLD,Outcome,Group))
Group='Male'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group))

Outcome='Fibrosis.Stage.0.to.4'; 
Group='All'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group)) #
Group='Female';jappend=c(jappend,sample_size(NAFLD,Outcome,Group))
Group='Male'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group))

Outcome='Necroinflammation'; Group='All'; 
jappend=c(jappend,sample_size(NAFLD,Outcome,Group)) #not the very first though...
Group='Female';jappend=c(jappend,sample_size(NAFLD,Outcome,Group))
Group='Male'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group))

Outcome='HOMA.IR';# xlim=c(0.1,5.3)
Group='All';jappend=c(jappend,sample_size(NAFLD,Outcome,Group)) #not the very first though...# xlim=c(0.1,1.7)
Group='Female';jappend=c(jappend,sample_size(NAFLD,Outcome,Group))
Group='Male'; jappend=c(jappend,sample_size(NAFLD,Outcome,Group))

matrix(unlist(jappend),ncol=8) #rows are all female and male and columns case vs. control (all outcomes...)

# Determining the mean (+-Q) in all, female and male
mean_q1q3=function(Group) {
  cM=c();csd=c();tot=c();tot2=c();tot3=c()
  if (Group=='Female') {cond=tv_all[,'Gender']==min(tv_all[,'Gender'])} else if (Group=='Male') {cond=tv_all[,'Gender']==max(tv_all[,'Gender'])} else if (Group=='All') {cond=rep(TRUE,dim(tv_all)[1])}
  tv_red=c(); 
  tv_red=tv[cond,]
  #Standard values did not five erros # hep=colnames(X)[!colnames(X) %in% c( "Benzylparaben" ,"Methylparaben")] # X=X[,hep]
  M <- tv_red[,Mediator]  #
  cM=round(apply(M, 2, median,na.rm = TRUE),0)
  quants <- c(0.25,0.75)
  csd=round(apply( M , 2 , quantile , probs = quants , na.rm = TRUE ),0)
  tot=rbind(cM,csd)
  tot2=cbind(tot2,tot)
  return(tot2)}

totQ=mean_q1q3('All');femQ=mean_q1q3('Female');menQ=mean_q1q3('Male')
tot3=cbind(t(totQ),t(femQ),t(menQ)); print(tot3)

# I wanted to check the steroids with highest ACMEs between healthy (all, all) and sick (all, all).
# I needed to have the cutoffs and ways to see the overlaps. For that, I developed a function called 'the_combos'
#Some data is needed... :)
path="C:/Users/patati/Desktop/Turku/R/hypo_basic/Tiedostot/"; setwd(path)
files <- list.files(pattern="*.RData")
ldf <- lapply(files, load)
list_of_files <- list() #create empty list
# Loop through the files
for (i in files) {list_of_files[[i]] <- get(load(paste0("", i)))}  #add files to list position
# https://www.reddit.com/r/Rlanguage/comments/nq773b/reading_multiple_rdata_files_into_a_list/
names(list_of_files) <- files #https://stackoverflow.com/questions/38643000/naming-list-elements-in-r
library(stringr)

the_combos=function(list_of_files,Group,cond) { #cond='' vastaa kaikkia
  #General categories of females or males (without 'All, 'MASLD, and 'Menopause')
  u <- names(list_of_files)
  a <- Group #note the writing, yes with " " in between. " female"  or " male" or " all"
  ie=grep(a,u,fixed=TRUE); u2=u[ie]
  del=c(grep("All",u2,fixed=TRUE),grep("MASLD",u2,fixed=TRUE),grep("Menopause",u2,fixed=TRUE))
  yl=1:length(u2); lop=yl[!yl %in% del]
  u=u2[lop] #general male
  ie=grep(cond,u,fixed=TRUE); u=u[ie]
  a <- "sick"; ie=grep(a,u,fixed=TRUE); u3=u[ie]# sick ones, male or females
  u_sick=list_of_files[u3]#https://www.tutorialspoint.com/how-to-extract-strings-that-contains-a-particular-substring-in-an-r-vector
  a='healthy';ie=grep(a,u,fixed=TRUE);u4=u[ie]
  u_healthy=list_of_files[u4] # u_healthy=list_of_files[grep(a,u,fixed=TRUE)] 
  tcross=function(u_sick) { 
    all_names = c(); i=1
    for (i in (1:length(u_sick))) { #length(u_sick) is 18
      us=u_sick[[i]]
      aux=c();
      if (dim(us)[1]>200) {aux=200} else {aux=dim(us)[1]}
      # plot(1:aux,as.numeric(us[1:aux,1]))
      values=(1:(length(as.numeric(us[,1]))-1))
      coo=c(); z=0
      for (z in values) {coo=append(coo,abs(us[z,1]-us[(z+1),1]))}  
      pss=which(coo>quantile(coo,0.95));ro=round(length(coo)/3)# pss[pss<ro]#round(length(which(coo>quantile(coo,0.95)))/2)]
      dpp=diff(pss[pss<ro]) #https://stackoverflow.com/questions/13602170/how-do-i-find-the-difference-between-two-values-without-knowing-which-is-larger
      dpp_sort=sort(dpp,decreasing = TRUE)
      if (length(dpp_sort)<5) { for_comp=length(dpp)+1} else {
        if (sum(dpp_sort[1:5]>5)==5) {cf=dpp_sort[5];cff=which(dpp>cf)[1]} else {cf=dpp_sort[2];cff=which(dpp>cf)[1]}
        cff=which(dpp>(cf-1))[1]
        for_comp=pss[cff]; }
      
      if (aux<30) {for_comp  =max(which(as.vector(us[,1]>0)))}
      if (aux>30) {if (max(pss[pss<ro])<30) (for_comp=30)} #due the small amount of good ones that can be like 4...
      
      rt2=us[1:for_comp,]; j=0
      hoi=c();for (j in 1:dim(rt2)[1]) {hoi=append(hoi,scan(text=rownames(rt2)[j], what=""))}
      hoi=as.data.frame(matrix(hoi, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
      hoi[,2] <- gsub("\\.", "-",  hoi[,2] ) #:)
      xz=round(quantile(table(hoi[,2]),0.25)); if (xz<2) {xz=0} else {xz=xz} #let's start with 25% and if not ok go to like 5%
      names=c();names=names(table(hoi[,2])[table(hoi[,2])>xz]) 
      # print(all_names)
      all_names=append(names,all_names)
    }
    return(sort(table(all_names),decreasing = TRUE))  
  }
  tc_sick=tcross(u_sick);tc_healthy=tcross(u_healthy) # table(the_cross)# hist(table(the_cross),breaks=10)
  cae1=as.numeric(names(sort(table(tc_sick),decreasing = TRUE)))[1];cae2=as.numeric(names(sort(table(tc_sick),decreasing = TRUE)))[2]
  if (max(tc_sick)==cae1 | max(tc_sick)==cae2)
    cfn=cae2-1 #sometimes as with females no differences, i.. tc_sick;rev(as.numeric(names(table(tc_sick))))[2]-1#
  if (is.na(cfn)) {steroid_sick=names(tc_sick)} else {steroid_sick=names(tc_sick[tc_sick>(cfn)])}
  cae1=as.numeric(names(sort(table(tc_healthy),decreasing = TRUE)))[1];cae2=as.numeric(names(sort(table(tc_healthy),decreasing = TRUE)))[2]
  if (max(tc_healthy)==cae1 | max(tc_healthy)==cae2)
    cfn=cae2-1 
  if (is.na(cfn)) {steroid_healthy=names(tc_healthy)} else {steroid_healthy=names(tc_healthy[tc_healthy>(cfn)])}
  # https://stackoverflow.com/questions/45271448/r-finding-intersection-between-two-vectors
  tbe=intersect(steroid_healthy, steroid_sick)
  totaali_sh_all=steroid_sick[!steroid_sick %in% tbe] #https://www.geeksforgeeks.org/difference-between-two-vectors-in-r/ 
  return(list(steroid_sick,tbe,totaali_sh_all)) } #"17a-OHP4" "DHT"      "DOC"      'P4'

Group = ' all'; cond='';all_all=the_combos(list_of_files,Group,cond) #ekat allit ('All...') oli deletoitu, yes, ja käytetty vain spesifisiä alleja...
Group = ' female'; cond='';female_all=the_combos(list_of_files,Group,cond) 
Group = ' male'; cond='';male_all=the_combos(list_of_files,Group,cond) 
Group = ' all'; cond='Steatosis';all_steatosis=the_combos(list_of_files,Group,cond) 
Group = ' female'; cond='Steatosis';female_steatosis=the_combos(list_of_files,Group,cond) 
Group = ' male'; cond='Steatosis';male_steatosis=the_combos(list_of_files,Group,cond) 
Group = ' all'; cond='Fibrosis';all_Fibrosis=the_combos(list_of_files,Group,cond) 
Group = ' female'; cond='Fibrosis';female_Fibrosis=the_combos(list_of_files,Group,cond) 
Group = ' male'; cond='Fibrosis';male_Fibrosis=the_combos(list_of_files,Group,cond) 
Group = ' all'; cond='Necroinflammation';all_Necroinflammation=the_combos(list_of_files,Group,cond) 
Group = ' female'; cond='Necroinflammation';female_Necroinflammation=the_combos(list_of_files,Group,cond) 
Group = ' male'; cond='Necroinflammation'; male_Necroinflammation=the_combos(list_of_files,Group,cond) 
Group = ' all'; cond='HOMAIR'; all_HOMAIR=the_combos(list_of_files,Group,cond) 
Group = ' female'; cond='HOMAIR'; female_HOMAIR=the_combos(list_of_files,Group,cond) 
Group = ' male'; cond='HOMAIR'; male_HOMAIR=the_combos(list_of_files,Group,cond) 

pottees=c(all_all[3],female_all[3],male_all[3],
          all_steatosis[3],female_steatosis[3],male_steatosis[3],
          all_Fibrosis[3],female_Fibrosis[3],male_Fibrosis[3],
          all_Necroinflammation[3],female_Necroinflammation[3],male_Necroinflammation[3],
          all_HOMAIR[3],female_HOMAIR[3],male_HOMAIR[3])


# So this will give the most common steroids in all cases compared to all cases in all the subjects (all, female, male)
table(unlist(pottees))[rev(order(table(unlist(pottees))))]

# For comparing the PFAS/steroid/BA (or lipid) in healthy and sick mediation:
# Load all the variables in the folder:
setwd("C:/Users/patati/Desktop/Turku/R/hypo4/Tiedostot/") #check this if needed...
files <- list.files(pattern="*.RData")
ldf <- lapply(files, load)
list_of_files <- list() #create empty list
# Loop through the files:
for (i in files) {list_of_files[[i]] <- get(load(paste0("", i)))}  #add files to list position
names(list_of_files) <- files #ht

comp_med_two=function(list_of_files) { #I made this a function, since this kind of cross comparison could be handy also in other contexts
  health=c();sickness=c()
  for (i in 1:length(names(list_of_files))) if (str_detect(names(list_of_files)[i],'healthy')) {health=append(health,list_of_files[i])} else if (str_detect(names(list_of_files)[i],'sick')) {sickness=append(sickness,list_of_files[i])} 
  health2=c();sickness2=c()
  i=0;for (i in 1:length(c(health))){health[[i]][rev(order(health[[i]][,1])),];health2=rbind(health2,health[[i]][1:10,])}
  i=0;for (i in 1:length(c(sickness))){sickness[[i]][rev(order(sickness[[i]][,1])),];sickness2=rbind(sickness2,sickness[[i]][1:10,])}
  health2=cbind(rownames(health2),health2);sickness2=cbind(rownames(sickness2),sickness2)
  colnames(health2)=c('Mediation','ACME', 'd0.p', 'd0.ci_l','d0.ci_u','ADE', 'z0.p', 'z0.ci_l','z0.ci_u','Proportion Mediated', 'n1.p','n.ci_l','n1.ci_u','Total Effect','tau.p','tau.ci_l','tau.ci_u')
  colnames(sickness2)=c('Mediation','ACME', 'd0.p', 'd0.ci_l','d0.ci_u','ADE', 'z0.p', 'z0.ci_l','z0.ci_u','Proportion Mediated', 'n1.p','n.ci_l','n1.ci_u','Total Effect','tau.p','tau.ci_l','tau.ci_u')
  rownames(health2)=str_replace_all(rep( names(health),each = 10), ".RData", "")
  rownames(sickness2)=str_replace_all(rep( names(sickness),each = 10), ".RData", "")

  hoi=c(); hoi=scan(text=health2[,1], what=""); hoi=as.data.frame(matrix(hoi, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
  colnames(hoi)=c('Contaminants','Steroids','Bile Acids or Lipids','Desig.'); hoi_healthy=hoi[,c('Contaminants','Steroids','Bile Acids or Lipids')]
  hoi=c(); hoi=scan(text=sickness2[,1], what=""); hoi=as.data.frame(matrix(hoi, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
  colnames(hoi)=c('Contaminants','Steroids','Bile Acids or Lipids','Desig.');
  hoi_sick=hoi[,c('Contaminants','Steroids','Bile Acids or Lipids')] ## https://stats.stackexchange.com/questions/282155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct
  return(list(hoi_sick,hoi_healthy))}

cmt=comp_med_two(list_of_files);
hoi_sick=cmt[[1]];hoi_healthy=cmt[[2]]

# So the differences between contaminants (in the sick vs. healthy 'mediation') are:
table(hoi_sick[,1])[rev(order(table(hoi_sick[,1])))];table(hoi_healthy[,1])[rev(order(table(hoi_healthy[,1])))]
# Differences Between Steroids
table(hoi_sick[,2])[rev(order(table(hoi_sick[,2])))];table(hoi_healthy[,2])[rev(order(table(hoi_healthy[,2])))]
# Differences Between BAs/Lipids
table(hoi_sick[,3])[rev(order(table(hoi_sick[,3])))];table(hoi_healthy[,3])[rev(order(table(hoi_healthy[,3])))]


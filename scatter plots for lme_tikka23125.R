# This should be as good as it gets... eli  maksimilla vedetään... eli pitäis olla ok, sillä skaalattu vastaa korrelaatiota tss. skaalaus vielä miehiin...
the_fun_figs=function(tv_all,Group,hopiu,aa,bb,fn) { #  ok,aa,bb
  if (Group=='male') {NAFLDo=tv_all[tv_all[,'Gender']==max(tv_all[,'Gender']),]} else if (Group=='female') 
  {NAFLDo=tv_all[tv_all[,'Gender']==min(tv_all[,'Gender']),]} else if (Group=='All') {NAFLDo=tv_all}
  SG0=NAFLDo[,c(2:dim(tv_all)[2])]
  #https://stackoverflow.com/questions/10688137/how-to-fix-spaces-in-column-names-of-a-data-frame-remove-spaces-inject-dots
  oknames=colnames(SG0)
  SG0=data.frame(SG0)
  colnames(SG0)
  colnames(SG0[,8:27]) <- gsub("-", ".", colnames(SG0[,8:27]))
  colnames(SG0[,8:27]) <- gsub("/", ".", colnames(SG0[,8:27]))
  hesh=c()
  xnam <- colnames(SG0)[c(4:7)]
  Treatment=colnames(tv_all)[52:58];
  y <- Treatment;#colnames(SG0)[c(70:76)]
  TreatmentN=Treatment
  
  # Group='All'
  j=1;i=1;p.val=c()
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      Treatment2=Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")# setting up the sub directory
      sub_dir <-Treatment #paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted",Group,Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else {
        jpeg(paste("Correlations plotted_alle",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      
      # equation1=function(x){coef(poissone)[2]*x+coef(poissone)[1]+coef(poissone)[3]}
      
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        # stat_function(fun=equation1,geom="line",color='black')+
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,5))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      
      print(a)
      dev.off()
    }}
  
  # getwd()
  
  j=1;i=1; rm(xnam,y)
  xnam <- colnames(SG0)[c(2)]
  y <- TreatmentN#colnames(SG0)[c(70:76)]
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")
      # main_dir <- "C://Users//patati//Desktop//Turku//R//males2/"# setting up the sub directory
      sub_dir <-Treatment#paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted",Group,Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else{
        jpeg(paste("Correlations plotted",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,4))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      
      print(a)
      dev.off()
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,p.val,rsadj))
    }}
  
  j=1;i=1; #rm(xnam,y)
  xnam <- colnames(SG0)[c(3)];y <- TreatmentN#colnames(SG0)[c(70:76)]
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      
      # #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      # rm(r,p.val,rsadj)
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      Treatment2=Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")
      # main_dir <- "C://Users//patati//Desktop//Turku//R//males2/"# setting up the sub directory
      sub_dir <-Treatment#paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted_alle",Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else{
        jpeg(paste("Correlations plotted",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        # stat_function(fun=equation1,geom="line",color='black')+
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,5))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }}
  
  
  j=1;i=1; #rm(xnam,y)
  xnam <- TreatmentN#colnames(SG0)[c(70:76)]#paste("x", 1:25, sep="") 28:length(colnames(SG0)
  y = colnames(SG0[,8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      rm(r,p.val,rsadj)
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      Treatment2=Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")
      sub_dir <-Treatment#paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted",Group,Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else{
        jpeg(paste("Correlations plotted",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        # stat_function(fun=equation1,geom="line",color='black')+
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,4))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      
    }}
  
  # Eli 'kaksi' vielä tarvitaan...
  # 1) BA/lipid=covar ja steroidit, ja 2) steroid=covar
  #tai oikeastaan ehkä... steroid=covar ja steroid = BA/lipid
  
  # # 1) (BA/lipid=covar ja steroids)
  #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
  
  # # 1)  steroids=BA/lipid
  j=1;i=1; #rm(xnam,y)
  xnam <- c(x3,x6) # Group='All'
  y <- c(colnames(SG0[,8:27])); #colnames(SG0)[c(4:7)]
  
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      rm(r,p.val,rsadj)
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      
      Treatment2=Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")
      # main_dir <- "C://Users//patati//Desktop//Turku//R//males2/"# setting up the sub directory
      sub_dir <-Treatment#paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted",Group,Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else{
        jpeg(paste("Correlations plotted",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,5))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,p.val,rsadj))}}
  
  # ja 2) steroid = covar
  j=1;i=1; #rm(xnam,y)
  xnam <- c('AGE','BMI',colnames(SG0)[c(4:7)]); 
  y <- c(colnames(SG0[,8:27])) # Group='All'
  
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh=hopiu[hopiu[,1]==y[j] & hopiu[,2]==xnam[i] & hopiu[,3]==Group,]
      
      # #printtaus tässä...
      jeps=SG0#
      rm(r,p.val,rsadj)
      r=as.numeric(hösh[4][1,])
      p.val=as.numeric(hösh[5][1,])
      rsadj=as.numeric(hösh[6][1,])
      colnames(jeps)=colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment=as.character(hösh[2][1,])
      Mediator=as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      
      if (Mediator=="T-Epi-T") {Mediator[Mediator=="T-Epi-T"]="T/Epi-T"}
      
      Treatment2=Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR']='HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn,Group,"/"),collapse="")
      # main_dir <- "C://Users//patati//Desktop//Turku//R//males2/"# setting up the sub directory
      sub_dir <-Treatment#paste0(c("\\",Treatment),collapse="")#Treatment2[i] # check if sub directory exists: #https://www.geeksforgeeks.org/r-check-if-a-directory-exists-and-create-if-it-does-not/
      if (file.exists(file.path(main_dir, sub_dir))){setwd(file.path(main_dir, sub_dir))} else {dir.create(file.path(main_dir, sub_dir)); setwd(file.path(main_dir, sub_dir))}
      if (Mediator!="T/Epi-T") {jpeg(paste("Correlations plotted",Group,Treatment2,Mediator,".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);} else{
        jpeg(paste("Correlations plotted_",Treatment2,'T.Epi.T',".jpg"), width = 1000, height = 1000, quality = 100,pointsize = 20, res=300);}
      xcx=jeps[,Treatment];
      ycy=jeps[,Mediator];väh=round(sd(ycy)/2,2)
      a=ggplot(jeps, aes(y=ycy, x=xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method="lm",col="black") + #https://ggplot2.tidyverse.org/reference/geom_smooth.html
        annotate("text", x=min(xcx),y=max(ycy),hjust=0,vjust=0, label=paste0("r = ", round(r,5))) +
        annotate("text", x=min(xcx),y=max(ycy)-väh,hjust=0,vjust=0, label=paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,p.val,rsadj))
    }}
  
  hesa=hesh
  hoi=as.data.frame(hesh)
  main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn),collapse="")
  setwd(main_dir)
  
  return(list(hopiu))
}

huus2=function(tv_all,hopiu) {
  huus=c();heijaa=c('All','female','male'); 
  for (hrt in 1:3) {
    huus=append(huus,the_fun_figs(tv_all,heijaa[hrt],hopiu,aa,bb,fn))}
  return(huus)}

aa=-0.5; bb=0.5; adj='nook'; sig.level=c(.001,0.01, 0.05)
fn='metabnorm//covScaled//non_fdr//'#
metanorm_S_non_fdr=huus(tv_covscl,adj,sig.level) # This was already done above
metanorm_S_fdr_non_tot=ldply(metanorm_S_non_fdr, data.frame) #Making the above ok for the next:
non_fdr_check=huus2(tv_covscl,metanorm_S_fdr_non_tot) # This plots all the combinations, so it takes some time
# Now I'll be just showing some examples, but this works and if you drive this, you can find all the combos in the folders.
# E.g.: ".../RWork/metabnorm/covScaled/non_fdr/'Subject'/Hexcer/Correlations plotted All Hexcer S .jpg" 
# fyi: do the 'subject' folders (all, male, fem.) separately
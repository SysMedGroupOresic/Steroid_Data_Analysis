the_funal=function(tv,Group,ok,fn,adj,sig.level,sick,sick_group,joo) { #  ok,aa,bb
  tv=tv_covscl
  if (Group=='male') {NAFLDo=tv[tv[,'Gender']==max(tv[,'Gender']),]} else if (Group=='female') 
  {NAFLDo=tv[tv[,'Gender']==min(tv[,'Gender']),]} else if (Group=='All') {NAFLDo=tv}
  SG0=NAFLDo[,c(2:dim(tv)[2])]
  #https://stackoverflow.com/questions/10688137/how-to-fix-spaces-in-column-names-of-a-data-frame-remove-spaces-inject-dots
  oknames=colnames(SG0)
  SG0=data.frame(SG0)
  colnames(SG0)
  colnames(SG0[,8:27]) <- gsub("-", ".", colnames(SG0[,8:27]))
  colnames(SG0[,8:27]) <- gsub("/", ".", colnames(SG0[,8:27]))
  hesh=c()
  
  Treatment=colnames(tv_all)[52:58];
  Mediator=colnames(tv_all)[9:28];
  Outcome=colnames(tv_all)[c(29:51,59:71)];
  
  xnam <- colnames(SG0)[c(4:7)]
  Treatment2=Treatment
  y <- Treatment2 
  hoesh=c()
  
  j=1;i=1;p.val=c()
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All') 
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] #https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])

      jeps=SG0# 
      r=as.numeric(hoesh[4]) #This was previuosly 'hösh', but for some reason it started to not work. Suom. tää oli aikasemmin 'hösh', mutta sitten tuli ongelmia. Hi hii.
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)
     
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))
    }}
  

  j=1;i=1; rm(xnam,y)
  xnam <- colnames(SG0)[c(2)]
  y <- Treatment2#colnames(SG0)[c(70:76)]
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All') 
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # Some pondering what to put as p values:
      # uh=c();uh=summary(poissone)$fstatistic # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      # p.val <- pf(uh[1], df1 = uh[2], df2 = uh[3],lower.tail=F)
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])
      
      jeps=SG0# 
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)
      
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}
  
  j=1;i=1; rm(xnam,y)
  xnam <- colnames(SG0)[c(3)];y <- Treatment2#colnames(SG0)[c(70:76)]
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All') 
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} 
      #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # fmla <- as.formula(paste(paste(c(colnames(SG0[,8:27])[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))
      uh=c();uh=summary(poissone)$fstatistic # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])
      jeps=SG0# 
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)
      
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}
  

  j=1;i=1; rm(xnam,y)
  xnam <- Treatment2
  y = colnames(SG0[,8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All') 
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) 
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # 
      uh=c();uh=summary(poissone)$fstatistic # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])
      jeps=SG0# 
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}

  # # 1)  steroids=BA/lipid
  
  # if (Group!='All') {
  j=1;i=1; rm(xnam,y)
  xnam <- c(x3,x6) # Group='All'Treatment2
  y <- c(colnames(SG0[,8:27])); #colnames(SG0)[c(4:7)]
  
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # j=10
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All')
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();
      p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # fmla <- as.formula(paste(paste(c(colnames(SG0[,8:27])[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))
      uh=summary(poissone)$fstatistic # https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
      p.vala <- pf(uh[1], df1 = uh[2], df2 = uh[3],lower.tail=F)
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])

      jeps=SG0# 
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)

      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}

  # ja 2) steroid = covar
  j=1;i=1; rm(xnam,y)
  xnam <- c('AGE','BMI',colnames(SG0)[c(4:7)]); 
  y <- c(colnames(SG0[,8:27])) # Group='All'
  
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All') 
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # fmla <- as.formula(paste(paste(c(colnames(SG0[,8:27])[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])

      jeps=SG0# 
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)
      
      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}

  # ja 2) steroid = covar
  j=1;i=1; rm(xnam,y)
  xnam <- c(x3,x6) # Group='All'Treatment2
  y <- colnames(tv_all)[52:58];

  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      if (Group!='All')  {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE'), collapse= "+")))} else if (Group=='All')
      {fmla <- as.formula(paste(paste(c(y[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))} #https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
      poissone=lm( fmla, data=SG0) # anova(poissone);# poissone
      p.val=c();p.val <- anova(poissone)$'Pr(>F)'[1]
      ps=summary(poissone);
      pss=ps[[4]] # fmla <- as.formula(paste(paste(c(colnames(SG0[,8:27])[j]," ~ "), collapse= ""), paste(c(xnam[i],'BMI','AGE','Gender'), collapse= "+")))
      hoesh=c(y[j],xnam[i],Group,pss[2,1],pss[2,4],pss[2,2])

      jeps=SG0#
      r=as.numeric(hoesh[4])
      p=as.numeric(hoesh[5])
      rsadj=as.numeric(hoesh[6])
      colnames(jeps)=colnames(tv)[2:dim(tv)[2]]
      Treatment=hoesh[2]
      Mediator=hoesh[1]
      rm(hoesh)

      hesh=rbind(hesh,c(y[j],xnam[i],Group,r,pss[2,4],rsadj))}}
  
  
    
  hesa=hesh
  hoi=as.data.frame(hesh)
  hopiu=hoi
  colnames(hopiu)=c('y','x','Gender','r','p','var_x')
  colnames(hoi)=c('y','x','Gender','r','p','var_x')
  
  #This in case you want to print to your local computer: ... :)
  # main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//",fn),collapse="")
  # setwd(main_dir)
  
  meds=names(table(hoi[,1]))[!names(table(hoi[,1])) %in% c(x3,x5,x6)]
  covas=c('Steatosis.Grade','Fibrosis.Stage','Necroinflammation','HOMA.IR','AGE','BMI')
  
  if (adj=='ok') {
    # p.adjust(p=hopiu[,5], method = 'BH', n = length(hopiu[,5]))
    hoi[,5]=p.adjust(p=hopiu[,5], method = 'BH', n = length(hopiu[,5]))
    hopiu[,5]=p.adjust(p=hopiu[,5], method = 'BH', n = length(hopiu[,5]))
  }
  
  if (ok=='big') {
    
    rsa=c();joi=c()
    
    # Eli 'kaksi' vielä tarvitaan...
    # 1) BA/lipid=covar ja steroidit, ja 2) steroid=covar

    meds=names(table(hoi[,1]))[!names(table(hoi[,1])) %in% c(x3,x5,x6)]
    covas=c('Steatosis.Grade','Fibrosis.Stage','Necroinflammation','HOMA.IR','AGE','BMI')
    
    c1=hoi[,2] %in% covas
    c2=hoi[,1] %in% meds
    hyy=c1 & c2
    m1=hoi[hyy,]
    colnames(m1)=c('y','x','Gender','r','p','var_x') #c('y','x','Gender','r','p','radj')
    c1=hoi[,2] %in% c(x3,x6); c2=hoi[,1] %in% meds
    hyy=c1 & c2; m2=hoi[hyy,]
    colnames(m2)=c('y','x','Gender','r','p','var_x') # hist(as.numeric(m2[,6]),breaks=50)
    joi=rbind(m1,m2)
    i=4;rs=c()
    
    for (i in 4:5) {
      rs=joi[,c(1,2,i)] # rs=data.frame(rs)
      rs=reshape(rs,idvar="x",timevar="y",direction="wide")
      rownames(rs)=rs[,1]
      rs=rs[,-1]

      library(stringr)
      colnames(rs)=str_sub(colnames(rs),3,-1)
      
      colnames(rs) <- gsub("\\.", "-", colnames(rs))
      colnames(rs) <- gsub("X11", "11", colnames(rs))
      colnames(rs) <- gsub("X17", "17", colnames(rs))
      colnames(rs)[colnames(rs)=="T-Epi-T"]="T/Epi-T"
      
      rownames(rs)[rownames(rs)=="Steatosis.Grade"]="Steatosis Grade"
      rownames(rs)[rownames(rs)=="Fibrosis.Stage"]="Fibrosis Stage"
      rownames(rs)[rownames(rs)=="HOMA.IR"]="HOMA-IR"
      covas[covas=="Steatosis.Grade"]="Steatosis Grade"
      covas[covas=="Fibrosis.Stage"]="Fibrosis Stage"
      covas[covas=="HOMA.IR"]="HOMA-IR"
      
      heps=c(groups[,2]) #check that you have driven the steroid data vis file...
      heps[heps=="17aOH-P4"]="17a-OHP4"
      cme1=match(heps,colnames(rs))
      cme2=match(c(covas,x3,x6),rownames(rs))
      rs=rs[cme2,cme1]
      rsa=rbind(rsa,rs) }
    
    
    rs1a=rsa[1:dim(rs)[1],];
    rs2a=rsa[(dim(rs1a)[1]+1):(dim(rs1a)[1]+dim(rs1a)[1]),]
  
    rs1=rs1a;rs2=rs2a
    rownames(rs2)=str_sub(rownames(rs2), end = -2)
    rownames(rs1) <- gsub("\\.", " ", rownames(rs1))
    rownames(rs2) <- gsub("\\.", " ", rownames(rs2))
    rownames(rs1)[rownames(rs1)=="HOMA IR"]="HOMA-IR";rownames(rs2)[rownames(rs2)=="HOMA IR"]="HOMA-IR"
    
    rownames(rs1)[rownames(rs1)=="Gender"]="HOMA-IR";rownames(rs2)[rownames(rs2)=="HOMA IR"]="HOMA-IR"
    
    rango = function(x,mi,ma) {(ma-mi)/(max(x)-min(x))*(x-min(x))+mi}
    rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
    rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))
    # rs1=rango(rs1,-0.5,0.5) #check this if needed
  
    rs1=as.matrix(rs1)
    rs2=as.matrix(rs2)
    
        rs1[rs1>0.5]=0.5;rs1[rs1 < -0.5]=-0.5;

    
    width=2500; height=4400
    order="original"; range='orig';corre='no_renorm'; type='full'; method='color';#ga='All';gf='Female';gm='Male' #color square
    cl.offset=1.0;cl.length=15;cl.cex = 1.09;pch.cex=1.09;pch=11;cl.pos = 'n';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
    ho=Group;hip1='BAs_lipids_as_y vs. steroids_as_x'

    # https://www.rdocumentation.org/packages/corrplot/versions/0.94/topics/corrplot
    # Oh! https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
    # https://r4ds.had.co.nz/graphics-for-communication.html#figure-sizing

#I have driven these separately for html: 
    # for that you need:     
    jpeg(paste("Linear Model Estimate Plot ofees5",hip1,Group,".jpg"), width = width, height = height, quality = 100,pointsize = 16, res=300);

    hepio=colorRampPalette(c('blue', 'white','orange'), alpha = TRUE)(150) #rev(COL2('RdBu')[25:(length(COL2('RdBu'))-25)])
    
    
    
    corrplot(rs1, type = type, order = order,method=method, p.mat=rs2, tl.col = "black", cl.cex = cl.cex,pch.cex=pch.cex,pch.col='black',pch=pch, ,sig.level = c(.001, .01, .05),cl.pos = cl.pos, 
             insig = "label_sig",cl.offset=cl.offset,cl.length=cl.length,tl.cex=0.5, tl.srt = 90, diag = TRUE, #tl.pos='n'
             col = colorRampPalette(c('blue', 'white','orange'), alpha = TRUE)(100) ,is.corr = FALSE,col.lim=c(-0.5,0.5));   #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html#change-color-spectra-color-legend-and-text-legend
    
    dev.off()  #,is.corr = FALSE
    eoh=paste("Linear Model Estimate Plot ofees5",hip1,Group,".jpg")
    daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'))
    my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))

    #

    #https://stackoverflow.com/questions/26574054/how-to-change-font-size-of-the-correlation-coefficient-in-corrplot
    #https://stackoverflow.com/questions/9543343/plot-a-jpg-image-using-base-graphics-in-r
   #oh, classical: https://forum.posit.co/t/r-markdown-html-document-doesnt-show-image/41629/2

    
  } else {
    
    rsa=c();rs1=c();rs2=c()
  
    c1=hoi[,1] %in% x5
    hoi[c1,] # This gives you the PFAS (x5) ok. Tää tulee suoraan tällä PFAS 7:lle ekalle
    c2=hoi[,1] %in% names(table(hoi[,1]))[!names(table(hoi[,1])) %in% c(x3,x5,x6)] #vaikeemman kautta
    hyy=c1 & c2
    hoi2=hoi[c2 | c1 ,] #Likewise...
    rownames(hoi2)=1:dim(hoi2)[1]
    hoi2=hoi2[1:182,]
    a=hoi2[1:42,1];b=hoi2[1:42,2]
    hoi2[1:42,]=cbind(b,a,hoi2[1:42,3:5])    
    hoi2=hoi2[,c(2,1,3:5)] 
    
    i=4;
    rse=c()
    for (i in 4:5) {
      
      rse=hoi2[,c(1,2,i)] 
      rse=rse[order(rse[,1]),]
      rs=reshape(rse,idvar="x",timevar="y",direction="wide")
      rownames(rs)=rs[,1]
      rs=rs[,-1]
      
      library(stringr) 
      colnames(rs)=str_sub(colnames(rs),3,-1)

      colnames(rs) <- gsub("\\.", "-", colnames(rs))
      colnames(rs) <- gsub("X11", "11", colnames(rs))
      colnames(rs) <- gsub("X17", "17", colnames(rs))
      colnames(rs)[colnames(rs)=="T-Epi-T"]="T/Epi-T"
      colnames(rs)[colnames(rs)=="T-E-T"]="T/Epi-T"
      colnames(rs)[colnames(rs)=="Steatosis-Grade"]="Steatosis Grade"
      colnames(rs)[colnames(rs)=="Fibrosis-Stage"]="Fibrosis Stage"
      colnames(rs)[colnames(rs)=="17aOH-P4"]="17a-OHP4"
      heps=c(covas,groups[,2])
      heps <- gsub("\\.", " ", heps)
      heps[heps=="HOMA IR"]="HOMA-IR"
      heps[heps=="17aOH-P4"]="17a-OHP4"
      ccc=match(heps,colnames(rs))

      rs=rs[,ccc]
      rsa=rbind(rsa,rs) 
    }
    
    rs1a=rsa[1:7,];
    rs2a=rsa[8:14,]
    rs1=rs1a;rs2=rs2a
    
    

    rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
    rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))
    # rs1=rango(rs1,-0.5,0.5) #check if needed

    rs1=as.matrix(rs1)
    rs2=as.matrix(rs2)
    
    rs1[rs1>0.4]=0.4;rs1[rs1 < -0.4]=-0.4;

  order="original"; range='orig';corre='no_renorm'; type='full'; method='color'; #ga='All';gf='Female';gm='Male' #color square
  cl.offset=1.0;cl.length=11;cl.cex = 1.4;pch.cex=1.5;pch=20;cl.pos = 'n';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
  ho=Group;hip1='Steroids_y vs. PFAS_as_x'
  width=5500; height=1800
#I have driven these separately for html: 

  
  
  
jpeg(paste("Linear Model Estimate Plot of_sa5",hip1,Group,".jpg"), width = width, height = height, quality = 100,pointsize = 16, res=300);
  corrplot(rs1, type = type, order = order,method=method, p.mat=rs2, tl.col = "black", cl.cex = cl.cex,pch.cex=pch.cex,pch.col='black',pch=pch,
sig.level = c(.001, .01, .05),cl.pos =  cl.pos, insig = "label_sig",cl.offset=cl.offset,cl.length=cl.length, tl.cex=0.8, #tl.pos='n',
tl.srt = 90, diag = TRUE,col = colorRampPalette(c('blue','white', 'orange'), alpha = TRUE)(100),is.corr = FALSE,col.lim=c(-0.4,0.4)); 

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html#change-color-spectra-color-legend-and-text-legend
dev.off();
    eoh=paste("Linear Model Estimate Plot of_sa5",hip1,Group,".jpg")
    daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'))
    my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))


  } 
  

# #     #,is.corr = FALSE
#   hoi=hopiu
#     rsa=c();rs1=c();rs2=c()
# 
#     hoi[,1] <- gsub("\\.", "-",hoi[,1])
#      hoi[,1] <- gsub("X11", "11",hoi[,1])
#      hoi[,1] <- gsub("X17", "17",hoi[,1])
#      hoi[,1][hoi[,1]=="T-Epi-T"]="T/Epi-T"
#      hoi[,1][hoi[,1]=="T-E-T"]="T/Epi-T"
#      # hoi[,1][hoi[,1]=="Steatosis-Grade"]="Steatosis Grade"
#      # hoi[,1][hoi[,1]=="Fibrosis-Stage"]="Fibrosis Stage"
#      # hoi[,1][hoi[,1]=="17aOH-P4"]="17a-OHP4"
# 
# # #
#     x2[x2=="17aOH-P4"]="17a-OHP4"
#     c1=hoi[,1] %in% x2
#     hoi2=hoi[c1,] # This gives you the PFAS (x5) ok. Tää tulee suoraan tällä PFAS 7:lle ekalle
#     c2=hoi2[,2] %in% c(x3,x6)#names(table(hoi[,1]))[!names(table(hoi[,1])) %in% c(x3,x5,x6)] #vaikeemman kautta
# #     # hyy=c1 & c2
#     hoi3=hoi2[c2,] #Likewise...
#     # hoi2=hoi2[1:182,]
#     # a=hoi2[1:42,1];b=hoi2[1:42,2]
#     # hoi2[1:42,]=cbind(b,a,hoi2[1:42,3:5])
#     # hoi2=hoi2[,c(2,1,3:5)]
#     # hoi4=rbind(hoi2,hoi3)
#     rownames(hoi3)=1:dim(hoi3)[1]
# 
# 
#     i=4;
#     rse=c()
#     for (i in 4:5) {
# 
#       rse=hoi3[,c(1,2,i)]
#       rse=rse[order(rse[,1]),]
#       rs=reshape(rse,idvar="x",timevar="y",direction="wide")
#       rownames(rs)=rs[,1]
#       rs=rs[,-1]
# 
#       library(stringr)
#       colnames(rs)=str_sub(colnames(rs),3,-1)
#       colnames(rs) <- gsub("\\.", "-", colnames(rs))
#       colnames(rs) <- gsub("X11", "11", colnames(rs))
#       colnames(rs) <- gsub("X17", "17", colnames(rs))
#       colnames(rs)[colnames(rs)=="T-Epi-T"]="T/Epi-T"
#       colnames(rs)[colnames(rs)=="T-E-T"]="T/Epi-T"
#       colnames(rs)[colnames(rs)=="Steatosis-Grade"]="Steatosis Grade"
#       colnames(rs)[colnames(rs)=="Fibrosis-Stage"]="Fibrosis Stage"
#       colnames(rs)[colnames(rs)=="17aOH-P4"]="17a-OHP4"
#       heps=c(groups[,2])
#       heps <- gsub("\\.", " ", heps)
#       heps[heps=="HOMA IR"]="HOMA-IR"
#       heps[heps=="17aOH-P4"]="17a-OHP4"
#       ccc=match(heps,colnames(rs))
# 
#       # rs=rs[,ccc]
# 
#       rsa=rbind(rsa,rs)
#     }
# 
#     rs1a=rsa[1:length(c(x3,x6)),];
#     rs2a=rsa[(length(c(x3,x6))+1):dim(rsa)[1],]
#     rs1=rs1a;rs2=rs2a;rownames(rs2)=str_sub(rownames(rs2),1,-2) #not obvious
#     width=3500; height=1700
# 
#     order="original"; range='orig';corre='no_renorm'; type='full'; method='color'; #ga='All';gf='Female';gm='Male' #color square
#     cl.offset=1.0;cl.length=5;cl.cex = 1.05;pch.cex=1.05;pch=14;cl.pos = 'r';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
#     ho=Group; hip1='Steroids_x vs. bal_as_y'
#     rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
#     rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))
#     # rs1=rango(rs1,-0.5,0.5) #check if needed
# 
#     rs1=t(as.matrix(rs1))
#     rs2=t(as.matrix(rs2))
# 
#     Outcome=colnames(tv_covNS)[c(29:51,59:71)];
# Outcome=Outcome[c(1,23,2:22,24:length(Outcome))]
# rs1=rs1[,Outcome[Outcome %in% colnames(rs1) ]];rs2=rs2[,Outcome[Outcome %in% colnames(rs2) ]]
# 
# 
# rs1=rs1[groups[,'Abbreviation'],]
# rs2=rs2[groups[,'Abbreviation'],]
# 
# 
#   order="original"; range='orig';corre='no_renorm'; type='full'; method='color'; #ga='All';gf='Female';gm='Male' #color square
#   cl.offset=1.0;cl.length=5;cl.cex = 1.4;pch.cex=1.5;pch=20;cl.pos = 'r';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
#   ho=Group;hip1='Steroids_y vs Bile Acids and Lipids_x'
#   col=colorRampPalette(c('blue','white','orange'), alpha = TRUE)(150)
# #I have driven these separately for html:
# 
#   hist(as.numeric(unlist(rs1)),breaks=30,ylim=c(0.0,40))  #xlim=c(0.04,0.4),
# resulta1[rs1 >= 0.24] = 0.3 #max(rs1)
# resulta1[rs1 < -0.24] = -0.3 #min(rs1)
# # resulta1[resulta1 > 1]  = 1
# # resulta1[resulta1 < -1] = -1 #col.lim=c(-0.4,0.4)) 
#   
# jpeg(paste("Linear Model Estimate Plot of_s",hip1,Group,".jpg"), width = width, height = height, quality = 100,pointsize = 16, res=300);
# 
# corrplot(rs1, type = type, order = order,method=method, p.mat=rs2, tl.col = "black", cl.cex = cl.cex,pch.cex=pch.cex,pch.col='black',pch=pch,
# sig.level = c(.001, .01, .05),cl.pos =  'r', insig = "label_sig",cl.offset=cl.offset,cl.length=cl.length, tl.cex=1.0, #tl.pos='n',
# tl.srt = 90, diag = TRUE,col =col ,is.corr = FALSE,col.lim=c(-0.3,0.3)); #colorRampPalette(c('blue','white', 'orange'), alpha = TRUE)(100) is.corr = TRUE or false
# 
# # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html#change-color-spectra-color-legend-and-text-legend
# dev.off();     eoh=paste("Linear Model Estimate Plot of_s",hip1,Group,".jpg"); daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'));my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))
# 
#   hoi=hopiu
#     rsa=c();rs1=c();rs2=c()
# 
#     hoi[,1] <- gsub("\\.", "-",hoi[,1])
#      hoi[,1] <- gsub("X11", "11",hoi[,1])
#      hoi[,1] <- gsub("X17", "17",hoi[,1])
#      hoi[,1][hoi[,1]=="T-Epi-T"]="T/Epi-T"
#      hoi[,1][hoi[,1]=="T-E-T"]="T/Epi-T"
#      # hoi[,1][hoi[,1]=="Steatosis-Grade"]="Steatosis Grade"
#      # hoi[,1][hoi[,1]=="Fibrosis-Stage"]="Fibrosis Stage"
#      # hoi[,1][hoi[,1]=="17aOH-P4"]="17a-OHP4"
# 
# # #
#     x2[x2=="17aOH-P4"]="17a-OHP4"
#     c1=hoi[,1] %in% x5
#     hoi2=hoi[c1,] # This gives you the PFAS (x5) ok. Tää tulee suoraan tällä PFAS 7:lle ekalle
#     c2=hoi2[,2] %in% c(x3,x6)#names(table(hoi[,1]))[!names(table(hoi[,1])) %in% c(x3,x5,x6)] #vaikeemman kautta
# #     # hyy=c1 & c2
#     hoi3=hoi2[c2,] #Likewise...
#     # hoi2=hoi2[1:182,]
#     # a=hoi2[1:42,1];b=hoi2[1:42,2]
#     # hoi2[1:42,]=cbind(b,a,hoi2[1:42,3:5])
#     # hoi2=hoi2[,c(2,1,3:5)]
#     # hoi4=rbind(hoi2,hoi3)
#     rownames(hoi3)=1:dim(hoi3)[1]
# 
# 
#     i=4;
#     rse=c()
#     for (i in 4:5) {
# 
#       rse=hoi3[,c(1,2,i)]
#       rse=rse[order(rse[,1]),]
#       rs=reshape(rse,idvar="x",timevar="y",direction="wide")
#       rownames(rs)=rs[,1]
#       rs=rs[,-1]
# 
#       library(stringr)
#       colnames(rs)=str_sub(colnames(rs),3,-1)
#       # colnames(rs) <- gsub("\\.", "-", colnames(rs))
#       # colnames(rs) <- gsub("X11", "11", colnames(rs))
#       # colnames(rs) <- gsub("X17", "17", colnames(rs))
#       # colnames(rs)[colnames(rs)=="T-Epi-T"]="T/Epi-T"
#       # colnames(rs)[colnames(rs)=="T-E-T"]="T/Epi-T"
#       # colnames(rs)[colnames(rs)=="Steatosis-Grade"]="Steatosis Grade"
#       # colnames(rs)[colnames(rs)=="Fibrosis-Stage"]="Fibrosis Stage"
#       # colnames(rs)[colnames(rs)=="17aOH-P4"]="17a-OHP4"
#       # heps=c(groups[,2])
#       # heps <- gsub("\\.", " ", heps)
#       # heps[heps=="HOMA IR"]="HOMA-IR"
#       # heps[heps=="17aOH-P4"]="17a-OHP4"
#       # ccc=match(heps,colnames(rs))
# 
#       # rs=rs[,ccc]
# 
#       rsa=rbind(rsa,rs)
#     }
# 
#     rs1a=rsa[1:length(c(x3,x6)),];
#     rs2a=rsa[(length(c(x3,x6))+1):dim(rsa)[1],]
#     rs1=rs1a;rs2=rs2a;rownames(rs2)=str_sub(rownames(rs2),1,-2) #not obvious
#     width=3200; height=1600
# 
#     order="original"; range='orig';corre='no_renorm'; type='full'; method='color'; #ga='All';gf='Female';gm='Male' #color square
#     cl.offset=1.0;cl.length=5;cl.cex = 1.05;pch.cex=1.05;pch=10;cl.pos = 'r';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
#     ho=Group; hip1='Steroids_x vs. bal_as_y'
#     rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
#     rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))
#     # rs1=rango(rs1,-0.5,0.5) #check if needed
# 
#     rs1=t(as.matrix(rs1))
#     rs2=t(as.matrix(rs2))
# 
#     Outcome=colnames(tv_covNS)[c(29:51,59:71)];
# Outcome=Outcome[c(1,23,2:22,24:length(Outcome))]
# rs1=rs1[,Outcome[Outcome %in% colnames(rs1) ]];rs2=rs2[,Outcome[Outcome %in% colnames(rs2) ]]
# 
# 
# # rs1=rs1[groups[,'Abbreviation'],]
# # rs2=rs2[groups[,'Abbreviation'],]
# 
# 
#   order="original"; range='orig';corre='no_renormaadasdfd'; type='full'; method='color'; #ga='All';gf='Female';gm='Male' #color square
#   cl.offset=1.0;cl.length=5;cl.cex = 1.4;pch.cex=1.5;pch=14;cl.pos = 'r';#cl.pos = 'b' ;#pch.cex=0.95,1.3; height=6300; pos 'b' cl.pos = 'b'
#   ho=Group;hip1='Steroids_y vs Bile Acids and Lipidds_x'
#   col=colorRampPalette(c('blue','white','orange'), alpha = TRUE)(150)
# #I have driven these separately for html:
# 
#   
# hist(as.numeric(unlist(rs1)),breaks=30,ylim=c(0.0,40))  #xlim=c(0.04,0.4),
# 
# resulta1[rs1 >= 0.26] = 0.3 #max(rs1)
# resulta1[rs1 < -0.26] = -0.3 #min(rs1)
# # resulta1[resulta1 > 1]  = 1
# # resulta1[resulta1 < -1] = -1 #col.lim=c(-0.4,0.4))   
#   
# jpeg(paste("Linear Model Estimate Plot of_sss",hip1,Group,".jpg"), width = width, height = height, quality = 100,pointsize = 14, res=300);
# 
# corrplot(rs1, type = type, order = order,method=method, p.mat=rs2, tl.col = "black", cl.cex = cl.cex,pch.cex=pch.cex,pch.col='black',pch=pch,
# sig.level = c(.001, .01, .05),cl.pos =  'r', insig = "label_sig",cl.offset=cl.offset,cl.length=cl.length, tl.cex=1.0, #tl.pos='n',
# tl.srt = 90, diag = TRUE,col =col ,is.corr = FALSE,col.lim=c(-0.3,0.3) ); #colorRampPalette(c('blue','white', 'orange'), alpha = TRUE)(100) is.corr = TRUE or false
# 
# # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html#change-color-spectra-color-legend-and-text-legend
# dev.off();eoh=paste("Linear Model Estimate Plot of_sss",hip1,Group,".jpg"); daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'));my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))

  
  # https://scales.arabpsychology.com/stats/how-to-remove-the-last-character-from-a-string-in-r-2-examples/
  return(list(hopiu))
}
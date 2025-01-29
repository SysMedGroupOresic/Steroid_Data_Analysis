
houdees=function(hoi, rt2, switch, mn, z, corr, date, neg) {
  indir=c(); dir=c(); ip=c(); rn=c(); rn2=c()
  Outcome=colnames(tv_covNS)[c(29:51,59:71)]; # The final dataframe is shorter or the like so there were less variables here...
  Treatment=colnames(tv_covNS)[52:58];
  ## https://sparkbyexamples.com/r-programming/r-remove-from-vector-with-examples/
  
  # Direct mediation analysis
  if (switch==1) {
    Mediator_ok=Outcome[Outcome %in% names(table(hoi[1:dim(hoi)[1],c(3)]))]
    for (i in 1:7) {
      for (j in 1:length(Mediator_ok)) {
        if (z=='dir') {
          ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ADE']))) {
            if (median(ap[,'ADE']) < quantile(rt2[,'ADE'],0.5)) {
              apa=min(ap[,'ADE']);
              ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']
            } else {
              apa=max(ap[,'ADE']);
              ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        } else {
          ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ACME']))) {
            if (median(ap[,'ACME']) < quantile(rt2[,'ACME'],0.5)) {
              apa=min(ap[,'ACME']);
              ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']
            } else {
              apa=max(ap[,'ACME']);
              ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        }
        rn=append(rn,hoi[,3][which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]]) # change this...
        rn2=append(rn2,hoi[,1][which(hoi[,1]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]])
        
        Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
        myData <- data.frame(matrix = Matrix)
        colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
      }
    }
  } else if (switch==0) {
    # Indirect mediation analysis
    Mediator_ok=colnames(tv_all)[9:28][colnames(tv_all)[9:28] %in% names(table(hoi[1:dim(hoi)[1],c(2)]))]
    for (i in 1:7) {
      for (j in 1:length(Mediator_ok)) {
        if (z=='dir') {
          ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ADE']))) {
            if (median(ap[,'ADE']) < quantile(rt2[,'ADE'],0.5)) {
              apa=min(ap[,'ADE']);
              ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']
            } else {
              apa=max(ap[,'ADE']);
              ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        } else {
          ap=rt2[which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ACME']))) {
            if (quantile(ap[,'ACME'],0.50) < quantile(rt2[,'ACME'],0.5)) {
              apa=min(ap[,'ACME']);
              ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']
            } else {
              apa=max(ap[,'ACME']);
              ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        }
        rn=append(rn,hoi[,2][which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j])[1]]) # change this...
        rn2=append(rn2,hoi[,1][which(hoi[,1]==Treatment[i] & hoi[,2]==Mediator_ok[j])[1]])
        
        Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
        myData <- data.frame(matrix = Matrix)
        colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
      }
    }
  } else if (switch==2) {
    Treatment=colnames(tv_all)[9:28]; # These names are a bit mixed, but the idea is ok.
    Mediator_ok=Outcome[Outcome %in% names(table(hoi[1:dim(hoi)[1],c(3)]))]
    
    for (i in 1:length(Treatment)) {
      for (j in 1:length(Mediator_ok)) {
        if (z=='dir') {
          ap=rt2[which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ADE']))) {
            if (median(ap[,'ADE']) < quantile(rt2[,'ADE'],0.5)) {
              apa=min(ap[,'ADE']);
              ape=ap[ap[,'ADE']==min(ap[,'ADE']),'z0.p']
            } else {
              apa=max(ap[,'ADE']);
              ape=ap[ap[,'ADE']==max(ap[,'ADE']),'z0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        } else {
          ap=rt2[which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j]),]; 
          if (!is.na(median(ap[,'ACME']))) {
            if (median(ap[,'ACME']) < quantile(rt2[,'ACME'],0.5)) {
              apa=min(ap[,'ACME']);
              ape=ap[ap[,'ACME']==min(ap[,'ACME']),'d0.p']
            } else {
              apa=max(ap[,'ACME']);
              ape=ap[ap[,'ACME']==max(ap[,'ACME']),'d0.p']
            }
          } else {
            apa=0; ape=1
          }
          indir=append(indir,apa) # or c(1) hoi 1 or 5 (5 is orig)
          ip=append(ip,ape)
        }
        rn=append(rn,hoi[,3][which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]]) # change this...
        rn2=append(rn2,hoi[,2][which(hoi[,2]==Treatment[i] & hoi[,3]==Mediator_ok[j])[1]])
        Matrix <- matrix(0, nrow = length(Treatment), ncol = length(Mediator_ok))
        myData <- data.frame(matrix = Matrix)
        colnames(myData) <- Mediator_ok; rownames(myData) <- Treatment
      }
    }
  } # You need three of these, yes :) ;print(ap[,'ADE']) print(rownames(ap[ap[,'ADE']==min(ap[,'ADE']),]))
  
  tot=cbind(rn2,rn,indir) # or indir or dir
  tot=tot[!is.na(tot[,1]),]
  tot=as.data.frame(tot)
  
  # uu=data.frame();hou=c()
  # if (neg=='no') {
  # for (i in 1:length(Treatment)) {hou=names( table(tot[tot[,1]==Treatment[i],2])[table(tot[tot[,1]==Treatment[i],2])>0]);#print(hou)
  # for (j in 1:length(hou)) {uu=rbind(uu, max(tot[tot[,1]==Treatment[i] & tot[,2]==hou[j],3]))}} #
  # rr=data.frame()
  # for (i in 1:length(Treatment)) {hou=names( table(tot[tot[,1]==Treatment[i],2])[table(tot[tot[,1]==Treatment[i],2])>0]);#print(hou)
  # for (j in 1:length(hou)) {rr=rbind(rr, rownames(tot[tot[,3] == max(tot[tot[,1]==Treatment[i] & tot[,2]==hou[j],3]),]))}};uim=cbind(uu,rr);tot=tot[uim[,2],]} else if
  # (neg=='yes') {for (i in 1:length(Treatment)) {hou=names( table(tot[tot[,1]==Treatment[i],2])[table(tot[tot[,1]==Treatment[i],2])>0]);#print(hou)
  # for (j in 1:length(hou)) {uu=rbind(uu, min(tot[tot[,1]==Treatment[i] & tot[,2]==hou[j],3]))}} #
  # rr=data.frame()
  # for (i in 1:length(Treatment)) {hou=names( table(tot[tot[,1]==Treatment[i],2])[table(tot[tot[,1]==Treatment[i],2])>0]);#print(hou)
  # for (j in 1:length(hou)) {rr=rbind(rr, rownames(tot[tot[,3] == min(tot[tot[,1]==Treatment[i] & tot[,2]==hou[j],3]),]))}};uim=cbind(uu,rr);tot=tot[uim[,2],]} else if
  # (neg=='else') {tot=tot}
  
  # Here you would need to have an evaluation for the 'else', if it is negative, it needs to be max negative, and vice versa for the
  # There does not seem to be doubles: table(tot[,1])
  
  tot[,3]=as.numeric(tot[,3])
  
  library(reshape2)
  jops=dcast(tot, rn2~rn, value.var='indir')
  jops[is.na(jops)]=0
  rownames(jops)=jops[,1]
  jops=jops[,2:dim(jops)[2]]
  jops=as.data.frame(jops)
  jopsr=matrix(as.numeric(unlist(jops)),nrow=dim(jops)[1],ncol=dim(jops)[2])
  colnames(jopsr)=colnames(jops); rownames(jopsr)=rownames(jops)
  
  # Ensure all rows and columns from myData are present in jopsr
  if (sum(!rownames(myData) %in% rownames(jopsr)) > 0) {
    to_df=rownames(myData)[!rownames(myData) %in% rownames(jopsr)]
    jopsr=rbind(jopsr, myData[to_df,]); jopsr=jopsr[rownames(myData),]
  }
  if (sum(!colnames(myData) %in% colnames(jopsr)) > 0) {
    to_df=colnames(myData)[!colnames(myData) %in% colnames(jopsr)]
    jopsr=cbind(jopsr, myData[,to_df]); jopsr=jopsr[,colnames(myData)]
  }
  
  tot=cbind(rn2, rn, ip)
  tot=tot[!is.na(tot[,1]),]
  tot=as.data.frame(tot)
  tot[,3]=as.numeric(tot[,3])
  
  jopsa=dcast(tot, rn2~rn, value.var='ip')
  jopsa[is.na(jopsa)]=0
  rownames(jopsa)=jopsa[,1]
  jopsa=jopsa[,2:dim(jopsa)[2]]
  jopsra=matrix(as.numeric(unlist(jopsa)),nrow=dim(jopsa)[1],ncol=dim(jopsa)[2])
  colnames(jopsra)=colnames(jopsa); rownames(jopsra)=rownames(jopsa)
  
  if (sum(!rownames(myData) %in% rownames(jopsra)) > 0) {
    to_df=rownames(myData)[!rownames(myData) %in% rownames(jopsra)]
    jopsra=rbind(jopsra, myData[to_df,]); jopsra=jopsra[rownames(myData),]
  }
  if (sum(!colnames(myData) %in% colnames(jopsra)) > 0) {
    to_df=colnames(myData)[!colnames(myData) %in% colnames(jopsra)]
    jopsra=cbind(jopsra, myData[,to_df]); jopsra=jopsra[,colnames(myData)]
  }
  
  if (switch==1) {
    # For direct effects
  } else if (switch==0) {
    # For indirect effects
    jopsra=jopsra[,groups[,'Abbreviation'][groups[,'Abbreviation'] %in% colnames(jopsra)]]
    jopsr=jopsr[,groups[,'Abbreviation'][groups[,'Abbreviation'] %in% colnames(jopsr)]]
  } else if (switch==2) {
    jopsra=jopsra[groups[,'Abbreviation'],]
    jopsr=jopsr[groups[,'Abbreviation'],]
  }
  
  
  path="C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/"; setwd(path) #check this if needed...
  jpeg(paste("Heatmap of high",date, mn,neg,".jpg"), width = width, height = height, quality = 100,pointsize = 14, res=300);# par( ps=ps)# par(cex.lab=90) 22 18
  # col = brewer.pal(n = 9, name = "YlOrRd")
  order="original"; range='orig';corre='no_renormaa'; type='full'; method='color';ga='All';gf='Female';gm='Male' #color square
  cl.offset=20;cl.length=7;cl.cex = 1.45;pch.cex=2.45;pch=2;cl.pos = 'r'; #cl.offset=2;cl.length=5;cl.cex = 1.3;pch.cex=1.95;pch=14;
  
  col=colorRampPalette(c('blue', 'white','orange'), alpha = TRUE)(150)
  if (neg=='no') {col=colorRampPalette(c( 'white','orange'), alpha = TRUE)(150)} else if (neg=='yes')
  {col=colorRampPalette(c('blue', 'white'), alpha = TRUE)(150)} else {col=col}
  
  # if (corr==TRUE) {if (min(as.matrix(resulta1))< -1  | max(as.matrix(resulta1))> 1) {resulta1=rango(resulta1,-1,1)}} else if (min(as.matrix(resulta1)) >= 0)  {resulta1=rango(resulta1,-1,1)} #
  # resulta1=rango(resulta1,-1,1)
  # if (min(as.matrix(resulta1)) >= 0  | max(as.matrix(resulta1)) <= 0) {resulta1=rango(resulta1,-1,1)}
  
  corrplot(as.matrix(resulta1), type = type, order = order,method=method, p.mat=as.matrix(p.mat.a1), tl.col = "black", #sum(COL2('RdBu')=="#FF7417")
           cl.cex = cl.cex, pch.cex=pch.cex, pch.col='black',pch=pch,#pitikö vain pch lisätä pch väriin väriin... mystistä...'#FEE12B'
           sig.level = c(0.05),cl.pos = cl.pos, insig = "label_sig", cl.offset=cl.offset,cl.length=cl.length, #.001, .05, .2
           tl.srt = 90, diag = TRUE,col=col,is.corr = corr,col.lim=c(-heps,heps)) #only in age...0.001,col.lim=c(-heps,heps) #rev(COL2('RdBu')[25:(length(COL2('RdBu'))-25)]) col.lim=c(-1,1) col.lim=c(-heps,heps)
  #non were significant in neg... but after mody yes!
  
  dev.off(); eoh=paste("Heatmap of high",date, mn,neg,".jpg"); daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'))
  my_image <- image_read(eoh);my_svg <- image_convert(my_image, format="svg"); image_write(my_svg, paste(eoh,".svg"))
  
  return(list(resulta1, p.mat.a1))
}




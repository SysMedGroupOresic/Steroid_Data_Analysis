### R code from vignette source 'qpTxRegNet.Rnw'

###################################################
### code chunk number 1: style
###################################################
BiocStyle::latex(use.unsrturl=FALSE)


###################################################
### code chunk number 2: setup
###################################################
library(Biobase)
library(annotate)
library(genefilter)
library(org.EcK12.eg.db)
library(graph)
library(qpgraph)


###################################################
### code chunk number 3: setup
###################################################
data(EcoliOxygen)
ls()


###################################################
### code chunk number 4: setup
###################################################
gds680.eset


###################################################
### code chunk number 5: preprocessing
###################################################
head(filtered.regulon6.1)


###################################################
### code chunk number 6: preprocessing
###################################################
knockoutsyms <- c("arcA","appY","oxyR","soxS","fnr")
rmap <- revmap(getAnnMap("SYMBOL", "org.EcK12.eg.db"))
knockoutEgIDs <- unlist(mget(knockoutsyms, rmap))
knockoutEgIDs


###################################################
### code chunk number 7: preprocessing
###################################################
mt <- match(filtered.regulon6.1[,"EgID_TF"], knockoutEgIDs)
cat("These 5 TFs are involved in",sum(!is.na(mt)),"TF-TG interactions\n")
genesO2net <- as.character(unique(as.vector(
              as.matrix(filtered.regulon6.1[!is.na(mt),c("EgID_TF","EgID_TG")]))))
cat("There are",length(genesO2net),"different genes in this subnetwork\n")


###################################################
### code chunk number 8: preprocessing
###################################################
IQRs <- apply(exprs(gds680.eset[genesO2net,]), 1, IQR)
largestIQRgenesO2net <- names(sort(IQRs,decreasing=TRUE)[1:100])


###################################################
### code chunk number 9: preprocessing
###################################################
dim(gds680.eset)
subset.gds680.eset <- gds680.eset[largestIQRgenesO2net,]
dim(subset.gds680.eset)
subset.gds680.eset


###################################################
### code chunk number 10: preprocessing
###################################################
mtTF <- match(filtered.regulon6.1[,"EgID_TF"],largestIQRgenesO2net)
mtTG <- match(filtered.regulon6.1[,"EgID_TG"],largestIQRgenesO2net)

cat(sprintf("The 100 genes are involved in %d RegulonDB interactions\n",
    sum(!is.na(mtTF) & !is.na(mtTG))))

subset.filtered.regulon6.1 <- filtered.regulon6.1[!is.na(mtTF) & !is.na(mtTG),]


###################################################
### code chunk number 11: preprocessing
###################################################
TFi <- match(subset.filtered.regulon6.1[,"EgID_TF"],
             featureNames(subset.gds680.eset))
TGi <- match(subset.filtered.regulon6.1[,"EgID_TG"],
             featureNames(subset.gds680.eset))

subset.filtered.regulon6.1 <- cbind(subset.filtered.regulon6.1,
                                    idx_TF=TFi, idx_TG=TGi)

p <- dim(subset.gds680.eset)["Features"]

subset.filtered.regulon6.1.I <- matrix(FALSE, nrow=p, ncol=p)
rownames(subset.filtered.regulon6.1.I) <- featureNames(subset.gds680.eset)
colnames(subset.filtered.regulon6.1.I) <- featureNames(subset.gds680.eset)

idxTFTG <- as.matrix(subset.filtered.regulon6.1[,c("idx_TF","idx_TG")])

subset.filtered.regulon6.1.I[idxTFTG] <-
   subset.filtered.regulon6.1.I[cbind(idxTFTG[,2],idxTFTG[,1])] <- TRUE


###################################################
### code chunk number 12: getthenet
###################################################
pcc.estimates <- qpPCC(subset.gds680.eset)


###################################################
### code chunk number 13: getthenet
###################################################
largestIQRgenesO2net_i <- match(largestIQRgenesO2net,
                                featureNames(subset.gds680.eset))
largestIQRgenesO2netTFs <- largestIQRgenesO2net[!is.na(
                           match(largestIQRgenesO2net,filtered.regulon6.1[,"EgID_TF"]))]
largestIQRgenesO2netTFs_i <- match(largestIQRgenesO2netTFs,
                                   featureNames(subset.gds680.eset))
TFsbyTGs <- as.matrix(expand.grid(largestIQRgenesO2netTFs_i,
                                  setdiff(largestIQRgenesO2net_i,largestIQRgenesO2netTFs_i)))
TFsbyTGs <- rbind(TFsbyTGs,t(combn(largestIQRgenesO2netTFs_i, 2)))
summary(abs(pcc.estimates$R[TFsbyTGs]))


###################################################
### code chunk number 14: getthenet
###################################################
regulonDBgenes <- as.character(unique(c(filtered.regulon6.1[, "EgID_TF"],
                                        filtered.regulon6.1[, "EgID_TG"])))
cat(sprintf("The RegulonDB transcriptional network involves %d genes",
            length(regulonDBgenes)))

pcc.allRegulonDB.estimates <- qpPCC(gds680.eset[regulonDBgenes,])

allTFs_i <- match(unique(filtered.regulon6.1[, "EgID_TF"]), regulonDBgenes)
allTFsbyTGs <- as.matrix(expand.grid(allTFs_i,
                                     setdiff(1:length(regulonDBgenes), allTFs_i)))
allTFsbyTGs <- rbind(allTFsbyTGs,t(combn(allTFs_i, 2)))
summary(abs(pcc.allRegulonDB.estimates$R[allTFsbyTGs]))


###################################################
### code chunk number 15: getthenet
###################################################
mt <- match(knockoutEgIDs,largestIQRgenesO2net)
unlist(mget(largestIQRgenesO2net[mt[!is.na(mt)]],org.EcK12.egSYMBOL))


###################################################
### code chunk number 16: getthenet
###################################################
maskRegulonTFTG <- subset.filtered.regulon6.1.I & upper.tri(subset.filtered.regulon6.1.I)
summary(abs(pcc.estimates$R[maskRegulonTFTG]))


###################################################
### code chunk number 17: PCCdistByTF
###################################################
par(mar=c(5,4,5,2))
pccsbyTF <- list()
for (TFi in subset.filtered.regulon6.1[,"idx_TF"])
  pccsbyTF[[featureNames(subset.gds680.eset)[TFi]]] <-
    abs(pcc.estimates$R[TFi, subset.filtered.regulon6.1.I[TFi,]])
bp <- boxplot(pccsbyTF,names=sprintf("%s",mget(names(pccsbyTF),org.EcK12.egSYMBOL)),
              ylab="Pearson correlation coefficient (PCC)",
              main=paste("Distribution of PCCs in each RegulonDB",
                         "regulatory module within the 100 genes data set", sep="\n"))
nint <- sprintf("(%d)",sapply(names(pccsbyTF), function(x)
                       sum(subset.filtered.regulon6.1.I[x,])))
mtext(nint, at=seq(bp$n), line=+2, side=1)
mtext("Transcription factor (# RegulonDB interactions)", side=1, line=+4)


###################################################
### code chunk number 18: getthenet
###################################################
set.seed(123)


###################################################
### code chunk number 19: getthenet
###################################################
avgnrr.estimates <- qpAvgNrr(subset.gds680.eset,
                             pairup.i=largestIQRgenesO2netTFs,
                             pairup.j=largestIQRgenesO2net, verbose=FALSE)


###################################################
### code chunk number 20: getthenet
###################################################
pcc.prerec <- qpPrecisionRecall(abs(pcc.estimates$R), subset.filtered.regulon6.1.I,
                                decreasing=TRUE, pairup.i=largestIQRgenesO2netTFs,
                                pairup.j=largestIQRgenesO2net,
                                recallSteps=c(seq(0,0.1,0.01),seq(0.2,1,0.1)))


###################################################
### code chunk number 21: getthenet
###################################################
avgnrr.prerec <- qpPrecisionRecall(avgnrr.estimates, subset.filtered.regulon6.1.I,
                                   decreasing=FALSE,
                                   recallSteps=c(seq(0,0.1,0.01),seq(0.2,1,0.1)))


###################################################
### code chunk number 22: getthenet
###################################################
set.seed(123)
rndcor <- qpUnifRndAssociation(100, featureNames(subset.gds680.eset))
random.prerec <- qpPrecisionRecall(abs(rndcor), subset.filtered.regulon6.1.I,
                                   decreasing=TRUE, pairup.i=largestIQRgenesO2netTFs,
                                   pairup.j=largestIQRgenesO2net,
                                   recallSteps=c(seq(0,0.1,0.01),seq(0.2,1,0.1)))


###################################################
### code chunk number 23: getthenet
###################################################
f <- approxfun(pcc.prerec[,c("Recall","Precision")])
area <- integrate(f,0,1)$value
f <- approxfun(avgnrr.prerec[,c("Recall","Precision")])
area <- cbind(area, integrate(f,0,1)$value)
f <- approxfun(random.prerec[,c("Recall","Precision")])
area <- cbind(area, integrate(f,0,1)$value)
colnames(area) <- c("PCC", "avgNRR", "Random")
rownames(area) <- "AreaPrecisionRecall"
printCoefmat(area)


###################################################
### code chunk number 24: preRecComparison
###################################################
par(mai=c(.5,.5,1,.5),mar=c(5,4,7,2)+0.1)
plot(avgnrr.prerec[,c(1,2)], type="b", lty=1, pch=19, cex=0.65, lwd=4, col="red",
     xlim=c(0,0.1), ylim=c(0,1), axes=FALSE,
     xlab="Recall (% RegulonDB interactions)", ylab="Precision (%)")
axis(1, at=seq(0,1,0.01), labels=seq(0,100,1))
axis(2, at=seq(0,1,0.10), labels=seq(0,100,10))
axis(3, at=avgnrr.prerec[,"Recall"],
     labels=round(avgnrr.prerec[,"Recall"]*dim(subset.filtered.regulon6.1)[1],
                  digits=0))
title(main="Precision-recall comparison", line=+5)
lines(pcc.prerec[,c(1,2)], type="b", lty=1, pch=22, cex=0.65, lwd=4, col="blue")
lines(random.prerec[,c(1,2)], type="l", lty=2, lwd=4, col="black")
mtext("Recall (# RegulonDB interactions)", 3, line=+3)
legend(0.06, 1.0, c("qp-graph","PCC","Random"), col=c("red","blue","black"),
       lty=c(1,1,2), pch=c(19,22,-1), lwd=3, bg="white",pt.cex=0.85)


###################################################
### code chunk number 25: preRecComparisonFullRecall
###################################################
par(mai=c(.5,.5,1,.5),mar=c(5,4,7,2)+0.1)
plot(avgnrr.prerec[,c(1,2)], type="b", lty=1, pch=19, cex=0.65, lwd=4, col="red",
     xlim=c(0,1), ylim=c(0,1), axes=FALSE,
     xlab="Recall (% RegulonDB interactions)", ylab="Precision (%)")
axis(1, at=seq(0,1,0.10), labels=seq(0,100,10))
axis(2, at=seq(0,1,0.10), labels=seq(0,100,10))
axis(3, at=avgnrr.prerec[,"Recall"],
     labels=round(avgnrr.prerec[,"Recall"]*dim(subset.filtered.regulon6.1)[1],
                  digits=0))
title(main="Precision-recall comparison", line=+5)
lines(pcc.prerec[,c(1,2)], type="b", lty=1, pch=22, cex=0.65, lwd=4, col="blue")
lines(random.prerec[,c(1,2)], type="l", lty=2, lwd=4, col="black")
mtext("Recall (# RegulonDB interactions)", 3, line=+3)
legend(0.6, 1.0, c("qp-graph","PCC","Random"), col=c("red","blue","black"),
       lty=c(1,1,2), pch=c(19,22,-1), lwd=3, bg="white",pt.cex=0.85)


###################################################
### code chunk number 26: getthenet
###################################################
thr <- qpPRscoreThreshold(avgnrr.prerec, level=0.5, recall.level=FALSE, max.score=0)
thr


###################################################
### code chunk number 27: getthenet
###################################################
g <- qpGraph(avgnrr.estimates, threshold=thr, return.type="graphNEL")
g


###################################################
### code chunk number 28: getthenet
###################################################
qpCliqueNumber(g, verbose=FALSE)


###################################################
### code chunk number 29: getthenet
###################################################
pac.estimates <- qpPAC(subset.gds680.eset, g, verbose=FALSE)


###################################################
### code chunk number 30: getthenet
###################################################
edL <- edges(g)[names(edges(g))[unlist(lapply(edges(g),length)) > 0]]
edM <- matrix(unlist(sapply(names(edL),
              function(x) t(cbind(x,edL[[x]])),USE.NAMES=FALSE)),
              ncol=2,byrow=TRUE)


###################################################
### code chunk number 31: getthenet
###################################################
edSymbols <- cbind(unlist(mget(edM[,1], org.EcK12.egSYMBOL)),
                   unlist(mget(edM[,2], org.EcK12.egSYMBOL)))
idxTF <- match(edM[,1], featureNames(subset.gds680.eset))
idxTG <- match(edM[,2], featureNames(subset.gds680.eset))
nrrs <- avgnrr.estimates[cbind(idxTF, idxTG)]
pacs.rho <- pac.estimates$R[cbind(idxTF, idxTG)]
pacs.pva <- pac.estimates$P[cbind(idxTF, idxTG)]
pccs.rho <- pcc.estimates$R[cbind(idxTF, idxTG)]
pccs.pva <- pcc.estimates$P[cbind(idxTF, idxTG)]

idxRegDB <- apply(edM,1,function(x) { 
                          regdbmask <-
                            apply(
                            cbind(match(subset.filtered.regulon6.1[,"EgID_TF"],x[1]),
                                  match(subset.filtered.regulon6.1[,"EgID_TG"],x[2])),
                            1, function(y) sum(!is.na(y))) == 2 ;
                          if (sum(regdbmask) > 0)
                            (1:dim(subset.filtered.regulon6.1)[1])[regdbmask]
                          else
                             NA
                        })

isinRegDB <- matrix(c("present","absent"),
                    nrow=2, ncol=length(idxRegDB))[t(cbind(!is.na(idxRegDB),
                    is.na(idxRegDB)))]


###################################################
### code chunk number 32: getthenet
###################################################
txregnet <- data.frame(RegulonDB=isinRegDB,
                       RegDBdir=subset.filtered.regulon6.1[idxRegDB,"Direction"],
                       AvgNRR=round(nrrs,digits=2),
                       PCC.rho=round(pccs.rho,digits=2),
                       PCC.pva=format(pccs.pva,scientific=TRUE,digits=3),
                       PAC.rho=round(pacs.rho,digits=2),
                       PAC.pva=format(pacs.pva,scientific=TRUE,digits=3))
rownames(txregnet) <- paste(edSymbols[,1],edSymbols[,2],sep=" -> ")


###################################################
### code chunk number 33: getthenet
###################################################
txregnet[sort(txregnet[["AvgNRR"]],index.return=TRUE)$ix,]


###################################################
### code chunk number 34: txNet50pctpre
###################################################
qpPlotNetwork(g, pairup.i=largestIQRgenesO2netTFs, pairup.j=largestIQRgenesO2net,
              annotation="org.EcK12.eg.db")


###################################################
### code chunk number 35: info
###################################################
toLatex(sessionInfo())



### R code from vignette source 'eQTLnetworks.Rnw'

###################################################
### code chunk number 1: style
###################################################
BiocStyle::latex(use.unsrturl=FALSE)


###################################################
### code chunk number 2: setup
###################################################
pdf.options(useDingbats=FALSE)
options(width=80)
rm(list=ls())
try(detach("package:mvtnorm"), silent=TRUE)
try(detach("package:qtl"), silent=TRUE)


###################################################
### code chunk number 3: eQTLnetworks.Rnw:71-74
###################################################
library(GenomeInfoDb)
library(qtl)
library(qpgraph)


###################################################
### code chunk number 4: eQTLnetworks.Rnw:79-84
###################################################
map <- sim.map(len=rep(100, times=9),
               n.mar=rep(10, times=9),
               anchor.tel=FALSE,
               eq.spacing=TRUE,
               include.x=FALSE)


###################################################
### code chunk number 5: eQTLnetworks.Rnw:96-99
###################################################
set.seed(12345)
sim.eqtl <- reQTLcross(eQTLcrossParam(map=map, genes=50, cis=0.5, trans=rep(5, 5)),
                       a=2, rho=0.5)


###################################################
### code chunk number 6: simeqtlnet
###################################################
plot(sim.eqtl, main="Simulated eQTL network")


###################################################
### code chunk number 7: eQTLnetworks.Rnw:106-109
###################################################
set.seed(12345)
cross <- sim.cross(map, sim.eqtl, n.ind=100)
cross


###################################################
### code chunk number 8: eQTLnetworks.Rnw:128-134
###################################################
annot <- data.frame(chr=as.character(sim.eqtl$genes[, "chr"]),
                    start=sim.eqtl$genes[, "location"],
                    end=sim.eqtl$genes[, "location"],
                    strand=rep("+", nrow(sim.eqtl$genes)),
                    row.names=rownames(sim.eqtl$genes),
                    stringsAsFactors=FALSE)


###################################################
### code chunk number 9: eQTLnetworks.Rnw:141-147
###################################################
pMap <- lapply(map, function(x) x * 5)
class(pMap) <- "map"
annot$start <- floor(annot$start * 5)
annot$end <- floor(annot$end * 5)
genome <- Seqinfo(seqnames=names(map), seqlengths=rep(100 * 5, nchr(pMap)),
                  NA, "simulatedGenome")


###################################################
### code chunk number 10: eQTLnetworks.Rnw:153-155
###################################################
param <- eQTLnetworkEstimationParam(cross, physicalMap=pMap,
                                    geneAnnotation=annot, genome=genome)


###################################################
### code chunk number 11: eQTLnetworks.Rnw:159-161
###################################################
eqtlnet.q0 <- eQTLnetworkEstimate(param, ~ marker + gene, verbose=FALSE)
eqtlnet.q0


###################################################
### code chunk number 12: eQTLnetworks.Rnw:166-169
###################################################
eqtlnet.q0.fdr <- eQTLnetworkEstimate(param, estimate=eqtlnet.q0,
                                      p.value=0.05, method="fdr")
eqtlnet.q0.fdr


###################################################
### code chunk number 13: simeqtlnetvsfdr
###################################################
par(mfrow=c(1, 2))
plot(sim.eqtl, main="Simulated eQTL network")
plot(eqtlnet.q0.fdr, main="Esiimated eQTL network")


###################################################
### code chunk number 14: eQTLnetworks.Rnw:191-194
###################################################
eqtlnet.q0.fdr.nrr <- eQTLnetworkEstimate(param, ~ marker + gene | gene(q=3),
                                          estimate=eqtlnet.q0.fdr, verbose=FALSE)
eqtlnet.q0.fdr.nrr


###################################################
### code chunk number 15: eQTLnetworks.Rnw:199-202
###################################################
eqtlnet.q0.fdr.nrr <- eQTLnetworkEstimate(param, estimate=eqtlnet.q0.fdr.nrr,
                                          epsilon=0.1)
eqtlnet.q0.fdr.nrr


###################################################
### code chunk number 16: simeqtlnetvsnrr
###################################################
par(mfrow=c(1, 2))
plot(sim.eqtl, main="Simulated eQTL network")
plot(eqtlnet.q0.fdr.nrr, main="Esiimated eQTL network")


###################################################
### code chunk number 17: eQTLnetworks.Rnw:225-227
###################################################
eqtls <- alleQTL(eqtlnet.q0.fdr.nrr)
median(sapply(split(eqtls$QTL, eqtls$gene), length))


###################################################
### code chunk number 18: eQTLnetworks.Rnw:237-240
###################################################
eqtlnet.q0.fdr.nrr.sel <- eQTLnetworkEstimate(param, estimate=eqtlnet.q0.fdr.nrr,
                                              alpha=0.05)
eqtlnet.q0.fdr.nrr.sel


###################################################
### code chunk number 19: simeqtlnetvsnrrsel
###################################################
par(mfrow=c(1, 2))
plot(sim.eqtl, main="Simulated eQTL network")
plot(eqtlnet.q0.fdr.nrr.sel, main="Esiimated eQTL network")


###################################################
### code chunk number 20: hiveplot
###################################################
library(grid)
library(graph)
grid.newpage()
pushViewport(viewport(layout=grid.layout(3, 3)))
for (i in 1:3) {
  for (j in 1:3) {
    chr <- (i-1) * 3 + j
    pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
    plot(eqtlnet.q0.fdr.nrr.sel, type="hive", chr=chr)
    grid.text(paste0("chr", as.roman(chr)), x=unit(0.05, "npc"),
              y=unit(0.9, "npc"), just="left")
    grid.text("genes", x=unit(0.08, "npc"), y=unit(0.1, "npc"), just="left", gp=gpar(cex=0.9))
    grid.text("all chr", x=unit(0.92, "npc"), y=unit(0.2, "npc"), just="right", gp=gpar(cex=0.9))
    grid.text("genes", x=unit(0.92, "npc"), y=unit(0.1, "npc"), just="right", gp=gpar(cex=0.9))
    grid.text("markers", x=unit(0.5, "npc"), y=unit(0.95, "npc"), just="centre", gp=gpar(cex=0.9))
    popViewport(2)
  }
}


###################################################
### code chunk number 21: info
###################################################
toLatex(sessionInfo())



### R code from vignette source 'qpgraphSimulate.Rnw'

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
### code chunk number 3: qpgraphSimulate.Rnw:118-124
###################################################
library(qpgraph)

args(erGraphParam)
args(erMarkedGraphParam)
args(dRegularGraphParam)
args(dRegularMarkedGraphParam)


###################################################
### code chunk number 4: qpgraphSimulate.Rnw:129-133
###################################################
erGraphParam()
erMarkedGraphParam()
dRegularGraphParam()
dRegularMarkedGraphParam()


###################################################
### code chunk number 5: qpgraphSimulate.Rnw:138-140
###################################################
showClass("graphParam")
showClass("markedGraphParam")


###################################################
### code chunk number 6: qpgraphSimulate.Rnw:151-152
###################################################
args(rgraphBAM)


###################################################
### code chunk number 7: qpgraphSimulate.Rnw:158-160
###################################################
rgraphBAM(erGraphParam())
rgraphBAM(n=2, dRegularGraphParam())


###################################################
### code chunk number 8: 3regulargraph
###################################################
set.seed(1234)
g <- rgraphBAM(dRegularMarkedGraphParam(pI=2, pY=10, d=3))
plot(g)


###################################################
### code chunk number 9: qpgraphSimulate.Rnw:198-199
###################################################
args(rUGgmm)


###################################################
### code chunk number 10: qpgraphSimulate.Rnw:210-219
###################################################
rUGgmm(dRegularGraphParam(p=4, d=2))
rUGgmm(matrix(c(0, 1, 0, 1,
                1, 0, 1, 0,
                0, 1, 0, 1,
                1, 0, 1, 0), nrow=4, byrow=TRUE))
rUGgmm(matrix(c(1, 2,
                2, 3,
                3, 4,
                4, 1), ncol=2, byrow=TRUE))


###################################################
### code chunk number 11: qpgraphSimulate.Rnw:226-229
###################################################
set.seed(12345)
gmm <- rUGgmm(dRegularGraphParam(p=4, d=2))
summary(gmm)


###################################################
### code chunk number 12: qpgraphSimulate.Rnw:233-240
###################################################
class(gmm)
names(gmm)
gmm$X
gmm$p
gmm$g
gmm$mean
gmm$sigma


###################################################
### code chunk number 13: 4cyclegraph
###################################################
plot(gmm)
round(solve(gmm$sigma), digits=1)


###################################################
### code chunk number 14: qpgraphSimulate.Rnw:275-276
###################################################
rmvnorm(10, gmm)


###################################################
### code chunk number 15: qpgraphSimulate.Rnw:281-285
###################################################
set.seed(123)
X <- rmvnorm(10000, gmm)
round(solve(cov(X)), digits=1)
round(solve(gmm$sigma), digits=1)


###################################################
### code chunk number 16: qpgraphSimulate.Rnw:349-350
###################################################
args(rHMgmm)


###################################################
### code chunk number 17: qpgraphSimulate.Rnw:359-368
###################################################
rHMgmm(dRegularMarkedGraphParam(pI=1, pY=3, d=2))
rHMgmm(matrix(c(0, 1, 0, 1,
                1, 0, 1, 0,
                0, 1, 0, 1,
                1, 0, 1, 0), nrow=4, byrow=TRUE), I=1)
rHMgmm(matrix(c(1, 2,
                2, 3,
                3, 4,
                4, 1), ncol=2, byrow=TRUE), I=1)


###################################################
### code chunk number 18: qpgraphSimulate.Rnw:378-381
###################################################
set.seed(12345)
gmm <- rHMgmm(dRegularMarkedGraphParam(pI=1, pY=3, d=2))
summary(gmm)


###################################################
### code chunk number 19: qpgraphSimulate.Rnw:386-399
###################################################
class(gmm)
names(gmm)
gmm$X
gmm$I
gmm$Y
gmm$p
gmm$pI
gmm$pY
gmm$g
gmm$mean()
gmm$sigma
gmm$a
gmm$eta2


###################################################
### code chunk number 20: hmgmm
###################################################
plot(gmm)


###################################################
### code chunk number 21: qpgraphSimulate.Rnw:425-426
###################################################
rcmvnorm(10, gmm)


###################################################
### code chunk number 22: qpgraphSimulate.Rnw:431-437
###################################################
set.seed(123)
X <- rcmvnorm(10000, gmm)
csigma <- (1/10000)*sum(X[, gmm$I] == 1)*cov(X[X[, gmm$I]==1, gmm$Y]) +
          (1/10000)*sum(X[, gmm$I] == 2)*cov(X[X[, gmm$I]==2, gmm$Y])
round(solve(csigma), digits=1)
round(solve(gmm$sigma), digits=1)


###################################################
### code chunk number 23: qpgraphSimulate.Rnw:442-447
###################################################
smean <- apply(X[, gmm$Y], 2, function(x, i) tapply(x, i, mean), X[, gmm$I])
smean
gmm$mean()
abs(smean[1, ] - smean[2, ])
gmm$a


###################################################
### code chunk number 24: qpgraphSimulate.Rnw:471-473
###################################################
eQTLcrossParam()
args(eQTLcrossParam)


###################################################
### code chunk number 25: qpgraphSimulate.Rnw:479-480
###################################################
reQTLcross(n=2, eQTLcrossParam())


###################################################
### code chunk number 26: qpgraphSimulate.Rnw:497-506
###################################################
detach("package:qpgraph")
library(qtl)
library(qpgraph)

map <- sim.map(len=rep(100, times=20),
               n.mar=rep(10, times=20),
               anchor.tel=FALSE,
               eq.spacing=FALSE,
               include.x=TRUE)


###################################################
### code chunk number 27: qpgraphSimulate.Rnw:511-514
###################################################
eqtl <- reQTLcross(eQTLcrossParam(map=map, genes=50))
class(eqtl)
eqtl


###################################################
### code chunk number 28: qpgraphSimulate.Rnw:521-522
###################################################
eqtl$model


###################################################
### code chunk number 29: geneticmap
###################################################
par(mfrow=c(1,2))
plot(map)
plot(eqtl, main="eQTL model with cis-associations only")


###################################################
### code chunk number 30: qpgraphSimulate.Rnw:546-549
###################################################
set.seed(123)
eqtl <- reQTLcross(eQTLcrossParam(map=map, genes=50,
                                  cis=0.5, trans=c(5, 5)), a=5)


###################################################
### code chunk number 31: qpgraphSimulate.Rnw:559-561
###################################################
head(ciseQTL(eqtl))
transeQTL(eqtl)


###################################################
### code chunk number 32: transeqtl
###################################################
plot(eqtl, main="eQTL model with trans-eQTL")


###################################################
### code chunk number 33: qpgraphSimulate.Rnw:581-584
###################################################
set.seed(123)
cross <- sim.cross(map, eqtl)
cross


###################################################
### code chunk number 34: qpgraphSimulate.Rnw:598-603
###################################################
allcis <- ciseQTL(eqtl)
allcis[allcis$chrom==1, ]
gene <- allcis[2, "gene"]
chrom <- allcis[2, "chrom"]
location <- allcis[2, "location"]


###################################################
### code chunk number 35: qpgraphSimulate.Rnw:606-609
###################################################
connectedGenes <- graph::inEdges(gene, eqtl$model$g)[[1]]
connectedGenes <- connectedGenes[connectedGenes %in% eqtl$model$Y]
connectedGenes


###################################################
### code chunk number 36: qpgraphSimulate.Rnw:616-617
###################################################
out.mr <- scanone(cross, method="mr", pheno.col=c(gene, connectedGenes))


###################################################
### code chunk number 37: lodprofiles
###################################################
plot(out.mr, chr=chrom, ylab="LOD score", lodcolumn=1:3, col=c("black", "blue", "red"), lwd=2)
abline(v=allcis[allcis$gene == gene, "location"])
legend("topleft", names(out.mr)[3:5], col=c("black", "blue", "red"), lwd=2, inset=0.05)


###################################################
### code chunk number 38: qpgraphSimulate.Rnw:639-642
###################################################
out.perm <- scanone(cross, method="mr", pheno.col=c(gene, connectedGenes),
                    n.perm=100, verbose=FALSE)
summary(out.perm, alpha=c(0.05, 0.10))


###################################################
### code chunk number 39: qpgraphSimulate.Rnw:647-648
###################################################
summary(out.mr, perms=out.perm, alpha=0.05)


###################################################
### code chunk number 40: info
###################################################
toLatex(sessionInfo())



## ----installgenomicfeatures, eval=FALSE---------------------------------------
#  if (!require("BiocManager", quietly=TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("GenomicFeatures")

## ----loadgenomicfeatures------------------------------------------------------
suppressPackageStartupMessages(library(GenomicFeatures))

## ----loadDb-------------------------------------------------------------------
samplefile <- system.file("extdata", "hg19_knownGene_sample.sqlite",
                          package="GenomicFeatures")
txdb <- loadDb(samplefile)
txdb

## ----loadPackage--------------------------------------------------------------
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
hg19_txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene  # shorthand (for convenience)
hg19_txdb

## ----seqlevels----------------------------------------------------------------
head(seqlevels(hg19_txdb))

## ----seqlevels2---------------------------------------------------------------
seqlevels(hg19_txdb) <- "chr1"

## ----seqlevels3---------------------------------------------------------------
seqlevels(hg19_txdb) <- seqlevels0(hg19_txdb)

## ----seqlevels4---------------------------------------------------------------
seqlevels(hg19_txdb) <- "chr15"
seqlevels(hg19_txdb)

## ----selectExample------------------------------------------------------------
keys <- c("100033416", "100033417", "100033420")
columns(hg19_txdb)
keytypes(hg19_txdb)
select(hg19_txdb, keys = keys, columns="TXNAME", keytype="GENEID")

## ----selectExercise-----------------------------------------------------------
columns(hg19_txdb)
cols <- c("TXNAME", "TXSTRAND", "TXCHROM")
select(hg19_txdb, keys=keys, columns=cols, keytype="GENEID")

## ----transcripts1-------------------------------------------------------------
GR <- transcripts(hg19_txdb)
GR[1:3]

## ----transcripts2-------------------------------------------------------------
tx_strand <- strand(GR)
tx_strand
sum(runLength(tx_strand))
length(GR)

## ----transcripts3-------------------------------------------------------------
GR <- transcripts(hg19_txdb, filter=list(tx_chrom = "chr15", tx_strand = "+"))
length(GR)
unique(strand(GR))

## ----transcripts4-------------------------------------------------------------
PR <- promoters(hg19_txdb, upstream=2000, downstream=400)
PR

## ----exonsExer1---------------------------------------------------------------
EX <- exons(hg19_txdb)
EX[1:4]
length(EX)
length(GR)

## ----transcriptsBy------------------------------------------------------------
GRList <- transcriptsBy(hg19_txdb, by = "gene")
length(GRList)
names(GRList)[10:13]
GRList[11:12]

## ----exonsBy------------------------------------------------------------------
GRList <- exonsBy(hg19_txdb, by = "tx")
length(GRList)
names(GRList)[10:13]
GRList[[12]]

## ----internalID---------------------------------------------------------------
GRList <- exonsBy(hg19_txdb, by = "tx")
tx_ids <- names(GRList)
head(select(hg19_txdb, keys=tx_ids, columns="TXNAME", keytype="TXID"))

## ----introns-UTRs-------------------------------------------------------------
length(intronsByTranscript(hg19_txdb))
length(fiveUTRsByTranscript(hg19_txdb))
length(threeUTRsByTranscript(hg19_txdb))

## ----extract------------------------------------------------------------------
suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg19))
genome <- BSgenome.Hsapiens.UCSC.hg19  # shorthand (for convenience)
tx_seqs1 <- extractTranscriptSeqs(genome, hg19_txdb, use.names=TRUE)

## ----translate1---------------------------------------------------------------
suppressWarnings(translate(tx_seqs1))

## ----betterTranslation--------------------------------------------------------
cds_seqs <- extractTranscriptSeqs(Hsapiens,
                                  cdsBy(hg19_txdb, by="tx", use.names=TRUE))
translate(cds_seqs)

## ----SessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()


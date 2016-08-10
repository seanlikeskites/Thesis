########################################################
# fonts!!
########################################################
library(extrafont)

########################################################
# load the data
########################################################
load("SAFE_Data.RData")

########################################################
# define the descriptors we want
########################################################
distDescriptors <- c("warm", "crunch", "fuzz", "cream", "bright", "harsh", "rasp", "smooth")
eqDescriptors <- c("warm", "bright", "clear", "thin", "air", "boom", "deep", "full", "tin", "mud", "box", "harsh")

########################################################
# filter out the descriptors we want
########################################################
source("descriptorPositions.r")
distProcSel <- getDescriptorPositions(distProc, distDescriptors)
distDiffSel <- getDescriptorPositions(distDiff, distDescriptors)
eqProcSel <- getDescriptorPositions(eqProc, eqDescriptors)
eqDiffSel <- getDescriptorPositions(eqDiff, eqDescriptors)

########################################################
# count the number of instances of each descriptor
########################################################
distTotalCount <- nrow(distProc)
distSelCount <- nrow(distProc[grep(paste("\\b", paste(distDescriptors, collapse="|\\b"), sep=""), rownames(distProc)),])
distCounts <- as.matrix(table(rownames(distProcSel)))
# 1 (harsh/crunch)
# 1 (warm/cream)
# 1 (warm/cream/fuzz)
# 2 (warm/fuzz)
# 1 (crunch/warm)

eqTotalCount <- nrow(eqProc)
eqSelCount <- nrow(eqProc[grepl(paste("\\b", paste(eqDescriptors, collapse="|\\b"), sep=""), rownames(eqProc)) &
		   	  !grepl("free", rownames(eqProc)),])
eqCounts <- as.matrix(table(rownames(eqProcSel)))
# 1 (deep/boom)
# 2 (tin/bright/clear)
# 1 (bright/thin)
# 1 (air/clear)
# 2 (air/bright)
# 3 (bright/clear)
# 1 (warm/bright)

########################################################
# apply stemming (no longer used)
########################################################
#source("stemming.r")
#rownames(distProc) <- safeStem(rownames(distProc))
#rownames(distDiff) <- safeStem(rownames(distDiff))
#rownames(eqProc) <- safeStem(rownames(eqProc))
#rownames(eqDiff) <- safeStem(rownames(eqDiff))

########################################################
# find centroids of terms
########################################################
termCentroids <- function(data)
{
	centroids <- apply(data, 2, function(x) tapply(x, rownames(data), mean))
}

distProcAvg <- termCentroids(distProcSel)
distDiffAvg <- termCentroids(distDiffSel)
eqProcAvg <- termCentroids(eqProcSel)
eqDiffAvg <- termCentroids(eqDiffSel)

########################################################
# do some clustering
########################################################
library(dendextend)
setEPS()
makePrettyDendrogram <- function(clusters, numColours)
{
	dend <- as.dendrogram(clusters)
	dend <- color_branches(dend, k=numColours)
	dend <- hang.dendrogram(dend, hang_height=0.1)
}

# distortion processed clusters
distProcClust <- hclust(dist(scale(distProcAvg)), method="ward.D2")
distProcDend <- makePrettyDendrogram(distProcClust, 3)
pdf("DistortionProcessedClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 0))
a <- plot(distProcDend, main=NA, sub=NA, xlim=c(20, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("DistortionProcessedClusters.pdf")

# distortion difference clusters
distDiffClust <- hclust(dist(scale(distDiffAvg)), method="ward.D2")
distDiffDend <- makePrettyDendrogram(distDiffClust, 3)
pdf("DistortionDifferenceClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 0))
a <- plot(distDiffDend, main=NA, sub=NA, xlim=c(25, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("DistortionDifferenceClusters.pdf")

# equaliser processed clusters
eqProcClust <- hclust(dist(scale(eqProcAvg)), method="ward.D2")
eqProcDend <- makePrettyDendrogram(eqProcClust, 6)
pdf("EqualiserProcessedClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 0))
a <- plot(eqProcDend, main=NA, sub=NA, xlim=c(25, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("EqualiserProcessedClusters.pdf")

# equaliser difference clusters
eqDiffClust <- hclust(dist(scale(eqDiffAvg)), method="ward.D2")
eqDiffDend <- makePrettyDendrogram(eqDiffClust, 5)
pdf("EqualiserDifferenceClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 0))
a <- plot(eqDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(eqDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("EqualiserDifferenceClusters.pdf")

# combined processed clusters
rownames(distProcAvg) <- paste("Distortion:", rownames(distProcAvg), sep="")
rownames(eqProcAvg) <- paste("Equaliser:", rownames(eqProcAvg), sep="")
combProcAvg <- rbind(distProcAvg, eqProcAvg)
combProcClust <- hclust(dist(scale(combProcAvg)), method="ward.D2")
combProcDend <- makePrettyDendrogram(combProcClust, 6)
pdf("CombinedProcessedClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 4))
a <- plot(combProcDend, main=NA, sub=NA, xlim=c(35, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("CombinedProcessedClusters.pdf")

# combined difference clusters
rownames(distDiffAvg) <- paste("Distortion:", rownames(distDiffAvg), sep="")
rownames(eqDiffAvg) <- paste("Equaliser:", rownames(eqDiffAvg), sep="")
combDiffAvg <- rbind(distDiffAvg, eqDiffAvg)
combDiffClust <- hclust(dist(scale(combDiffAvg)), method="ward.D2")
combDiffDend <- makePrettyDendrogram(combDiffClust, 6)
pdf("CombinedDifferenceClusters.pdf", pointsize=8, family="CM Sans", width=3, height=3)
par(mar=c(3, 0, 0, 4))
a <- plot(combDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(combDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()
embed_fonts("CombinedDifferenceClusters.pdf")

########################################################
# do some PCA
########################################################
source("plotPCA.r")
pcaPlotSize <- 2.95

# distortion processed PCA
distProcPCA <- prcomp(distProc, scale=TRUE)
distProcPCAPoints <- distProcPCA$x
distProcPCAPointsSel <- getDescriptorPositions(distProcPCAPoints, distDescriptors)

pdf("DistortionProcessedPCA1-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(distProcPCAPointsSel, c(1, 2), "topleft")
dev.off()
embed_fonts("DistortionProcessedPCA1-2.pdf")

pdf("DistortionProcessedPCA3-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(distProcPCAPointsSel, c(3, 2), "topleft")
dev.off()
embed_fonts("DistortionProcessedPCA3-2.pdf")

postscript("DistortionProcessedScree.eps")
a <- screeplot(distProcPCA, type="l")
dev.off()

# distortion difference PCA
distDiffPCA <- prcomp(distDiff, scale=TRUE)
distDiffPCAPoints <- distDiffPCA$x
distDiffPCAPointsSel <- getDescriptorPositions(distDiffPCAPoints, distDescriptors)

pdf("DistortionDifferencePCA1-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(distDiffPCAPointsSel, c(1, 2), "topright")
dev.off()
embed_fonts("DistortionDifferencePCA1-2.pdf")

pdf("DistortionDifferencePCA3-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(distDiffPCAPointsSel, c(3, 2), "topright")
dev.off()
embed_fonts("DistortionDifferencePCA3-2.pdf")

postscript("DistortionDifferenceScree.eps")
a <- screeplot(distDiffPCA, type="l")
dev.off()

# equaliser processed PCA
eqProcPCA <- prcomp(eqProc, scale=TRUE)
eqProcPCAPoints <- eqProcPCA$x
eqProcPCAPointsSel <- getDescriptorPositions(eqProcPCAPoints, eqDescriptors)

pdf("EqualiserProcessedPCA1-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(eqProcPCAPointsSel, c(1, 2), "topright")
dev.off()
embed_fonts("EqualiserProcessedPCA1-2.pdf")

pdf("EqualiserProcessedPCA3-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(eqProcPCAPointsSel, c(3, 2), "topleft", c(0.15, 0, 0, 0))
dev.off()
embed_fonts("EqualiserProcessedPCA3-2.pdf")

postscript("EqualiserProcessedScree.eps")
a <- screeplot(eqProcPCA, type="l")
dev.off()

# equaliser difference PCA
eqDiffPCA <- prcomp(eqDiff, scale=TRUE)
eqDiffPCAPoints <- eqDiffPCA$x
eqDiffPCAPointsSel <- getDescriptorPositions(eqDiffPCAPoints, eqDescriptors)

pdf("EqualiserDifferencePCA1-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(eqDiffPCAPointsSel, c(1, 2), "topright", c(0, 0.2, 0, 0))
dev.off()
embed_fonts("EqualiserDifferencePCA1-2.pdf")

pdf("EqualiserDifferencePCA3-2.pdf", pointsize=8, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 0.2, 0.2))
a <- plotIndividualPCA(eqDiffPCAPointsSel, c(3, 2), "bottomright")
dev.off()
embed_fonts("EqualiserDifferencePCA3-2.pdf")

postscript("EqualiserDifferenceScree.eps")
a <- screeplot(eqDiffPCA, type="l")
dev.off()

########################################################
# feature correlations
########################################################
source("correlation.r")

distProcCorr <- matrixCorrelationTest(distProcPCAPoints[,1:5], distProc)
distProcFeatures <- getSalientFeatures(distProcCorr, 0.7)
makeCorrelationList(distProcFeatures, "DistortionProcessedCorrelations.txt")
#makeCorrelationTable(distProcCorr$correlations[,distProcFeatures], "DistortionProcessedCorrelations.txt")

distDiffCorr <- matrixCorrelationTest(distDiffPCAPoints[,1:5], distDiff)
distDiffFeatures <- getSalientFeatures(distDiffCorr, 0.7)
makeCorrelationList(distDiffFeatures, "DistortionDifferenceCorrelations.txt")
#makeCorrelationTable(distDiffCorr$correlations[,distDiffFeatures], "DistortionDifferenceCorrelations.txt")

eqProcCorr <- matrixCorrelationTest(eqProcPCAPoints[,1:5], eqProc)
eqProcFeatures <- getSalientFeatures(eqProcCorr, 0.7)
makeCorrelationList(eqProcFeatures, "EqualiserProcessedCorrelations.txt")
#makeCorrelationTable(eqProcCorr$correlations[,eqProcFeatures], "EqualiserProcesedCorrelations.txt")

eqDiffCorr <- matrixCorrelationTest(eqDiffPCAPoints[,1:5], eqDiff)
eqDiffFeatures <- getSalientFeatures(eqDiffCorr, 0.7)
makeCorrelationList(eqDiffFeatures, "EqualiserDifferenceCorrelations.txt")
#makeCorrelationTable(eqDiffCorr$correlations[,eqDiffFeatures], "EqualiserDifferenceCorrelations.txt")

########################################################
# confidences
########################################################
source("agreement.r")

distProcPCAScaled <- scale(distProcPCAPoints)
distProcPCAScaledSel <- getDescriptorPositions(distProcPCAScaled, distDescriptors)
distProcAgreement <- termAgreement(distProcPCAScaledSel)

distDiffPCAScaled <- scale(distDiffPCAPoints)
distDiffPCAScaledSel <- getDescriptorPositions(distDiffPCAScaled, distDescriptors)
distDiffAgreement <- termAgreement(distDiffPCAScaledSel)

eqProcPCAScaled <- scale(eqProcPCAPoints)
eqProcPCAScaledSel <- getDescriptorPositions(eqProcPCAScaled, eqDescriptors)
eqProcAgreement <- termAgreement(eqProcPCAScaledSel)

eqDiffPCAScaled <- scale(eqDiffPCAPoints)
eqDiffPCAScaledSel <- getDescriptorPositions(eqDiffPCAScaled, eqDescriptors)
eqDiffAgreement <- termAgreement(eqDiffPCAScaledSel)

#########################################################
## biplots
#########################################################
# distortion processed PCA
distProcPlotFeatures1 <- c("Krimphoff Irregularity", "Spectral Roll Off", "MFCC 1")
pdf("DistortionProcessedCentroidsPCA1-2.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans",
    width=2.95, height=2.95)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distProcPCA, c(1, 2), distDescriptors, distProcPlotFeatures1, c(0.1, 0.1, 0, 0.1))
dev.off()
embed_fonts("DistortionProcessedCentroidsPCA1-2.pdf")

distProcPlotFeatures2 <- c("Spectral Roll Off", "MFCC 1", "MFCC 2", "Third Peak Tristimulus")
pdf("DistortionProcessedCentroidsPCA3-2.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans",
    width=2.95, height=2.95)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distProcPCA, c(3, 2), distDescriptors, distProcPlotFeatures2, c(0.15, 0.9, 0, 0.1))
dev.off()
embed_fonts("DistortionProcessedCentroidsPCA3-2.pdf")

# distortion difference PCA
distDiffPlotFeatures1 <- c("Krimphoff Irregularity", "Spectral Roll Off")
pdf("DistortionDifferenceCentroidsPCA1-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distDiffPCA, c(1, 2), distDescriptors, distDiffPlotFeatures1, c(0.1, 0.1, 0, 0))
dev.off()
embed_fonts("DistortionDifferenceCentroidsPCA1-2.pdf")

distDiffPlotFeatures2 <- c("Spectral Roll Off", "First Tristimulus")
pdf("DistortionDifferenceCentroidsPCA3-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distDiffPCA, c(3, 2), distDescriptors, distDiffPlotFeatures2, c(0.15, 0.05, 0, 0))
dev.off()
embed_fonts("DistortionDifferenceCentroidsPCA3-2.pdf")

# equaliser processed PCA
eqProcPlotFeatures1 <- c("Krimphoff Irregularity", "Harmonic Spectral Standard Deviation", "MFCC 1")
pdf("EqualiserProcessedCentroidsPCA1-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqProcPCA, c(1, 2), eqDescriptors, eqProcPlotFeatures1, c(0.1, 0.1, 0.15, 0))
dev.off()
embed_fonts("EqualiserProcessedCentroidsPCA1-2.pdf")

eqProcPlotFeatures2 <- c("Harmonic Spectral Standard Deviation", "MFCC 1", "MFCC 4")
pdf("EqualiserProcessedCentroidsPCA3-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqProcPCA, c(3, 2), eqDescriptors, eqProcPlotFeatures2, c(0.1, 0.1, 0.15, 0))
dev.off()
embed_fonts("EqualiserProcessedCentroidsPCA3-2.pdf")

# equaliser difference PCA
eqDiffPlotFeatures1 <- c("Krimphoff Irregularity", "Peak Spectral Centroid", "MFCC 1")
pdf("EqualiserDifferenceCentroidsPCA1-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqDiffPCA, c(1, 2), eqDescriptors, eqDiffPlotFeatures1, c(0.1, 0.05, 0, 0))
dev.off()
embed_fonts("EqualiserDifferenceCentroidsPCA1-2.pdf")

eqDiffPlotFeatures2 <- c("Peak Spectral Centroid", "MFCC 1", "MFCC 5")
pdf("EqualiserDifferenceCentroidsPCA3-2.pdf", pointsize=8, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqDiffPCA, c(3, 2), eqDescriptors, eqDiffPlotFeatures2, c(0.1, 0.05, 0, 0))
dev.off()
embed_fonts("EqualiserDifferenceCentroidsPCA3-2.pdf")

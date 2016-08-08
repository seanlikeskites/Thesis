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
postscript("DistortionProcessedClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 0))
a <- plot(distProcDend, main=NA, sub=NA, xlim=c(20, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# distortion difference clusters
distDiffClust <- hclust(dist(scale(distDiffAvg)), method="ward.D2")
distDiffDend <- makePrettyDendrogram(distDiffClust, 3)
postscript("DistortionDifferenceClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 0))
a <- plot(distDiffDend, main=NA, sub=NA, xlim=c(25, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# equaliser processed clusters
eqProcClust <- hclust(dist(scale(eqProcAvg)), method="ward.D2")
eqProcDend <- makePrettyDendrogram(eqProcClust, 6)
postscript("EqualiserProcessedClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 0))
a <- plot(eqProcDend, main=NA, sub=NA, xlim=c(25, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# equaliser difference clusters
eqDiffClust <- hclust(dist(scale(eqDiffAvg)), method="ward.D2")
eqDiffDend <- makePrettyDendrogram(eqDiffClust, 5)
postscript("EqualiserDifferenceClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 0))
a <- plot(eqDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(eqDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# combined processed clusters
rownames(distProcAvg) <- paste("Distortion:", rownames(distProcAvg), sep="")
rownames(eqProcAvg) <- paste("Equaliser:", rownames(eqProcAvg), sep="")
combProcAvg <- rbind(distProcAvg, eqProcAvg)
combProcClust <- hclust(dist(scale(combProcAvg)), method="ward.D2")
combProcDend <- makePrettyDendrogram(combProcClust, 6)
postscript("CombinedProcessedClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 4))
a <- plot(combProcDend, main=NA, sub=NA, xlim=c(35, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# combined difference clusters
rownames(distDiffAvg) <- paste("Distortion:", rownames(distDiffAvg), sep="")
rownames(eqDiffAvg) <- paste("Equaliser:", rownames(eqDiffAvg), sep="")
combDiffAvg <- rbind(distDiffAvg, eqDiffAvg)
combDiffClust <- hclust(dist(scale(combDiffAvg)), method="ward.D2")
combDiffDend <- makePrettyDendrogram(combDiffClust, 6)
postscript("CombinedDifferenceClusters.eps", pointsize=18)
par(mar=c(3, 0, 0, 4))
a <- plot(combDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(combDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

########################################################
# do some PCA
########################################################
source("plotPCA.r")

# distortion processed PCA
distProcPCA <- prcomp(distProc, scale=TRUE)
distProcPCAPoints <- distProcPCA$x
distProcPCAPointsSel <- getDescriptorPositions(distProcPCAPoints, distDescriptors)

postscript("DistortionProcessedPCA.eps", pointsize=18)
par(mar=c(4, 4, 0.5, 0.5))
a <- plotIndividualPCA(distProcPCAPointsSel, "topleft")
dev.off()

postscript("DistortionProcessedScree.eps")
a <- screeplot(distProcPCA, type="l")
dev.off()

# distortion difference PCA
distDiffPCA <- prcomp(distDiff, scale=TRUE)
distDiffPCAPoints <- distDiffPCA$x
distDiffPCAPointsSel <- getDescriptorPositions(distDiffPCAPoints, distDescriptors)

postscript("DistortionDifferencePCA.eps", pointsize=18)
par(mar=c(4, 4, 0.5, 0.5))
a <- plotIndividualPCA(distDiffPCAPointsSel, "topright")
dev.off()

postscript("DistortionDifferenceScree.eps")
a <- screeplot(distDiffPCA, type="l")
dev.off()

# equaliser processed PCA
eqProcPCA <- prcomp(eqProc, scale=TRUE)
eqProcPCAPoints <- eqProcPCA$x
eqProcPCAPointsSel <- getDescriptorPositions(eqProcPCAPoints, eqDescriptors)

postscript("EqualiserProcessedPCA.eps", pointsize=18)
par(mar=c(4, 4, 0.5, 0.5))
a <- plotIndividualPCA(eqProcPCAPointsSel, "topright")
dev.off()

postscript("EqualiserProcessedScree.eps")
a <- screeplot(eqProcPCA, type="l")
dev.off()

# equaliser difference PCA
eqDiffPCA <- prcomp(eqDiff, scale=TRUE)
eqDiffPCAPoints <- eqDiffPCA$x
eqDiffPCAPointsSel <- getDescriptorPositions(eqDiffPCAPoints, eqDescriptors)

postscript("EqualiserDifferencePCA.eps", pointsize=18)
par(mar=c(4, 4, 0.5, 0.5))
a <- plotIndividualPCA(eqDiffPCAPointsSel, "topleft")
dev.off()

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

########################################################
# biplots
########################################################
distProcPlotFeatures <- c("Krimphoff Irregularity", "Spectral Roll Off", "MFCC 1", "MFCC 4")
postscript("DistortionProcessedCentroidsPCA.eps", pointsize=18)
a <- plotCentroidBiplot(distProcPCA, distDescriptors, distProcPlotFeatures, c(0.4, 0.15, 0.15, 0.15))
dev.off()

distDiffPlotFeatures <- c("Krimphoff Irregularity", "Spectral Roll Off", "First Tristimulus")
postscript("DistortionDifferenceCentroidsPCA.eps", pointsize=18)
a <- plotCentroidBiplot(distDiffPCA, distDescriptors, distDiffPlotFeatures, c(0.2, 0.45, 0.15, 0.15))
dev.off()

eqProcPlotFeatures <- c("Krimphoff Irregularity", "Peak Spectral Centroid", "MFCC 1")
postscript("EqualiserProcessedCentroidsPCA.eps", pointsize=18)
a <- plotCentroidBiplot(eqProcPCA, eqDescriptors, eqProcPlotFeatures, c(0.25, 0.45, 0.15, 0.15))
dev.off()

eqDiffPlotFeatures <- c("Krimphoff Irregularity", "Harmonic Spectral Standard Deviation", "MFCC 1")
postscript("EqualiserDifferenceCentroidsPCA.eps", pointsize=18)
a <- plotCentroidBiplot(eqDiffPCA, eqDescriptors, eqDiffPlotFeatures, c(0.35, 0.25, 0.15, 0.15))
dev.off()

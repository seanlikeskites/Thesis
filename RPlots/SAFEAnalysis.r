########################################################
# load the data
########################################################
load("SAFE_Data.RData")

########################################################
# apply stemming
########################################################
source("stemming.r")
rownames(distProc) <- safeStem(rownames(distProc))
rownames(distDiff) <- safeStem(rownames(distDiff))
rownames(eqProc) <- safeStem(rownames(eqProc))
rownames(eqDiff) <- safeStem(rownames(eqDiff))

########################################################
# find centroids of terms
########################################################
termCentroids <- function(data)
{
	centroids <- apply(data, 2, function(x) tapply(x, rownames(data), mean))
}

distProcAvg <- termCentroids(distProc)
distDiffAvg <- termCentroids(distDiff)
eqProcAvg <- termCentroids(eqProc)
eqDiffAvg <- termCentroids(eqDiff)

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
postscript("DistortionProcessedClusters.eps")
par(mar=c(3.5, 0, 0, 0))
a <- plot(distProcDend, main=NA, sub=NA, xlim=c(20, 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# distortion difference clusters
distDiffClust <- hclust(dist(scale(distDiffAvg)), method="ward.D2")
distDiffDend <- makePrettyDendrogram(distDiffClust, 3)
postscript("DistortionDifferenceClusters.eps")
par(mar=c(3.5, 0, 0, 0))
a <- plot(distDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(distDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# equaliser processed clusters
eqProcClust <- hclust(dist(scale(eqProcAvg)), method="ward.D2")
eqProcDend <- makePrettyDendrogram(eqProcClust, 6)
postscript("EqualiserProcessedClusters.eps")
par(mar=c(3.5, 0, 0, 0))
a <- plot(eqProcDend, main=NA, sub=NA, xlim=c(ceiling(attr(eqProcDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# equaliser difference clusters
eqDiffClust <- hclust(dist(scale(eqDiffAvg)), method="ward.D2")
eqDiffDend <- makePrettyDendrogram(eqDiffClust, 5)
postscript("EqualiserDifferenceClusters.eps")
par(mar=c(3.5, 0, 0, 0))
a <- plot(eqDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(eqDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# combined processed clusters
rownames(distProcAvg) <- paste("Distortion:", rownames(distProcAvg), sep="")
rownames(eqProcAvg) <- paste("Equaliser:", rownames(eqProcAvg), sep="")
combProcAvg <- rbind(distProcAvg, eqProcAvg)
combProcClust <- hclust(dist(scale(combProcAvg)), method="ward.D2")
combProcDend <- makePrettyDendrogram(combProcClust, 6)
postscript("CombinedProcessedClusters.eps")
par(mar=c(3.5, 1, 0, 2.5))
a <- plot(combProcDend, main=NA, sub=NA, xlim=c(ceiling(attr(combProcDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

# combined difference clusters
rownames(distDiffAvg) <- paste("Distortion:", rownames(distDiffAvg), sep="")
rownames(eqDiffAvg) <- paste("Equaliser:", rownames(eqDiffAvg), sep="")
combDiffAvg <- rbind(distDiffAvg, eqDiffAvg)
combDiffClust <- hclust(dist(scale(combDiffAvg)), method="ward.D2")
combDiffDend <- makePrettyDendrogram(combDiffClust, 6)
postscript("CombinedDifferenceClusters.eps")
par(mar=c(3.5, 1, 0, 2.5))
a <- plot(combDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(combDiffDend, "height")), 0),
	  xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

########################################################
# do some PCA
########################################################
source("plotPCA.r")

# distortion processed PCA
distProcPCA <- prcomp(distProc, scale=TRUE)

postscript("DistortionProcessedPCA.eps")
a <- plotIndividualPCA(distProcPCA$x, "bottomleft")
dev.off()

postscript("DistortionProcessedScree.eps")
a <- screeplot(distProcPCA, type="l")
dev.off()

# distortion difference PCA
distDiffPCA <- prcomp(distDiff, scale=TRUE)

postscript("DistortionDifferencePCA.eps")
a <- plotIndividualPCA(distDiffPCA$x, "bottomleft")
dev.off()

postscript("DistortionDifferenceScree.eps")
a <- screeplot(distDiffPCA, type="l")
dev.off()

# equaliser processed PCA
eqProcPCA <- prcomp(eqProc, scale=TRUE)

postscript("EqualiserProcessedPCA.eps")
a <- plotIndividualPCA(eqProcPCA$x, "bottomright")
dev.off()

postscript("EqualiserProcessedScree.eps")
a <- screeplot(eqProcPCA, type="l")
dev.off()

# equaliser difference PCA
eqDiffPCA <- prcomp(eqDiff, scale=TRUE)

postscript("EqualiserDifferencePCA.eps")
a <- plotIndividualPCA(eqDiffPCA$x, "bottomright")
dev.off()

postscript("EqualiserDifferenceScree.eps")
a <- screeplot(eqDiffPCA, type="l")
dev.off()

########################################################
# feature correlations
########################################################
source("correlation.r")

distProcCorr <- matrixCorrelationTest(distProcPCA$x[,1:5], distProc)
distProcFeatures <- getSalientFeatures(distProcCorr, 0.8)
makeCorrelationTable(distProcCorr$correlations[,distProcFeatures], "DistortionProcessedCorrelations.txt")

distDiffCorr <- matrixCorrelationTest(distDiffPCA$x[,1:5], distDiff)
distDiffFeatures <- getSalientFeatures(distDiffCorr, 0.8)
makeCorrelationTable(distDiffCorr$correlations[,distDiffFeatures], "DistortionDifferenceCorrelations.txt")

eqProcCorr <- matrixCorrelationTest(eqProcPCA$x[,1:5], eqProc)
eqProcFeatures <- getSalientFeatures(eqProcCorr, 0.8)
makeCorrelationTable(eqProcCorr$correlations[,eqProcFeatures], "EqualiserProcesedCorrelations.txt")

eqDiffCorr <- matrixCorrelationTest(eqDiffPCA$x[,1:5], eqDiff)
eqDiffFeatures <- getSalientFeatures(eqDiffCorr, 0.8)
makeCorrelationTable(eqDiffCorr$correlations[,eqDiffFeatures], "EqualiserDifferenceCorrelations.txt")

########################################################
# biplots
########################################################
distProcPlotFeatures <- c("Irregularity_K", "Spectral_Roll_Off", "MFCC_1", "MFCC_4")
postscript("DistortionProcessedCentroidsPCA.eps")
a <- plotCentroidBiplot(distProcPCA, distProcPlotFeatures)
dev.off()

distDiffPlotFeatures <- c("Irregularity_K", "Peak_Spectral_Skewness", "Spectral_Roll_Off")
postscript("DistortionDifferenceCentroidsPCA.eps")
a <- plotCentroidBiplot(distDiffPCA, distDiffPlotFeatures, c(0.45, 0.15, 0.15, 0.15))
dev.off()

eqProcPlotFeatures <- c("Irregularity_K", "MFCC_10", "Harmonic_Spectral_Kurtosis", "Spectral_Flatness")
postscript("EqualiserProcessedCentroidsPCA.eps")
a <- plotCentroidBiplot(eqProcPCA, eqProcPlotFeatures, c(0.15, 0.4, 0.15, 0.15))
dev.off()

eqDiffPlotFeatures <- c("Irregularity_K", "MFCC_10", "Peak_Spectral_Centroid", "Spectral_Skewness")
postscript("EqualiserDifferenceCentroidsPCA.eps")
a <- plotCentroidBiplot(eqDiffPCA, eqDiffPlotFeatures, c(0.15, 0.2, 0.15, 0.15))
dev.off()

########################################################
# confidences
########################################################
source("confidence.r")
distProcConf <- termConfidence(distProcPCA)
distDiffConf <- termConfidence(distDiffPCA)
eqProcConf <- termConfidence(eqProcPCA)
eqDiffConf <- termConfidence(eqDiffPCA)

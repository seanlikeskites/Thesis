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

postscript("DistortionProcessedCentroidsPCA.eps")
a <- plotCentroidBiplot(distProcPCA, c("Irregularity_K", "MFCC_1", "Spectral_Roll_Off"))
dev.off()

# distortion difference PCA
distDiffPCA <- prcomp(distDiff, scale=TRUE)

postscript("DistortionDifferencePCA.eps")
a <- plotIndividualPCA(distDiffPCA$x, "bottomleft")
dev.off()

postscript("DistortionDifferenceCentroidsPCA.eps")
a <- plotCentroidBiplot(distDiffPCA, c("Irregularity_K", "Spectral_Variance"))
dev.off()

# equaliser processed PCA
eqProcPCA <- prcomp(eqProc, scale=TRUE)

postscript("EqualiserProcessedPCA.eps")
a <- plotIndividualPCA(eqProcPCA$x, "bottomright")
dev.off()

postscript("EqualiserProcessedCentroidsPCA.eps")
a <- plotCentroidBiplot(eqProcPCA, c("Peak_Tristimulus_2", "Spectral_Skewness"))
dev.off()

# equaliser difference PCA
eqDiffPCA <- prcomp(eqDiff, scale=TRUE)

postscript("EqualiserDifferencePCA.eps")
a <- plotIndividualPCA(eqDiffPCA$x, "bottomright")
dev.off()

postscript("EqualiserDifferenceCentroidsPCA.eps")
a <- plotCentroidBiplot(eqDiffPCA, c("Peak_Tristimulus_2", "Irregularity_K"))
dev.off()

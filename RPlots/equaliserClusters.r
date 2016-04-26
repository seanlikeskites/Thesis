library(SnowballC)
library(SAFER)
library(dendextend)
source("stemming.r")

makePrettyDendrogram <- function(clusters, numColours)
{
	dend <- as.dendrogram(clusters)
	dend <- color_branches(dend, k=numColours)
	dend <- hang.dendrogram(dend, hang_height=0.1)
}

load("equaliserData.RData")

setEPS()

processedFeatures <- processedMDS$Features
rownames(processedFeatures) <- safeStem(rownames(processedFeatures))
processedAverages <- apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean))
processedClusters <- hclust(dist(normalise(processedAverages)))
processedDend <- makePrettyDendrogram(processedClusters, 6)

postscript("EqualiserProcessedClusters.eps")
a <- plot(processedDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

differenceFeatures <- differenceMDS$Features
rownames(differenceFeatures) <- safeStem(rownames(differenceFeatures))
differenceAverages <- apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean))
differenceClusters <- hclust(dist(normalise(differenceAverages)))
differenceDend <- makePrettyDendrogram(differenceClusters, 5)

postscript("EqualiserDifferenceClusters.eps")
a <- plot(differenceDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

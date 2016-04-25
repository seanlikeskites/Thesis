library(SnowballC)
library(SAFER)
library(dendextend)

makePrettyDendrogram <- function(clusters, numColours)
{
	dend <- as.dendrogram(clusters)
	dend <- color_branches(dend, k=numColours)
	dend <- hang.dendrogram(dend, hang_height=0.1)
}

load("distortionData.RData")

setEPS()

processedFeatures <- processedMDS$Features
rownames(processedFeatures) <- sub("i$", "", wordStem(rownames(processedFeatures)))
processedAverages <- apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean))
processedClusters <- hclust(dist(normalise(processedAverages)))
processedDend <- makePrettyDendrogram(processedClusters, 3)

postscript("DistortionProcessedClusters.eps")
a <- plot(processedDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

differenceFeatures <- differenceMDS$Features
rownames(differenceFeatures) <- sub("i$", "", wordStem(rownames(differenceFeatures)))
differenceAverages <- apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean))
differenceClusters <- hclust(dist(normalise(differenceAverages)))
differenceDend <- makePrettyDendrogram(differenceClusters, 3)

postscript("DistortionDifferenceClusters.eps")
a <- plot(differenceDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

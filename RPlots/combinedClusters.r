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

# distortion
load("distortionData.RData")
processedFeatures <- processedMDS$Features
rownames(processedFeatures) <- paste("Distortion:", safeStem(rownames(processedFeatures)), sep="")
processedAverages <- apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean))

differenceFeatures <- differenceMDS$Features
rownames(differenceFeatures) <- paste("Distortion:", safeStem(rownames(differenceFeatures)), sep="")
differenceAverages <- apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean))

# equaliser
load("equaliserData.RData")
processedFeatures <- processedMDS$Features
rownames(processedFeatures) <- paste("Equaliser:", safeStem(rownames(processedFeatures)), sep="")
processedAverages <- rbind(processedAverages,
			   apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean)))

differenceFeatures <- differenceMDS$Features
rownames(differenceFeatures) <- paste("Equaliser:", safeStem(rownames(differenceFeatures)), sep="")
differenceAverages <- rbind(differenceAverages,
			    apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean)))


setEPS()

processedClusters <- hclust(dist(normalise(processedAverages)))
processedDend <- makePrettyDendrogram(processedClusters, 6)

postscript("CombinedProcessedClusters.eps")
a <- plot(processedDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

differenceClusters <- hclust(dist(normalise(differenceAverages)))
differenceDend <- makePrettyDendrogram(differenceClusters, 6)

postscript("CombinedDifferenceClusters.eps")
a <- plot(differenceDend, main=NA, sub=NA, xlab=NA, ylab=NA, horiz=TRUE)
dev.off()

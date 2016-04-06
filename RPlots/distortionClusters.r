library(SnowballC)

load("distortionData.RData")

setEPS()

processedFeatures <- processedMDS$Features
rownames(processedFeatures) <- sub("i$", "", wordStem(rownames(processedFeatures)))
processedAverages <- apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean))
processedClusters <- hclust(dist(processedAverages))

postscript("DistortionProcessedClusters.eps")
a <- plot(processedClusters, main=NA, sub=NA, xlab=NA, ylab=NA, xaxt="n", yaxt="n")
dev.off()

differenceFeatures <- differenceMDS$Features
rownames(differenceFeatures) <- sub("i$", "", wordStem(rownames(differenceFeatures)))
differenceAverages <- apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean))
differenceClusters <- hclust(dist(differenceAverages))

postscript("DistortionDifferenceClusters.eps")
a <- plot(differenceClusters, main=NA, sub=NA, xlab=NA, ylab=NA, xaxt="n", yaxt="n")
dev.off()

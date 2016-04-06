load("equaliserData.RData")

setEPS()

processedFeatures <- processedMDS$Features
processedAverages <- apply(processedFeatures, 2, function(x) tapply(x, rownames(processedFeatures), mean))
processedClusters <- hclust(dist(processedAverages))

postscript("EqualiserProcessedClusters.eps")
a <- plot(processedClusters, main=NA, sub=NA, xlab=NA, ylab=NA, xaxt="n", yaxt="n")
dev.off()

differenceFeatures <- differenceMDS$Features
differenceAverages <- apply(differenceFeatures, 2, function(x) tapply(x, rownames(differenceFeatures), mean))
differenceClusters <- hclust(dist(differenceAverages))

postscript("EqualiserDifferenceClusters.eps")
a <- plot(differenceClusters, main=NA, sub=NA, xlab=NA, ylab=NA, xaxt="n", yaxt="n")
dev.off()

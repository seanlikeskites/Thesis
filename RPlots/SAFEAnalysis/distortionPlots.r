library(SAFER)
library(SnowballC)
source("stemming.r")
source("plotMDS.r")

load("distortionData.RData")

rownames(processedMDS$Points) <- safeStem(rownames(processedMDS$Points))
rownames(differenceMDS$Points) <- safeStem(rownames(differenceMDS$Points))

setEPS()

postscript("DistortionProcessedCentroidsMDS.eps")
a <- plotMDS(processedMDS$Points, plotCentroids=TRUE)
dev.off()

postscript("DistortionProcessedMDS.eps")
a <- plotMDS(processedMDS$Points)
dev.off()

postscript("DistortionDifferenceCentroidsMDS.eps")
a <- plotMDS(differenceMDS$Points, plotCentroids=TRUE)
dev.off()

postscript("DistortionDifferenceMDS.eps")
a <- plotMDS(differenceMDS$Points)
dev.off()

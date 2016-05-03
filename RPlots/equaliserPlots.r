library(SAFER)
library(SnowballC)
source("stemming.r")
source("plotMDS.r")

load("equaliserData.RData")

rownames(processedMDS$Points) <- safeStem(rownames(processedMDS$Points))
rownames(differenceMDS$Points) <- safeStem(rownames(differenceMDS$Points))

setEPS()

postscript("EqualiserProcessedCentroidsMDS.eps")
a <- plotMDS(processedMDS$Points, plotCentroids=TRUE)
dev.off()

postscript("EqualiserProcessedMDS.eps")
a <- plotMDS(processedMDS$Points)
dev.off()

postscript("EqualiserDifferenceCentroidsMDS.eps")
a <- plotMDS(differenceMDS$Points, plotCentroids=TRUE)
dev.off()

postscript("EqualiserDifferenceMDS.eps")
a <- plotMDS(differenceMDS$Points)
dev.off()

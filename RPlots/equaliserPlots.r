library(SAFER)
library(SnowballC)
source("stemming.r")

load("equaliserData.RData")

rownames(processedMDS$Features) <- safeStem(rownames(processedMDS$Features))
rownames(processedMDS$Points) <- safeStem(rownames(processedMDS$Points))
rownames(differenceMDS$Features) <- safeStem(rownames(differenceMDS$Features))
rownames(differenceMDS$Points) <- safeStem(rownames(differenceMDS$Points))
descriptorsToPlot <- unique(rownames(processedMDS$Features))

setEPS()

postscript("EqualiserProcessedCentroidsMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("EqualiserProcessedMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("EqualiserProcessedTSNE.eps")
#plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

postscript("EqualiserDifferenceCentroidsMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("EqualiserDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("EqualiserDifferenceTSNE.eps")
#plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

library(SAFER)
library(SnowballC)
source("stemming.r")

load("distortionData.RData")

rownames(processedMDS$Features) <- safeStem(rownames(processedMDS$Features))
rownames(processedMDS$Points) <- safeStem(rownames(processedMDS$Points))
rownames(differenceMDS$Features) <- safeStem(rownames(differenceMDS$Features))
rownames(differenceMDS$Points) <- safeStem(rownames(differenceMDS$Points))
descriptorsToPlot <- unique(rownames(processedMDS$Features))

setEPS()

postscript("DistortionProcessedCentroidsMDS.eps")
a <- plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("DistortionProcessedMDS.eps")
a <- plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("DistortionProcessedTSNE.eps")
#plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", TRUE, FALSE)
#dev.off()

postscript("DistortionDifferenceCentroidsMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("DistortionDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("DistortionDifferenceTSNE.eps")
#plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", TRUE, FALSE)
#dev.off()

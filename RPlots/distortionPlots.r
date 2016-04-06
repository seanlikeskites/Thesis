library(SAFER)

load("distortionData.RData")
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

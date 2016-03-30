library(SAFER)

descriptorsToPlot <- c("warm", "bright", "fuzz", "crunch", "raspy", "harsh", "creamy", "smooth")
load("distortionData.RData")

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

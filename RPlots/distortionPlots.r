library(SAFER)

descriptorsToPlot <- c("warm", "bright", "fuzz", "fuzzy", "crunch", "crunchy", "raspy", "harsh")
load("distortionData.RData")

setEPS()

postscript("DistortionProcessedMDS.eps")
a <- plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("DistortionProcessedTSNE.eps")
plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("DistortionDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("DistortionDifferenceTSNE.eps")
plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

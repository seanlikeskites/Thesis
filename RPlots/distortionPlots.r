library(SAFER)

features <- c(SpectralFeatures, 30, 38:42)
descriptorsToPlot <- c("warm", "bright", "fuzz", "fuzzy", "crunch", "crunchy", "raspy", "harsh")

processedMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "MDS")
processedTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "MDS")
differenceTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "TSNE")

setEPS()

postscript("DistortionProcessedMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", FALSE)
dev.off()

postscript("DistortionProcessedTSNE.eps")
plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", FALSE)
dev.off()

postscript("DistortionDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", FALSE)
dev.off()

postscript("DistortionDifferenceTSNE.eps")
plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", FALSE)
dev.off()

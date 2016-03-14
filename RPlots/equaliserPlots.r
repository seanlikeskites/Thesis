library(SAFER)

features <- c(SpectralFeatures, 30, 38:42)
descriptorsToPlot <- c("air", "airy", "warm", "bright", "harsh", "sharp", "clear", "full")

processedMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "MDS")
#processedTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "MDS")
#differenceTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "TSNE")

setEPS()

postscript("EqualiserProcessedMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", FALSE)
dev.off()

#postscript("EqualiserProcessedTSNE.eps")
#plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

postscript("EqualiserDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", FALSE)
dev.off()

#postscript("EqualiserDifferenceTSNE.eps")
#plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

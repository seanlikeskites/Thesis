library(SAFER)

features <- c(SpectralFeatures, 30, 38:42)

processedMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "MDS")
processedTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "MDS")
differenceTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "TSNE")

save(processedMDS, processedTSNE, differenceMDS, differenceTSNE, file=("distortionData.RData"))

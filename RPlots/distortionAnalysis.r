library(SAFER)

features <- 1:80
descriptors <- c("warm", "bright", "fuzz", "fuzzy", "crunch", "raspy", "harsh", "creamy", "smooth", "crunchy")

processedMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "MDS", 2, descriptors)
# processedTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "MDS", 2, descriptors)
# differenceTSNE <- individualEntryFeatureSpace ("SAFEDistortion", features, "Differences", "TSNE")

save(processedMDS, differenceMDS, file=("distortionData.RData"))
# save(processedMDS, processedTSNE, differenceMDS, differenceTSNE, file=("distortionData.RData"))

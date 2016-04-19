library(SAFER)

features <- 1:80
descriptors <- c("warm", "bright", "brighter", "clear", "thin", "boomy", "airy", "air","muddy", "full", "deep", "tinny",
		 "harsh", "boxy", "box")

processedMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "MDS", 2, descriptors)
#processedTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "MDS", 2, descriptors)
#differenceTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "TSNE")

save(processedMDS, differenceMDS, file="equaliserData.RData")

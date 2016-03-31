library(SAFER)

features <- 1:80
descriptors <- c("warm", "bright", "clear", "thin", "boomy", "airy", "muddy", "full", "deep", "tinny", "harsh", 
		       "boxy", "clicky", "hollow", "tight")

processedMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "MDS", 2, descriptors)
#processedTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Processed", "TSNE")

differenceMDS <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "MDS", 2, descriptors)
#differenceTSNE <- individualEntryFeatureSpace ("SAFEEqualiser", features, "Differences", "TSNE")

save(processedMDS, differenceMDS, file="equaliserData.RData")

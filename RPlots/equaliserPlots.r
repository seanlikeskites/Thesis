library(SAFER)

descriptorsToPlot <- c("warm", "bright", "clear", "thin", "boomy", "airy", "muddy", "full", "deep", "tinny", "harsh", 
		       "boxy", "clicky", "hollow", "tight")
load("equaliserData.RData")

setEPS()

postscript("EqualiserProcessedCentroidsMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("EqualiserProcessedMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("EqualiserProcessedTSNE.eps")
#plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

postscript("EqualiserDifferenceCentroidsMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

postscript("EqualiserDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", FALSE, FALSE)
dev.off()

#postscript("EqualiserDifferenceTSNE.eps")
#plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()
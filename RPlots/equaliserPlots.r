library(SAFER)

descriptorsToPlot <- c("air", "airy", "warm", "bright", "harsh", "sharp", "clear", "full")
load("equaliserData.RData")

setEPS()

postscript("EqualiserProcessedMDS.eps")
plotTimbreSpace(processedMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

#postscript("EqualiserProcessedTSNE.eps")
#plotTimbreSpace(processedTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

postscript("EqualiserDifferenceMDS.eps")
plotTimbreSpace(differenceMDS, descriptorsToPlot, "Pretty", TRUE, FALSE)
dev.off()

#postscript("EqualiserDifferenceTSNE.eps")
#plotTimbreSpace(differenceTSNE, descriptorsToPlot, "Pretty", FALSE)
#dev.off()

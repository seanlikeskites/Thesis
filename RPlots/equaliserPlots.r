library(SAFER)
library(SnowballC)

load("equaliserData.RData")

rownames(processedMDS$Features) <- sub("i$", "", wordStem(rownames(processedMDS$Features)))
rownames(processedMDS$Points) <- sub("i$", "", wordStem(rownames(processedMDS$Points)))
rownames(differenceMDS$Features) <- sub("i$", "", wordStem(rownames(differenceMDS$Features)))
rownames(differenceMDS$Points) <- sub("i$", "", wordStem(rownames(differenceMDS$Points)))
descriptorsToPlot <- unique(rownames(processedMDS$Features))

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

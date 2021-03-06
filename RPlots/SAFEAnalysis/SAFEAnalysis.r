#########################################################
## fonts!!
#########################################################
#library(extrafont)
#
#########################################################
## load the data
#########################################################
#load("SAFE_Data.RData")
#
#source("combineData.r")
#combProc <- combineData(distProc, eqProc)
#combDiff <- combineData(distDiff, eqDiff)
#
#########################################################
## define the descriptors we want
#########################################################
#distDescriptors <- c("warm", "crunch", "fuzz", "cream", "bright", "harsh", "rasp", "smooth")
#eqDescriptors <- c("warm", "bright", "clear", "thin", "air", "boom", "deep", "full", "tin", "mud", "box", "harsh")
#combDescriptors <- c(paste("D:", distDescriptors, sep=""), paste("E:", eqDescriptors, sep=""))
#
#########################################################
## filter out the descriptors we want
#########################################################
#source("descriptorPositions.r")
#distProcSel <- getDescriptorPositions(distProc, distDescriptors)
#distDiffSel <- getDescriptorPositions(distDiff, distDescriptors)
#eqProcSel <- getDescriptorPositions(eqProc, eqDescriptors)
#eqDiffSel <- getDescriptorPositions(eqDiff, eqDescriptors)
#combProcSel <- getDescriptorPositions(combProc, combDescriptors)
#combDiffSel <- getDescriptorPositions(combDiff, combDescriptors)
#
#########################################################
## count the number of instances of each descriptor
#########################################################
#distTotalCount <- nrow(distProc)
#distSelCount <- nrow(distProc[grep(paste("\\b", paste(distDescriptors, collapse="|\\b"), sep=""), rownames(distProc)),])
#distCounts <- as.matrix(table(rownames(distProcSel)))
## 1 (harsh/crunch)
## 1 (warm/cream)
## 1 (warm/cream/fuzz)
## 2 (warm/fuzz)
## 1 (crunch/warm)
#
#eqTotalCount <- nrow(eqProc)
#eqSelCount <- nrow(eqProc[grepl(paste("\\b", paste(eqDescriptors, collapse="|\\b"), sep=""), rownames(eqProc)) &
#		   	  !grepl("free", rownames(eqProc)),])
#eqCounts <- as.matrix(table(rownames(eqProcSel)))
## 1 (deep/boom)
## 2 (tin/bright/clear)
## 1 (bright/thin)
## 1 (air/clear)
## 2 (air/bright)
## 3 (bright/clear)
## 1 (warm/bright)
#
#########################################################
## apply stemming (no longer used)
#########################################################
##source("stemming.r")
##rownames(distProc) <- safeStem(rownames(distProc))
##rownames(distDiff) <- safeStem(rownames(distDiff))
##rownames(eqProc) <- safeStem(rownames(eqProc))
##rownames(eqDiff) <- safeStem(rownames(eqDiff))
#
#########################################################
## find centroids of terms
#########################################################
#termCentroids <- function(data)
#{
#	centroids <- apply(data, 2, function(x) tapply(x, rownames(data), mean))
#}
#
#distProcAvg <- termCentroids(distProcSel)
#distDiffAvg <- termCentroids(distDiffSel)
#eqProcAvg <- termCentroids(eqProcSel)
#eqDiffAvg <- termCentroids(eqDiffSel)
#combProcAvg <- termCentroids(combProcSel)
#combDiffAvg <- termCentroids(combDiffSel)
#
#########################################################
## do some clustering
#########################################################
#library(dendextend)
#plotPointSize <- 9
#
#setEPS()
#makePrettyDendrogram <- function(clusters, numColours)
#{
#	dend <- as.dendrogram(clusters)
#	dend <- color_branches(dend, k=numColours)
#	dend <- hang.dendrogram(dend, hang_height=0.3)
#}
#
## distortion processed clusters
#distProcClust <- hclust(dist(scale(distProcAvg)), method="ward.D2")
#distProcDend <- makePrettyDendrogram(distProcClust, 3)
#pdf("DistortionProcessedClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0))
#a <- plot(distProcDend, main=NA, sub=NA, xlim=c(20, 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("DistortionProcessedClusters.pdf")
#
## distortion difference clusters
#distDiffClust <- hclust(dist(scale(distDiffAvg)), method="ward.D2")
#distDiffDend <- makePrettyDendrogram(distDiffClust, 3)
#pdf("DistortionDifferenceClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0))
#a <- plot(distDiffDend, main=NA, sub=NA, xlim=c(25, 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("DistortionDifferenceClusters.pdf")
#
## equaliser processed clusters
#eqProcClust <- hclust(dist(scale(eqProcAvg)), method="ward.D2")
#eqProcDend <- makePrettyDendrogram(eqProcClust, 6)
#pdf("EqualiserProcessedClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0))
#a <- plot(eqProcDend, main=NA, sub=NA, xlim=c(25, 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("EqualiserProcessedClusters.pdf")
#
## equaliser difference clusters
#eqDiffClust <- hclust(dist(scale(eqDiffAvg)), method="ward.D2")
#eqDiffDend <- makePrettyDendrogram(eqDiffClust, 5)
#pdf("EqualiserDifferenceClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0))
#a <- plot(eqDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(eqDiffDend, "height")), 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("EqualiserDifferenceClusters.pdf")
#
## combined processed clusters
#combProcClust <- hclust(dist(scale(combProcAvg)), method="ward.D2")
#combProcDend <- makePrettyDendrogram(combProcClust, 6)
#pdf("CombinedProcessedClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0.5))
#a <- plot(combProcDend, main=NA, sub=NA, xlim=c(35, 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("CombinedProcessedClusters.pdf")
#
## combined difference clusters
#combDiffClust <- hclust(dist(scale(combDiffAvg)), method="ward.D2")
#combDiffDend <- makePrettyDendrogram(combDiffClust, 6)
#pdf("CombinedDifferenceClusters.pdf", pointsize=plotPointSize, family="CM Sans", width=3, height=3)
#par(mar=c(3, 0, 0, 0.5))
#a <- plot(combDiffDend, main=NA, sub=NA, xlim=c(ceiling(attr(combDiffDend, "height")), 0),
#	  xlab=NA, ylab=NA, horiz=TRUE)
#dev.off()
#embed_fonts("CombinedDifferenceClusters.pdf")
#
## save cluster distances
#distProcDist <- as.matrix(cophenetic(distProcClust))
#distDiffDist <- as.matrix(cophenetic(distDiffClust))
#eqProcDist <- as.matrix(cophenetic(eqProcClust))
#eqDiffDist <- as.matrix(cophenetic(eqDiffClust))
#combProcDist <- as.matrix(cophenetic(combProcClust))
#combDiffDist <- as.matrix(cophenetic(combDiffClust))
#save(distProcDist, distDiffDist, eqProcDist, eqDiffDist, combProcDist, combDiffDist, file="ClusterDistances.RData")
#
#########################################################
## do some PCA
#########################################################
#source("plotPCA.r")
#pcaPlotSize <- 2.95
#
## distortion processed PCA
#distProcPCA <- prcomp(distProc, scale=TRUE)
#distProcPCAPoints <- distProcPCA$x
#distProcPCAPointsSel <- getDescriptorPositions(distProcPCAPoints, distDescriptors)
#
#pdf("DistortionProcessedPCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(distProcPCAPointsSel, c(1, 2), "topleft", c(-15, 5, -10, 15))
#dev.off()
#embed_fonts("DistortionProcessedPCA1-2.pdf")
#
#pdf("DistortionProcessedPCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(distProcPCAPointsSel, c(3, 2), "topleft", c(-10, 5, -10, 15))
#dev.off()
#embed_fonts("DistortionProcessedPCA3-2.pdf")
#
#postscript("DistortionProcessedScree.eps")
#a <- screeplot(distProcPCA, type="l")
#dev.off()
#
## distortion difference PCA
#distDiffPCA <- prcomp(distDiff, scale=TRUE)
#distDiffPCAPoints <- distDiffPCA$x
#distDiffPCAPointsSel <- getDescriptorPositions(distDiffPCAPoints, distDescriptors)
#
#pdf("DistortionDifferencePCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(distDiffPCAPointsSel, c(1, 2), "topright", c(-5, 15, -5, 20))
#dev.off()
#embed_fonts("DistortionDifferencePCA1-2.pdf")
#
#pdf("DistortionDifferencePCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(distDiffPCAPointsSel, c(3, 2), "topright", c(-8, 6, -5, 20))
#dev.off()
#embed_fonts("DistortionDifferencePCA3-2.pdf")
#
#postscript("DistortionDifferenceScree.eps")
#a <- screeplot(distDiffPCA, type="l")
#dev.off()
#
## equaliser processed PCA
#eqProcPCA <- prcomp(eqProc, scale=TRUE)
#eqProcPCAPoints <- eqProcPCA$x
#eqProcPCAPointsSel <- getDescriptorPositions(eqProcPCAPoints, eqDescriptors)
#eqPalette <- rainbow(length(eqDescriptors))
#eqPalette[eqPalette == "#FFFF00FF"] <- "#FFD700FF"
#eqRemainderPalette <- eqPalette[c(1:3, 5:11)]
#eqBrightWarmPalette <- eqPalette[c(4, 12)]
#
## warm and bright
#eqProcBrightWarm <- eqProcPCAPointsSel[grepl("warm|bright", rownames(eqProcPCAPointsSel)),]
#
#pdf("EqualiserBWProcessedPCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqProcBrightWarm, c(1, 2), "topright", c(-5, 15, -10, 15), colourPalette=eqBrightWarmPalette)
#dev.off()
#embed_fonts("EqualiserBWProcessedPCA1-2.pdf")
#
#pdf("EqualiserBWProcessedPCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqProcBrightWarm, c(3, 2), "topright", c(-5, 30, -10, 15), colourPalette=eqBrightWarmPalette)
#dev.off()
#embed_fonts("EqualiserBWProcessedPCA3-2.pdf")
#
## remainder
#eqProcRemainder <- eqProcPCAPointsSel[!grepl("warm|bright", rownames(eqProcPCAPointsSel)),]
#
#pdf("EqualiserProcessedPCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqProcRemainder, c(1, 2), "topright", c(-5, 30, -10, 20), colourPalette=eqRemainderPalette)
#dev.off()
#embed_fonts("EqualiserProcessedPCA1-2.pdf")
#
#pdf("EqualiserProcessedPCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqProcRemainder, c(3, 2), "topleft", c(-15, 5, -10, 20), colourPalette=eqRemainderPalette)
#dev.off()
#embed_fonts("EqualiserProcessedPCA3-2.pdf")
#
#postscript("EqualiserProcessedScree.eps")
#a <- screeplot(eqProcPCA, type="l")
#dev.off()
#
## equaliser difference PCA
#eqDiffPCA <- prcomp(eqDiff, scale=TRUE)
#eqDiffPCAPoints <- eqDiffPCA$x
#eqDiffPCAPointsSel <- getDescriptorPositions(eqDiffPCAPoints, eqDescriptors)
#
## warm and bright
#eqDiffBrightWarm <- eqDiffPCAPointsSel[grepl("warm|bright", rownames(eqDiffPCAPointsSel)),]
#
#pdf("EqualiserBWDifferencePCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqDiffBrightWarm, c(1, 2), "topleft", c(-20, 10, -15, 25), colourPalette=eqBrightWarmPalette)
#dev.off()
#embed_fonts("EqualiserBWDifferencePCA1-2.pdf")
#
#pdf("EqualiserBWDifferencePCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqDiffBrightWarm, c(3, 2), "topleft", c(-10, 25, -15, 25), colourPalette=eqBrightWarmPalette)
#dev.off()
#embed_fonts("EqualiserBWDifferencePCA3-2.pdf")
#
## remainder
#eqDiffRemainder <- eqDiffPCAPointsSel[!grepl("warm|bright", rownames(eqDiffPCAPointsSel)),]
#
#pdf("EqualiserDifferencePCA1-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqDiffRemainder, c(1, 2), "topleft", c(-30, 10, -20, 20), colourPalette=eqRemainderPalette, legendncol=2)
#dev.off()
#embed_fonts("EqualiserDifferencePCA1-2.pdf")
#
#pdf("EqualiserDifferencePCA3-2.pdf", pointsize=plotPointSize, family="CM Sans", width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 0.5, 0.5))
#a <- plotIndividualPCA(eqDiffRemainder, c(3, 2), "topright", c(-6, 8, -20, 20), colourPalette=eqRemainderPalette, legendncol=2)
#dev.off()
#embed_fonts("EqualiserDifferencePCA3-2.pdf")
#
#postscript("EqualiserDifferenceScree.eps")
#a <- screeplot(eqDiffPCA, type="l")
#dev.off()
#
## combined processed
#combProcPCA <- prcomp(combProc, scale=TRUE)
#combProcPCAPoints <- combProcPCA$x
#combProcPCAPointsSel <- getDescriptorPositions(combProcPCAPoints, combDescriptors)
#
## combined difference
#combDiffPCA <- prcomp(combDiff, scale=TRUE)
#combDiffPCAPoints <- combDiffPCA$x
#combDiffPCAPointsSel <- getDescriptorPositions(combDiffPCAPoints, combDescriptors)
#
## save PCAs
#save(distProcPCA, distDiffPCA, eqProcPCA, eqDiffPCA, combProcPCA, combDiffPCA, file="PCAData.RData")
#
#########################################################
## feature correlations
#########################################################
#source("correlation.r")
#
#distProcCorr <- matrixCorrelationTest(distProcPCAPoints[,1:5], distProc)
#distProcFeatures <- getSalientFeatures(distProcCorr, 0.7)
#makeCorrelationList(distProcFeatures, "DistortionProcessedCorrelations.tex")
##makeCorrelationTable(distProcCorr$correlations[,distProcFeatures], "DistortionProcessedCorrelations.txt")
#
#distDiffCorr <- matrixCorrelationTest(distDiffPCAPoints[,1:5], distDiff)
#distDiffFeatures <- getSalientFeatures(distDiffCorr, 0.7)
#makeCorrelationList(distDiffFeatures, "DistortionDifferenceCorrelations.tex")
##makeCorrelationTable(distDiffCorr$correlations[,distDiffFeatures], "DistortionDifferenceCorrelations.txt")
#
#eqProcCorr <- matrixCorrelationTest(eqProcPCAPoints[,1:5], eqProc)
#eqProcFeatures <- getSalientFeatures(eqProcCorr, 0.7)
#makeCorrelationList(eqProcFeatures, "EqualiserProcessedCorrelations.tex")
##makeCorrelationTable(eqProcCorr$correlations[,eqProcFeatures], "EqualiserProcesedCorrelations.txt")
#
#eqDiffCorr <- matrixCorrelationTest(eqDiffPCAPoints[,1:5], eqDiff)
#eqDiffFeatures <- getSalientFeatures(eqDiffCorr, 0.7)
#makeCorrelationList(eqDiffFeatures, "EqualiserDifferenceCorrelations.tex")
##makeCorrelationTable(eqDiffCorr$correlations[,eqDiffFeatures], "EqualiserDifferenceCorrelations.txt")
#
#combProcCorr <- matrixCorrelationTest(combProcPCAPoints[,1:5], combProc)
#combProcFeatures <- getSalientFeatures(combProcCorr, 0.7)
#makeCorrelationList(combProcFeatures, "CombinedProcessedCorrelations.tex")
#
#combDiffCorr <- matrixCorrelationTest(combDiffPCAPoints[,1:5], combDiff)
#combDiffFeatures <- getSalientFeatures(combDiffCorr, 0.7)
#makeCorrelationList(combDiffFeatures, "CombinedDifferenceCorrelations.tex")
#
#########################################################
## confidences
#########################################################
#source("agreement.r")
#
#distProcPCAScaled <- scale(distProcPCAPoints)
#distProcPCAScaledSel <- getDescriptorPositions(distProcPCAScaled, distDescriptors)
#distProcAgreement <- termAgreement(distProcPCAScaledSel)
#makeAgreementTable(distProcAgreement, "DistortionProcessedAgreements.tex")
#
#distDiffPCAScaled <- scale(distDiffPCAPoints)
#distDiffPCAScaledSel <- getDescriptorPositions(distDiffPCAScaled, distDescriptors)
#distDiffAgreement <- termAgreement(distDiffPCAScaledSel)
#makeAgreementTable(distDiffAgreement, "DistortionDifferenceAgreements.tex")
#
#eqProcPCAScaled <- scale(eqProcPCAPoints)
#eqProcPCAScaledSel <- getDescriptorPositions(eqProcPCAScaled, eqDescriptors)
#eqProcAgreement <- termAgreement(eqProcPCAScaledSel)
#makeAgreementTable(eqProcAgreement, "EqualiserProcessedAgreements.tex")
#
#eqDiffPCAScaled <- scale(eqDiffPCAPoints)
#eqDiffPCAScaledSel <- getDescriptorPositions(eqDiffPCAScaled, eqDescriptors)
#eqDiffAgreement <- termAgreement(eqDiffPCAScaledSel)
#makeAgreementTable(eqDiffAgreement, "EqualiserDifferenceAgreements.tex")
#
#combProcPCAScaled <- scale(combProcPCAPoints)
#combProcPCAScaledSel <- getDescriptorPositions(combProcPCAScaled, combDescriptors)
#combProcAgreement <- termAgreement(combProcPCAScaledSel)
#makeAgreementTable(combProcAgreement, "CombinedProcessedAgreements.tex")
#
#combDiffPCAScaled <- scale(combDiffPCAPoints)
#combDiffPCAScaledSel <- getDescriptorPositions(combDiffPCAScaled, combDescriptors)
#combDiffAgreement <- termAgreement(combDiffPCAScaledSel)
#makeAgreementTable(combDiffAgreement, "CombinedDifferenceAgreements.tex")
#
#########################################################
## biplots
#########################################################
# distortion processed PCA
distProcPlotFeatures1 <- c("Krimphoff Irregularity", "Spectral Roll Off", "MFCC 1")
pdf("DistortionProcessedCentroidsPCA1-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman", "CM Sans"), family="CM Sans",
    width=2.95, height=2.95)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distProcPCA, c(1, 2), distDescriptors, distProcPlotFeatures1, c(-2.2, 2.2, -2.2, 2.2))
dev.off()
embed_fonts("DistortionProcessedCentroidsPCA1-2.pdf")

distProcPlotFeatures2 <- c("Spectral Roll Off", "MFCC 1", "MFCC 2", "Third Peak Tristimulus")
pdf("DistortionProcessedCentroidsPCA3-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman", "CM Sans"), family="CM Sans",
    width=2.95, height=2.95)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distProcPCA, c(3, 2), distDescriptors, distProcPlotFeatures2, c(-6, 6, -2.2, 2.2))
dev.off()
embed_fonts("DistortionProcessedCentroidsPCA3-2.pdf")

# distortion difference PCA
distDiffPlotFeatures1 <- c("Krimphoff Irregularity", "Spectral Roll Off")
pdf("DistortionDifferenceCentroidsPCA1-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distDiffPCA, c(1, 2), distDescriptors, distDiffPlotFeatures1, c(-3, 3, -2, 1.05),
			ax2=c(-2, -1, 0, 1), ax3=c(-30, -15, 0, 15, 30), ax4=c(-20, -10, 0, 10))
dev.off()
embed_fonts("DistortionDifferenceCentroidsPCA1-2.pdf")

distDiffPlotFeatures2 <- c("Spectral Roll Off", "First Tristimulus")
pdf("DistortionDifferenceCentroidsPCA3-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(distDiffPCA, c(3, 2), distDescriptors, distDiffPlotFeatures2, c(-3, 3, -2, 1.05),
			ax2=c(-2, -1, 0, 1), ax4=c(-20, -10, 0, 10))
dev.off()
embed_fonts("DistortionDifferenceCentroidsPCA3-2.pdf")

# equaliser processed PCA
eqProcPlotFeatures1 <- c("Krimphoff Irregularity", "Harmonic Spectral Standard Deviation", "MFCC 1")
pdf("EqualiserProcessedCentroidsPCA1-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqProcPCA, c(1, 2), eqDescriptors, eqProcPlotFeatures1, c(-4, 4, -4, 4),
			ax3=c(-60, -30, 0, 30, 60))
dev.off()
embed_fonts("EqualiserProcessedCentroidsPCA1-2.pdf")

eqProcPlotFeatures2 <- c("Harmonic Spectral Standard Deviation", "MFCC 1", "MFCC 4")
pdf("EqualiserProcessedCentroidsPCA3-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqProcPCA, c(3, 2), eqDescriptors, eqProcPlotFeatures2, c(-4, 3, -4, 4),
			ax3=c(-40, -20, 0, 20))
dev.off()
embed_fonts("EqualiserProcessedCentroidsPCA3-2.pdf")

# equaliser difference PCA
eqDiffPlotFeatures1 <- c("Krimphoff Irregularity", "Peak Spectral Centroid", "MFCC 1")
pdf("EqualiserDifferenceCentroidsPCA1-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqDiffPCA, c(1, 2), eqDescriptors, eqDiffPlotFeatures1, c(-4, 2, -6, 6))
dev.off()
embed_fonts("EqualiserDifferenceCentroidsPCA1-2.pdf")

eqDiffPlotFeatures2 <- c("Peak Spectral Centroid", "MFCC 1", "MFCC 5")
pdf("EqualiserDifferenceCentroidsPCA3-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
    width=pcaPlotSize, height=pcaPlotSize)
par(mar=c(4, 4, 2, 2))
a <- plotCentroidBiplot(eqDiffPCA, c(3, 2), eqDescriptors, eqDiffPlotFeatures2, c(-4, 3, -6, 6))
dev.off()
embed_fonts("EqualiserDifferenceCentroidsPCA3-2.pdf")

## combined processed PCA
#combProcPlotFeatures1 <- c("Krimphoff Irregularity", "Harmonic Spectral Standard Deviation", "MFCC 1")
#pdf("CombinedProcessedCentroidsPCA1-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
#    width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 2, 2))
#a <- plotCentroidBiplot(combProcPCA, c(1, 2), combDescriptors, combProcPlotFeatures1, c(0.2, 0.15, 0.1, 0))
#dev.off()
#embed_fonts("CombinedProcessedCentroidsPCA1-2.pdf")
#
#combProcPlotFeatures2 <- c("Harmonic Spectral Standard Deviation", "MFCC 1", "MFCC 2")
#pdf("CombinedProcessedCentroidsPCA3-2.pdf", pointsize=plotPointSize, fonts=c("CM Roman","CM Sans"), family="CM Sans",
#    width=pcaPlotSize, height=pcaPlotSize)
#par(mar=c(4, 4, 2, 2))
#a <- plotCentroidBiplot(combProcPCA, c(3, 2), combDescriptors, combProcPlotFeatures2, c(0.15, 0.25, 0.15, 0))
#dev.off()
#embed_fonts("CombinedProcessedCentroidsPCA3-2.pdf")
#
##########################################################
### plot some spectra
##########################################################
#source("plotSpectralChange.r")
#
## distortion processed
#pdf("DistortionProcessedSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
#plotSpectrum(distProcSel, "topright", 2)
#dev.off()
#embed_fonts("DistortionProcessedSpectra.pdf")
#
## distortion difference
#distUnprocSel <- distProcSel - distDiffSel
#pdf("DistortionDifferenceSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
#plotSpectralChange(distProcSel, distUnprocSel, "bottomleft", 2, c(0.05, 0))
#dev.off()
#embed_fonts("DistortionDifferenceSpectra.pdf")
#
## equaliser processed
#pdf("EqualiserProcessedSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
#plotSpectrum(eqProcSel, "topright", 4)
#dev.off()
#embed_fonts("EqualiserProcessedSpectra.pdf")
#
## equaliser difference
#eqUnprocSel <- eqProcSel - eqDiffSel
#pdf("EqualiserDifferenceSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
#plotSpectralChange(eqProcSel, eqUnprocSel, "bottomright", 3, c(0.3, 0))
#dev.off()
#embed_fonts("EqualiserDifferenceSpectra.pdf")

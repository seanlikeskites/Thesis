# probably just use mahalanobis distance here
load("../../SAFEAnalysis/PCAData.RData")
load("ValidationMeans.RData")
source("../../SAFEAnalysis/descriptorPositions.r")

descriptors <- c("E:warm", "D:warm", "E:bright", "D:bright", "E:harsh", "D:harsh", "D:crunch")

getPCACoords <- function(means, pca)
{
	means <- scale(means, center=pca$center, scale=pca$scale)
	coords <- means %*% pca$rotation
	return(coords)
}

plotPCAWithExtraPoints <- function(points, dims, legendPos, extraPoints, border=c(0, 0, 0, 0), colourPalette=NULL, legendcol=1)
{
	descriptors <- rownames(points)
	uniqueDescriptors <- sort(unique(descriptors))

	if (is.null(colourPalette))
	{
		colourPalette <- rainbow(length(uniqueDescriptors))
		colourPalette[colourPalette == "#FFFF00FF"] <- "#FFD700FF"
	}

	colours <- "black"

	for (i in 1:length(uniqueDescriptors))
	{
		colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
	}

	xs <- points[,dims[1]]
	ys <- points[,dims[2]]

	xRange <- diff(range(xs))
	xLimits <- c(min(xs) - border[1] * xRange, max(xs) + border[2] * xRange)

	yRange <- diff(range(ys))
	yLimits <- c(min(ys) - border[3] * yRange, max(ys) + border[4] * yRange)

	xLabel <- paste("PC ", dims[1], sep="")
	yLabel <- paste("PC ", dims[2], sep="")

	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel, xlim=xLimits, ylim=yLimits)
	points(xs, ys, pch=20, col=colours, cex=1.5)
	legend(legendPos, legend=uniqueDescriptors, pch=20, pt.cex=1.5,
	       col=colourPalette, ncol=legendcol)

	
	extraNames <- rownames(extraPoints)
	extraXs <- extraPoints[,dims[1]]
	extraYs <- extraPoints[,dims[2]]
	text(extraXs, extraYs, extraNames, cex=1)

	box()
}

combProcPoints <- getDescriptorPositions(combProcPCA$x, descriptors)
procCrunch <- getPCACoords(crunchMeans$Crunch$Processed, combProcPCA)
#plotPCAWithExtraPoints(combProcPoints, c(1, 2), "topright", procCrunch, legendcol=3)

combDiffPoints <- getDescriptorPositions(combDiffPCA$x, descriptors)
diffCrunch <- getPCACoords(crunchMeans$Harsh$Processed - crunchMeans$Harsh$Unprocessed, combDiffPCA)
plotPCAWithExtraPoints(combDiffPoints, c(1, 2), "topright", diffCrunch, legendcol=3)

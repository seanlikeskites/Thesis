source("agreement.r")
source("descriptorPositions.r")

plotIndividualPCA <- function(points, legendPos)
{
	descriptors <- rownames(points)
	uniqueDescriptors <- sort(unique(descriptors))
	colourPalette <- rainbow(length(uniqueDescriptors))

	colours <- "black"

	for (i in 1:length(uniqueDescriptors))
	{
		colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
	}

	xs <- points[,1]
	ys <- points[,2]

	xLabel <- "PC 1"
	yLabel <- "PC 2"

	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel)
	points(xs, ys, pch=4, col=colours, cex=2)
	legend(legendPos, legend=uniqueDescriptors, pch=4, pt.cex=2, col=colourPalette)
}

plotCentroidBiplot <- function(PCA, desc, var, border=c(0.15, 0.15, 0.15, 0.15))
{
	data <- PCA$x
	points <- getDescriptorPositions(data, desc)
	scaledData <- scale(data)
	scaledPoints <- getDescriptorPositions(scaledData, desc)
	centroids <- apply(points, 2, function(x) tapply(x, rownames(points), mean))

	descriptors <- rownames(centroids)
	uniqueDescriptors <- sort(unique(descriptors))
	colourPalette <- rainbow(length(uniqueDescriptors))

	termAgreements <- termAgreement(scaledPoints)
	agreements <- array(0, length(uniqueDescriptors))

	colours <- "black"

	for (i in 1:length(uniqueDescriptors))
	{
		colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
		agreements[descriptors == uniqueDescriptors[i]] <- termAgreements[i]
	}

	xs <- centroids[,1]
	ys <- centroids[,2]

	xLabel <- "PC 1"
	yLabel <- "PC 2"

	xRange <- diff(range(xs))
	xLimits <- c(min(xs) - border[1] * xRange, max(xs) + border[2] * xRange)

	yRange <- diff(range(ys))
	yLimits <- c(min(ys) - border[3] * yRange, max(ys) + border[4] * yRange)

	n <- nrow(PCA$x)
	scaleX <- PCA$sdev[1] * sqrt(n)
	varXs <- PCA$rotation[var,1] * scaleX

	scaleY <- PCA$sdev[2] * sqrt(n)
	varYs <- PCA$rotation[var,2] * scaleY

	par(mar=c(4, 4, 4, 4))
	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel, xlim=xLimits, ylim=yLimits)
	text(xs, ys, descriptors, col=colours, cex=1.5+agreements)

	xLimScale <- max(abs(varXs)) / max(abs(xs)) * 1.3
	xLimits <- xLimits * xLimScale
	yLimScale <- max(abs(varYs)) / max(abs(ys)) * 1.3
	yLimits <- yLimits * yLimScale

	labelPos <- integer(length=length(var))

	for (i in 1:length(var))
	{
		if (varXs[i] > 0)
			xLim <- max(xLimits)
		else
			xLim <- min(xLimits)

		if (varYs[i] > 0)
			yLim <- max(yLimits)
		else
			yLim <- min(yLimits)

		xMag <- varXs[i] / xLim
		yMag <- varYs[i] / yLim

		if (yMag > xMag)
		{
			if (varYs[i] > 0)
				labelPos[i] <- 3
			else
				labelPos[i] <- 1
		}
		else
		{
			if (varXs[i] > 0)
				labelPos[i] <- 4
			else
				labelPos[i] <- 2
		}
	}

	varCol <- "gray45"
	par(new=TRUE)
	plot(varXs, varYs, type='n', axes=FALSE, xlab=NA, ylab=NA, xlim=xLimits, ylim=yLimits)
	arrows(0, 0, varXs, varYs, col=varCol)
	axis(3, col.ticks=varCol, col.axis=varCol)
	axis(4, col.ticks=varCol, col.axis=varCol)
	text(varXs, varYs, var, pos=labelPos, col=varCol)
}

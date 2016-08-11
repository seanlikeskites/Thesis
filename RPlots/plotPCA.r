source("agreement.r")
source("descriptorPositions.r")
source("featureNames.r")

plotIndividualPCA <- function(points, dims, legendPos, border=c(0, 0, 0, 0))
{
	descriptors <- rownames(points)
	uniqueDescriptors <- sort(unique(descriptors))
	colourPalette <- rainbow(length(uniqueDescriptors))

	colours <- "black"
	pchs <- array(1, nrow(points))

	for (i in 1:length(uniqueDescriptors))
	{
		colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
		pchs[descriptors == uniqueDescriptors[i]] <- i + 1
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
	points(xs, ys, pch=pchs, col=colours, cex=1.5)
	legend(legendPos, legend=uniqueDescriptors, pch=2:(length(uniqueDescriptors) + 1), pt.cex=1, col=colourPalette)

	box()
}

plotCentroidBiplot <- function(PCA, dims, desc, var, border=c(0.15, 0.15, 0.15, 0.15))
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

	xs <- centroids[, dims[1]]
	ys <- centroids[, dims[2]]

	xLabel <- paste("PC ", dims[1], sep="")
	yLabel <- paste("PC ", dims[2], sep="")

	xRange <- diff(range(xs))
	xLimits <- c(min(xs) - border[1] * xRange, max(xs) + border[2] * xRange)

	yRange <- diff(range(ys))
	yLimits <- c(min(ys) - border[3] * yRange, max(ys) + border[4] * yRange)

	n <- nrow(PCA$x)
	scaleX <- PCA$sdev[dims[1]] * sqrt(n)
	varXs <- PCA$rotation[var, dims[1]] * scaleX

	scaleY <- PCA$sdev[dims[2]] * sqrt(n)
	varYs <- PCA$rotation[var, dims[2]] * scaleY

	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel, xlim=xLimits, ylim=yLimits)
	text(xs, ys, descriptors, col=colours, cex=1+agreements)

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
	plotVar <- plotFeatureNames(var)
	text(varXs, varYs, plotVar, pos=labelPos, col=varCol, family="CM Roman")

	box()
}

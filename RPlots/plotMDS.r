plotMDS <- function(points, legendPos="topleft", plotCentroids=FALSE)
{
	if (plotCentroids)
	{
		points <- apply(points, 2, function(x) tapply(x, rownames(points), mean))
	}

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

	xLabel <- "First Dimension"
	yLabel <- "Second Dimension"

	xRange <- diff(range(xs))
	xLimits <- c(min(xs) - 0.15 * xRange, max(xs) + 0.15 * xRange)

	yRange <- diff(range(ys))
	yLimits <- c(min(ys) - 0.15 * yRange, max(ys) + 0.15 * yRange)

	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel, xlim=xLimits, ylim=yLimits)

	if (plotCentroids)
	{
		text(xs, ys, descriptors, col=colours)
	}
	else
	{
		points(xs, ys, pch=4, col=colours)
		legend(legendPos, legend=uniqueDescriptors, pch=4, col=colourPalette)
	}
}

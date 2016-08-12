rms <- function(data)
{
	return(sqrt(mean(data^2)))
}

plotSpectrum <- function(data, legendPos, legendncol, diff, yBorder=c(0, 0))
{
	barkBands <- paste("Bark Coefficient ", as.character(0:24), sep="")
	spectra <- data[,barkBands]
	rmss <- apply(spectra, 2, function(x) tapply(x, rownames(data), rms))
	meanSpectra <- 20*log10(rmss)

	yRange <- diff(range(meanSpectra))
	yLimits <- c(min(meanSpectra) - yBorder[1]*yRange, max(meanSpectra) + yBorder[2]*yRange)

	if (diff)
	{
		yLabel <- "Mean Gain (dB)"
	}
	else
	{
		yLabel <- "Mean Amplitude (dB)"
	}

	par(xaxs='i', mar=c(4, 4, 0.6, 0.6))
	plot(0:24, rmss[1,], type='n', xlab="Bark Band", ylab=yLabel, ylim=yLimits)

	nTerms <- nrow(meanSpectra)
	colours <- rainbow(nTerms)
	ltys <- rep(c(1, 2, 4, 5, 6), 1, nTerms)

	for (i in 1:nTerms)
	{
		lines(0:24, meanSpectra[i,], col=colours[i], lty=ltys[i])
	}

	legend(legendPos, legend=rownames(meanSpectra), ncol=legendncol, lty=ltys, col=colours)
}

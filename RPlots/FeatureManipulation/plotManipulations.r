library(foreign)
library(extrafont)

signals <- c("Cello", "Clarinet", "Synthesised", "Piano")

plotManipulation <- function(x, y, lims, legendText, legendPos, xlab, ylab)
{
	nLines <- nrow(y)
	colours <- rainbow(nLines)
	par(xaxs='i', yaxs='i', mar=c(4, 4.5, 0.5, 0.5))
	plot(x, y[1,], type='n', main="", xlab=xlab, ylab=ylab, xlim=c(lims[1], lims[2]), ylim=c(lims[3], lims[4]))

	for (i in 1:nLines)
	{
		lines(x, y[i,], col=colours[i])
	}

	legend(legendPos, legend=legendText, lty=1, col=colours)
	box()
}

########################################################
# centroids
########################################################
gains = seq(0, 1, 0.01)
centroids <- read.octave("MoveCentroids.mat")$centroids[c(1, 2, 4, 3),] / 1000
pdf("MoveCentroids.pdf", pointsize=8, family="CM Sans", width=4.2, height=3)
plotManipulation(gains, centroids, c(0, 1, 0, 6), signals, "topleft", "Parameter Setting", "Spectral Centroid (kHz)")
dev.off()
embed_fonts("MoveCentroids.pdf")

########################################################
# spreads
########################################################
gains = seq(0, 10, 0.1)
spreads <- read.octave("MoveSpreads.mat")$spreads[c(1, 2, 4, 3),] / 1000000
pdf("MoveSpreads.pdf", pointsize=8, family="CM Sans", width=4.2, height=3)
plotManipulation(gains, spreads, c(0, 10, 4, 16), signals, "topright", "Band Gain",
		 expression("Spectral Spread" ~ (kHz^2)))
dev.off()
embed_fonts("MoveSpreads.pdf")

########################################################
# skewnesses
########################################################
gains = seq(-10, 10, 0.5)
skews <- read.octave("MoveSkewnesses.mat")$skews[c(1, 2, 4, 3),]
pdf("MoveSkewnesses.pdf", pointsize=8, family="CM Sans", width=4.2, height=3)
plotManipulation(gains, skews, c(-10, 10, 0, 25), signals, "topright", "Tilt Gradient", "Spectral Skewness")
dev.off()
embed_fonts("MoveSkewnesses.pdf")

########################################################
# kurtoses
########################################################
gains = seq(0, 10, 0.1)
kurts <- read.octave("MoveKurtoses.mat")$kurts[c(1, 2, 4, 3),]
pdf("MoveKurtoses.pdf", pointsize=8, family="CM Sans", width=4.2, height=3)
plotManipulation(gains, kurts, c(0, 10, 6, 16), signals, "topleft", "Band Gain", "Spectral Kurtosis")
dev.off()
embed_fonts("MoveKurtoses.pdf")

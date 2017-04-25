library(foreign)
library(extrafont)

signals <- c("Cello", "Clarinet", "Synthesised", "Piano")
bigMar <- c(4, 4.3, 1, 1)

plotManipulation <- function(x, y, lims, legendText, legendPos, xlab, ylab, mar, ax1=NULL, ax2=NULL)
{
	nLines <- nrow(y)
	colours <- rainbow(nLines)
	par(xaxs='i', yaxs='i', mar=mar)
	plot(x, y[1,], type='n', main="", xlab=xlab, ylab=ylab,
	     xlim=c(lims[1], lims[2]), ylim=c(lims[3], lims[4]), axes=FALSE)

	if (is.null(ax1))
		axis(1)
	else
		axis(1, at=ax1)

	if (is.null(ax2))
		axis(2)
	else
		axis(2, at=ax2)

	for (i in 1:nLines)
	{
		lines(x, y[i,], col=colours[i])
	}

	legend(legendPos, legend=legendText, lty=1, col=colours, cex=0.95)
	box()
}

########################################################
# centroids
########################################################
gains <- seq(0, 1, 0.01)
centroids <- read.octave("MoveCentroids.mat")$centroids[c(1, 2, 4, 3),] / 1000
pdf("MoveCentroids.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, centroids, c(0, 1, 0, 6), signals, "topleft", "", "Spectral Centroid (kHz)",
		 bigMar)
mtext(expression(italic(P)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveCentroids.pdf")

########################################################
# spreads
########################################################
gains <- seq(0, 10, 0.1)
spreads <- read.octave("MoveSpreads.mat")$spreads[c(1, 2, 4, 3),] / 1000000
pdf("MoveSpreads.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, spreads, c(0, 10, 4, 16), signals, "topright", "",
		 expression("Spectral Spread" ~ (kHz^2)), bigMar)
mtext(expression(italic(m)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveSpreads.pdf")

########################################################
# skewnesses
########################################################
gains <- seq(-10, 10, 0.5)
skews <- read.octave("MoveSkewnesses.mat")$skews[c(1, 2, 4, 3),]
pdf("MoveSkewnesses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, skews, c(-10, 10, 0, 25), signals, "topright", "", "Spectral Skewness", bigMar)
mtext(expression(italic(k)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveSkewnesses.pdf")

########################################################
# kurtoses
########################################################
gains <- seq(0, 10, 0.1)
kurts <- read.octave("MoveKurtoses.mat")$kurts[c(1, 2, 4, 3),]
pdf("MoveKurtoses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, kurts, c(0, 10, 6, 17), signals, "topleft", "", "Spectral Kurtosis", bigMar)
mtext(expression(italic(m)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveKurtoses.pdf")

########################################################
# irregularities
########################################################
littleMar <- c(4, 4, 0.6, 0.6)
m <- seq(-1, 1, 0.01)
irregs <- read.octave("MoveIrregularities.mat")$irregs
irregKs <- t(irregs[1,,c(1, 2, 4, 3)])
irregJs <- t(irregs[2,,c(1, 2, 4, 3)])
irregBs <- t(irregs[3,,c(1, 2, 4, 3)])

pdf("MoveIrregularitiesK.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
plotManipulation(m, irregKs, c(-1, 1, 0, 0.4), signals, "topright", "",
		 "Krimphoff Irregularity", littleMar)
mtext(expression(italic(P)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveIrregularitiesK.pdf")

pdf("MoveIrregularitiesJ.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
plotManipulation(m, irregJs, c(-1, 1, 0, 1), signals, "bottomleft", "",
		 "Jensen Irregularity", littleMar)
mtext(expression(italic(P)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveIrregularitiesJ.pdf")

pdf("MoveIrregularitiesB.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
plotManipulation(m, irregBs, c(-1, 1, 0, 0.4), signals, "topright", "",
		 "Beauchamp Irregularity", littleMar)
mtext(expression(italic(P)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveIrregularitiesB.pdf")

########################################################
# flatnesses
########################################################
args <- seq(0, 1.5, 0.01)
flatnesses <- read.octave("MoveFlatnesses.mat")$flatnesses[c(1, 2, 4, 3),]
pdf("MoveFlatnesses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(args, flatnesses, c(0, 1.5, 0, 0.1), signals, "topright", "", "Spectral Flatness",
		 bigMar)
mtext(expression(italic(P)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveFlatnesses.pdf")

########################################################
# slopes
########################################################
gains <- seq(-10, 10, 0.5)
slopes <- read.octave("MoveSlopes.mat")$slopes[c(1, 2, 4, 3),]
pdf("MoveSlopes.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, slopes, c(-10, 10, -12, 0), signals, "bottomright", 
		 "", "Spectral Slope (dB per octave)", bigMar)
mtext(expression(italic(k)), side=1, line=3, family="CM Roman")
dev.off()
embed_fonts("MoveSlopes.pdf")

########################################################
# tristimulus 1
########################################################
gains <- seq(0, 5, 0.1)
tri1s <- read.octave("MoveTristimulus1.mat")$tri1s[c(1, 2, 4, 3),]
pdf("MoveTristimulus1.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, tri1s, c(0, 5, 0, 0.9), signals, "topleft", "", 
		 "First Tristimulus", bigMar)
mtext(expression(italic(f)[0]), side=1, line=3, at=2.3, family="CM Roman")
mtext("Gain", side=1, line=2.85, at=2.65)
dev.off()
embed_fonts("MoveTristimulus1.pdf")

########################################################
# tristimulus 2
########################################################
gains <- seq(0, 5, 0.1)
tri2s <- read.octave("MoveTristimulus2.mat")$tri2s[c(1, 2, 4, 3),]
pdf("MoveTristimulus2.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, tri2s, c(0, 5, 0, 0.8), signals, "topleft", "Band Gain", 
		 "Second Tristimulus", bigMar)
dev.off()
embed_fonts("MoveTristimulus2.pdf")

########################################################
# tristimulus 3
########################################################
gains <- seq(0, 5, 0.1)
tri3s <- read.octave("MoveTristimulus3.mat")$tri3s[c(1, 2, 4, 3),]
pdf("MoveTristimulus3.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, tri3s, c(0, 5, 0, 0.9), signals, "topleft", "Band Gain", 
		 "Third Tristimulus", bigMar)
dev.off()
embed_fonts("MoveTristimulus3.pdf")

########################################################
# parity ratios
########################################################
gains <- seq(0, 1, 0.01)
parities <- read.octave("MoveParities.mat")$parities[c(1, 2, 4, 3),]
pdf("MoveParities.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(gains, parities, c(0, 1, 0, 50), signals, "topleft", "Parameter Setting", 
		 "Odd to Even Harmonic Ratio", bigMar)
dev.off()
embed_fonts("MoveParities.pdf")

########################################################
# inharmonicities
########################################################
gains <- seq(0, 0.001, 0.00005)
inharms <- read.octave("MoveInharmonicities.mat")$inharms[c(1, 2, 4, 3),]
pdf("MoveInharmonicities.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotManipulation(1000*gains, inharms, c(0, 1, 0, 0.08), signals, "topleft", "Inharmonicity Factor", 
		 "Inharmonicity", bigMar)
mtext(expression(x10^-3), side=1, line=2, at=1)
dev.off()
embed_fonts("MoveInharmonicities.pdf")

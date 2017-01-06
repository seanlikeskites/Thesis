library(foreign)
library(extrafont)

data <- read.octave("HarmonicRelationships.mat")
bassAmps <- data$bassAmpRatios[3:9,20:110]
clarAmps <- data$clarAmpRatios[3:9,1:230]
pianAmps <- data$pianAmpRatios[3:9,1:250]
synAmps <- data$synAmpRatios[3:9,1:200]

#######################
# lines
#######################
plotHarmonicLevels <- function(data, ylim=c(0, 1), legendpos)
{
	nHarms <- nrow(data)
	time <- 0:(ncol(data) - 1)

	par(xaxs='i', yaxs='i')
	plot(time, data[1,], type='n', ylim=ylim,
	     xlab="Time (s)", ylab="Relative Amplitude")

	colours <- rainbow(nHarms)
	ltys <- rep(c(1, 2, 4, 5, 6), 1, nHarms)

	for (i in 1:nHarms)
	{
		lines(time, data[i,], col=colours[i], lty=ltys[i])
	}

	#par(family="CM Roman")
	#legendText <- paste("italic(h)[", orders, "]", sep="")
	#legend(legendpos, legend=parse(text=legendText), col=colours, lty=ltys)

	box()
}

pdf("CelloHarmonicAmplitudes.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0.2))
plotHarmonicLevels(bassAmps)
dev.off()
embed_fonts("CelloHarmonicAmplitudes.pdf")

pdf("ClarinetHarmonicAmplitudes.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0.2))
plotHarmonicLevels(clarAmps, c(0, 2))
dev.off()
embed_fonts("ClarinetHarmonicAmplitudes.pdf")

pdf("PianoHarmonicAmplitudes.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0.2))
plotHarmonicLevels(pianAmps, c(0, 10))
dev.off()
embed_fonts("PianoHarmonicAmplitudes.pdf")

pdf("SynthHarmonicAmplitudes.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0.2))
plotHarmonicLevels(synAmps)
dev.off()
embed_fonts("SynthHarmonicAmplitudes.pdf")

#######################
# Boxplots
#######################
harmonicAmplitudeBoxPlot <- function(data, ylim=c(0, 2))
{
	nHarmonics <- ncol(data)

	boxWidth <- 0.8
	par(xaxs='i', yaxs='i')
	boxplot(data, frame.plot=FALSE, axes=FALSE, col="blue",
		medlty="blank", boxwex=boxWidth, boxlty="blank", ylim=ylim,
		ylab="Relative Amplitude", outline=FALSE)
	axis(1, at=1:nHarmonics, line=0, lwd=0, labels=(1:nHarmonics)+2)
	mtext("Harmonic", 1, 3)
	axis(2)

	# plot the medians
	for (i in 1:nHarmonics)
	{
		stats <- boxplot.stats(data[,i])$stats

		left <- i - boxWidth / 2
		right <- i + boxWidth / 2
		bottom <- stats[2]
		top <- stats[4]
		clip(left, right, bottom, top)
		abline(h=stats[3], col="red", lwd=1.5)

		lims <- par("usr")
		clip(lims[1], lims[2], lims[3], lims[4])
		rect(left, bottom, right, top, lwd=1)
	}
}

pdf("CelloHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0))
harmonicAmplitudeBoxPlot(t(bassAmps), c(0, 1))
dev.off()
embed_fonts("CelloHarmonicAmplitudeBoxs.pdf")

pdf("ClarinetHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0))
harmonicAmplitudeBoxPlot(t(clarAmps), c(0, 2))
dev.off()
embed_fonts("ClarinetHarmonicAmplitudeBoxs.pdf")

pdf("PianoHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0))
harmonicAmplitudeBoxPlot(t(pianAmps), c(0, 10))
dev.off()
embed_fonts("PianoHarmonicAmplitudeBoxs.pdf")

pdf("SynthHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.6, 0))
harmonicAmplitudeBoxPlot(t(synAmps), c(0, 1))
dev.off()
embed_fonts("SynthHarmonicAmplitudeBoxs.pdf")

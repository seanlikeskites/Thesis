library(foreign)
library(extrafont)

harmonicsToPlot <- 3:9

data <- read.octave("HarmonicRelationships.mat")
stepSize <- data$stepSize

bassAmps <- data$bassAmpRatios[harmonicsToPlot,]
bassAttackFrame <- 1:round(9512 / stepSize)
bassAttack <- bassAmps[,bassAttackFrame]
bassSustainFrame <- (tail(bassAttackFrame, 1) + 1):round(103633 / stepSize)
bassSustain <- bassAmps[,bassSustainFrame]
bassReleaseFrame <- (tail(bassSustainFrame, 1) + 1):dim(bassAmps)[2]
bassRelease <- bassAmps[,bassReleaseFrame]

clarAmps <- data$clarAmpRatios[harmonicsToPlot,]
clarAttackFrame <- 1:round(2431 / stepSize)
clarAttack <- clarAmps[,clarAttackFrame]
clarSustainFrame <- (tail(clarAttackFrame, 1) + 1):round(246727 / stepSize)
clarSustain <- clarAmps[,clarSustainFrame]
clarReleaseFrame <- (tail(clarSustainFrame, 1) + 1):dim(clarAmps)[2]
clarRelease <- clarAmps[,clarReleaseFrame]

pianAmps <- data$pianAmpRatios[harmonicsToPlot,]
pianAttackFrame <- 1:round(8728 / stepSize)
pianAttack <- pianAmps[,pianAttackFrame]
pianSustainFrame <- (tail(pianAttackFrame, 1) + 1):round(45594 / stepSize)
pianSustain <- pianAmps[,pianSustainFrame]
pianReleaseFrame <- (tail(pianSustainFrame, 1) + 1):dim(pianAmps)[2]
pianRelease <- pianAmps[,pianReleaseFrame]

synAmps <- data$synAmpRatios[harmonicsToPlot,]
synAttackFrame <- 1:round(127657 / stepSize)
synAttack <- synAmps[,synAttackFrame]
synSustainFrame <- (tail(synAttackFrame, 1) + 1):dim(synAmps)[2]
synSustain <- synAmps[,synSustainFrame]

#######################
# lines
#######################
plotHarmonicLevels <- function(data, stepSize, ylim=c(0, 1), legendpos="topleft", legendcol=3)
{
	nHarms <- nrow(data)
	orders <- (1:nHarms)+2
	time <- stepSize * 0:(ncol(data) - 1) / 44100

	par(xaxs='i', yaxs='i')
	plot(time, data[1,], type='n', ylim=ylim,
	     xlab="Time (s)", ylab="Amplitude Ratio")

	colours <- rainbow(nHarms)
	ltys <- rep(c(1, 2, 4, 5, 6), 1, nHarms)

	for (i in 1:nHarms)
	{
		lines(time, data[i,], col=colours[i], lty=ltys[i])
	}

	par(family="CM Roman")
	legendText <- paste("italic(h)[", orders, "]", sep="")
	legend(legendpos, legend=parse(text=legendText), col=colours, lty=ltys, ncol=legendcol)

	box()
}

attackRange <- c(0, 3.2)
sustainRange <- c(0, 4.4)
releaseRange <- c(0, 10)

# cello
pdf("CelloAttackAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(bassAttack, stepSize, attackRange, "topright", legendcol=4)
dev.off()
embed_fonts("CelloAttackAmplitudes.pdf")

pdf("CelloSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(bassSustain, stepSize, c(0, 1.2), legendcol=4)
dev.off()
embed_fonts("CelloSustainAmplitudes.pdf")

pdf("CelloReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(bassRelease, stepSize, c(0, 3), legendcol=4)
dev.off()
embed_fonts("CelloReleaseAmplitudes.pdf")

# clarinet
pdf("ClarinetSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(clarSustain, stepSize, c(0, 2.5), legendcol=4)
dev.off()
embed_fonts("ClarinetSustainAmplitudes.pdf")

pdf("ClarinetReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(clarRelease, stepSize, c(0, 1), legendcol=4)
dev.off()
embed_fonts("ClarinetReleaseAmplitudes.pdf")

# piano
pdf("PianoAttackAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(pianAttack, stepSize, attackRange, legendcol=4)
dev.off()
embed_fonts("PianoAttackAmplitudes.pdf")

pdf("PianoSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(pianSustain, stepSize, sustainRange, legendcol=4)
dev.off()
embed_fonts("PianoSustainAmplitudes.pdf")

pdf("PianoReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(pianRelease, stepSize, releaseRange, "topright", legendcol=4)
dev.off()
embed_fonts("PianoReleaseAmplitudes.pdf")

# synth
pdf("SynthAttackAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(synAttack, stepSize, c(0, 1.3), legendcol=4)
dev.off()
embed_fonts("SynthAttackAmplitudes.pdf")

pdf("SynthSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.2))
plotHarmonicLevels(synSustain, stepSize, c(0, 2.5), legendcol=4)
dev.off()
embed_fonts("SynthSustainAmplitudes.pdf")

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
		ylab="Amplitude Ratio", outline=FALSE)
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

#pdf("CelloHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
#par(mar=c(4.1, 4, 0.6, 0))
#harmonicAmplitudeBoxPlot(t(bassAmps), c(0, 1.1))
#dev.off()
#embed_fonts("CelloHarmonicAmplitudeBoxs.pdf")
#
#pdf("ClarinetHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
#par(mar=c(4.1, 4, 0.6, 0))
#harmonicAmplitudeBoxPlot(t(clarAmps), c(0, 2.3))
#dev.off()
#embed_fonts("ClarinetHarmonicAmplitudeBoxs.pdf")
#
#pdf("PianoHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
#par(mar=c(4.1, 4, 0.6, 0))
#harmonicAmplitudeBoxPlot(t(pianAmps), c(0, 8))
#dev.off()
#embed_fonts("PianoHarmonicAmplitudeBoxs.pdf")
#
#pdf("SynthHarmonicAmplitudeBoxs.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
#par(mar=c(4.1, 4, 0.6, 0))
#harmonicAmplitudeBoxPlot(t(synAmps), c(0, 1.8))
#dev.off()
#embed_fonts("SynthHarmonicAmplitudeBoxs.pdf")

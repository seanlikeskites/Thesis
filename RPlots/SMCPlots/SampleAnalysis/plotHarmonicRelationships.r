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

bassSTFT50Amps <- data$bassSTFT50AmpRatios[harmonicsToPlot,]

clarAmps <- data$clarAmpRatios[harmonicsToPlot,]
clarAttackFrame <- 1:round(2431 / stepSize)
clarAttack <- clarAmps[,clarAttackFrame]
clarSustainFrame <- (tail(clarAttackFrame, 1) + 1):round(246727 / stepSize)
clarSustain <- clarAmps[,clarSustainFrame]
clarReleaseFrame <- (tail(clarSustainFrame, 1) + 1):dim(clarAmps)[2]
clarRelease <- clarAmps[,clarReleaseFrame]

clarSTFT50Amps <- data$clarSTFT50AmpRatios[harmonicsToPlot,]
clarSTFT50Sustain <- clarSTFT50Amps[,clarSustainFrame]
clarSTFT100Amps <- data$clarSTFT100AmpRatios[harmonicsToPlot,]
clarSTFT100Sustain <- clarSTFT100Amps[,clarSustainFrame]
clarSTFT500Amps <- data$clarSTFT500AmpRatios[harmonicsToPlot,]
clarSTFT500Sustain <- clarSTFT500Amps[,clarSustainFrame]

pianAmps <- data$pianAmpRatios[harmonicsToPlot,]
pianAttackFrame <- 1:round(8728 / stepSize)
pianAttack <- pianAmps[,pianAttackFrame]
pianSustainFrame <- (tail(pianAttackFrame, 1) + 1):round(45594 / stepSize)
pianSustain <- pianAmps[,pianSustainFrame]
pianReleaseFrame <- (tail(pianSustainFrame, 1) + 1):dim(pianAmps)[2]
pianRelease <- pianAmps[,pianReleaseFrame]

pianSTFT50Amps <- data$pianSTFT50AmpRatios[harmonicsToPlot,]

synAmps <- data$synAmpRatios[harmonicsToPlot,]
synAttackFrame <- 1:round(127657 / stepSize)
synAttack <- synAmps[,synAttackFrame]
synSustainFrame <- (tail(synAttackFrame, 1) + 1):dim(synAmps)[2]
synSustain <- synAmps[,synSustainFrame]

synSTFT50Amps <- data$synSTFT50AmpRatios[harmonicsToPlot,]

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
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(bassAttack, stepSize, attackRange, "topright", legendcol=4)
dev.off()
embed_fonts("CelloAttackAmplitudes.pdf")

pdf("CelloSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(bassSustain, stepSize, c(0, 1.2), legendcol=4)
dev.off()
embed_fonts("CelloSustainAmplitudes.pdf")

pdf("CelloReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(bassRelease, stepSize, c(0, 3), legendcol=4)
dev.off()
embed_fonts("CelloReleaseAmplitudes.pdf")

# clarinet
pdf("ClarinetSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(clarSustain, stepSize, c(0, 2.5), legendcol=4)
dev.off()
embed_fonts("ClarinetSustainAmplitudes.pdf")

pdf("ClarinetSTFT50SustainAmplitudes.pdf", pointsize=8, 
    fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(clarSTFT50Sustain, stepSize, c(0, 1), legendcol=4)
dev.off()
embed_fonts("ClarinetSTFT50SustainAmplitudes.pdf")

pdf("ClarinetSTFT100SustainAmplitudes.pdf", pointsize=8, 
    fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(clarSTFT100Sustain, stepSize, c(0, 1), legendcol=4)
dev.off()
embed_fonts("ClarinetSTFT100SustainAmplitudes.pdf")

pdf("ClarinetSTFT500SustainAmplitudes.pdf", pointsize=8, 
    fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(clarSTFT500Sustain, stepSize, c(0, 1), legendcol=4)
dev.off()
embed_fonts("ClarinetSTFT500SustainAmplitudes.pdf")

pdf("ClarinetReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(clarRelease, stepSize, c(0, 1), legendcol=4)
dev.off()
embed_fonts("ClarinetReleaseAmplitudes.pdf")

# piano
pdf("PianoAttackAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(pianAttack, stepSize, attackRange, legendcol=4)
dev.off()
embed_fonts("PianoAttackAmplitudes.pdf")

pdf("PianoSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(pianSustain, stepSize, sustainRange, legendcol=4)
dev.off()
embed_fonts("PianoSustainAmplitudes.pdf")

pdf("PianoReleaseAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(pianRelease, stepSize, releaseRange, "topright", legendcol=4)
dev.off()
embed_fonts("PianoReleaseAmplitudes.pdf")

# synth
pdf("SynthAttackAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
plotHarmonicLevels(synAttack, stepSize, c(0, 1.3), legendcol=4)
dev.off()
embed_fonts("SynthAttackAmplitudes.pdf")

pdf("SynthSustainAmplitudes.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(4.1, 4, 0.6, 0.5))
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

#######################
# Do the correlation
#######################
load("../maxMushraScores.RData")

matrixCorrelationTest <- function(data1, data2)
{
	if (nrow(data1) != length(data2))
	{
		stop("The number of rows in each dataset doesn't match")
	}

	numRows <- ncol(data1)
	numCols <- 1
	correlations <- matrix(nrow=numRows, ncol=numCols)
	rownames(correlations) <- colnames(data1)
	colnames(correlations) <- colnames(data2)
	pvalues <- matrix(nrow=numRows, ncol=numCols)
	rownames(pvalues) <- colnames(data1)
	colnames(pvalues) <- colnames(data2)

	for (i in 1:numRows)
	{
		for (j in 1:numCols)
		{
			cortest <- cor.test (data1[,i], data2)
			correlations[i,j] <- cortest$estimate
			pvalues[i,j] <- cortest$p.value
		}
	}

	output <- list()
	output$correlations <- correlations
	output$pValues <- pvalues

	return(output)
}

pop.var <- function(x)
{
	return(var(x) * (length(x) - 1) / length(x))
}

pop.cov <- function(x, y)
{
	return(cov(x, y) * (length(x) - 1) / length(x))
}


pop.sd <- function(x)
{
	return(sqrt(pop.var(x)))
}

harmonicVariation <- function(data)
{
	return(sum(apply(data, 1, pop.sd)))
}

harmonicCovariation <- function(input, output)
{
	nHarms <- nrow(input)
	out <- 0

	for (i in 1:nHarms)
	{
		out = out + pop.var(input[i,]) - abs(pop.cov(input[i,], output[i,]) - pop.var(input[i,]))
	}

	return(out)
}

harmVars <- c(harmonicVariation(bassAmps),
	      harmonicVariation(clarAmps),
	      harmonicVariation(synAmps),
	      harmonicVariation(pianAmps))

# make table of harmonic variabilities
samples <- c("Cello", "Clarinet", "Synthesised", "Piano")
lines <- character()
nSamples <- length(samples)

lines <- c(lines, "\\begin{tabular}{|c|c|}")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\bf{Signal} & $\\boldsymbol{\\mathrm{HV}}$ \\tabularnewline")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\hline")

for (i in 1:nSamples)
{
	variability <- format(round(harmVars[i], 2), nsmall=2)
	lines <- c(lines, paste("\t", samples[i], " & ", variability, " \\tabularnewline", sep=""))
	lines <- c(lines, "\t\\hline")
}

lines <- c(lines, "\\end{tabular}")

f <- file("HarmonicVariabilities.tex")
writeLines(lines, f)
close(f)

# correlations
correlations <- matrixCorrelationTest(maxScores, harmVars)

lines <- character()
nStimuli <- ncol(maxScores)

for (j in 0:2)
{
	lines <- c(lines, "\\begin{tabular}{|c|c|c|}")
	lines <- c(lines, "\t\\hline")
	lines <- c(lines, "\t\\bf{Stimulus} & $\\boldsymbol{r}$ & $\\boldsymbol{p}$ \\tabularnewline")
	lines <- c(lines, "\t\\hline")
	lines <- c(lines, "\t\\hline")

	for (i in 1:3)
	{
		idx <- 3 * j + i
		correlation <- format(round(correlations$correlations[idx], 3), nsmall=3)
		pValue <- format(round(correlations$pValues[idx], 3), nsmall=3)
		lines <- c(lines, paste("\t", idx + 1, " & ", correlation, " & ", pValue, " \\tabularnewline", sep=""))
		lines <- c(lines, "\t\\hline")
	}

	lines <- c(lines, "\\end{tabular}")

	if (j < 2)
	{
		lines <- c(lines, "\\qquad")
	}
}

f <- file("HarmonicVariabilityCorrelations.tex")
writeLines(lines, f)
close(f)

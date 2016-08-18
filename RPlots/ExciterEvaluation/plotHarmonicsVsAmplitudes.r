library(extrafont)
library(foreign)

plotHarmonicsVsAmplitude <- function(data, orders, legendpos)
{
	nHarms <- length(orders)
	data <- data[orders,]
	amps <- seq(0.005, 1, 0.005)

	par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
	plot(amps, data[1,], xlim=c(0, 1), ylim=c(-120, 0), type='n', 
	     xlab="Peak Input Amplitude", ylab="Distortion Level (dB)")

	colours <- rainbow(nHarms)
	ltys <- rep(c(1, 2, 4, 5, 6), 1, nHarms)

	for (i in 1:nHarms)
	{
		lines(amps, data[i,], col=colours[i], lty=ltys[i])
	}

	par(family="CM Roman")
	legendText <- paste("italic(h)[", orders, "]", sep="")
	legend(legendpos, legend=parse(text=legendText), col=colours, lty=ltys)

	box()
}

# load the data
data <- read.octave("HarmonicLevels.mat")

pdf("HardClippingHarmonics.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotHarmonicsVsAmplitude(data$hardClipHarms, seq(3, 13, 2), "topleft")
dev.off()
embed_fonts("HardClippingHarmonics.pdf")

pdf("SoftClippingHarmonics.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotHarmonicsVsAmplitude(data$softClipHarms, seq(3, 13, 2), "topleft")
dev.off()
embed_fonts("SoftClippingHarmonics.pdf")

pdf("ExponentialClippingHarmonics.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotHarmonicsVsAmplitude(data$expClipHarms, seq(3, 13, 2), "bottomright")
dev.off()
embed_fonts("ExponentialClippingHarmonics.pdf")

pdf("AsymmetricHardClippingHarmonics.pdf", pointsize=9,
    fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotHarmonicsVsAmplitude(data$asymmetricClipHarms, seq(2, 7), "topleft")
dev.off()
embed_fonts("AsymmetricHardClippingHarmonics.pdf")

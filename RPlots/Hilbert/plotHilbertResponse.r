library(extrafont)
library(foreign)

data <- read.octave("HilbertData.mat")

plotResponse <- function(f, responses, names, colours, ylab, ylim, legendpos, ax2)
{
	par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.7))
	plot(f, responses[[1]], type='n', axes=FALSE, xlab="Frequency (Hz)", ylab=ylab, ylim=ylim)

	nResponses <- length(responses)

	for (i in 1:nResponses)
	{
		lines(f, responses[[i]], col=colours[i])
	}

	par(family="CM Roman")
	axis(1, at=max(f), labels=expression(italic(f[s])), mgp=c(3, 0.7, 0))
	axis(1, at=min(f) + 0.1, labels=expression(italic(f[s])), mgp=c(3, 0.7, 0), tick=FALSE)
	par(family="CM Sans")
	axis(1, at=c(min(f), max(f)), labels=expression(-frac(, 2), frac(, 2)), mgp=c(3, 2, 0))
	axis(1, at=0, labels="0")

	if (ax2)
		axis(2)

	if (!is.null(legendpos))
		legend(legendpos, legend=names, col=colours, lty=1)

	box()
}

# fir amp
pdf("HilbertMagnitudeResponses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotResponse(data$freqs, list(20*log10(abs(data$lowOrder)), 20*log10(abs(data$highOrder))),
	     c("   = 11", "   = 101"), c("blue", "red"), "Amplitude (dB)", c(-60, 10), "bottomright", TRUE)
par(family="CM Roman")
text(2.3, c(-50.9, -55.5), expression(italic(M)))
dev.off()
embed_fonts("HilbertMagnitudeResponses.pdf")

# fir phase
pdf("HilbertPhaseResponses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotResponse(data$freqs, list(Arg(data$lowOrder), Arg(data$highOrder)),
	     c("   = 11", "   = 101"), c("blue", "red"), "Phase (radians)", c(-2, 2), "topright", FALSE)
par(family="CM Roman")
axis(2, at=pi/2, labels=expression(italic(pi)), mgp=c(3, 2.3, 0))
axis(2, at=-pi/2 + 0.105, labels=expression(italic(pi)), mgp=c(3, 2.3, 0), tick=FALSE)
par(family="CM Sans")
axis(2, at=c(-pi/2, pi/2), labels=expression(-frac(, 2), frac(, 2)))
axis(2, at=0, labels="0")
par(family="CM Roman")
text(2.3, c(1.48, 1.74), expression(italic(M)))
dev.off()
embed_fonts("HilbertPhaseResponses.pdf")

# iir phase
pdf("IIRHilbertPhaseResponses.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
plotResponse(data$freqs, list(data$iirResponse), "Fuck", c("blue"), "Phase (radians)", c(-2.5, 2.5), NULL, FALSE)
par(family="CM Roman")
axis(2, at=pi/2, labels=expression(italic(pi)), mgp=c(3, 2.3, 0))
axis(2, at=-pi/2 + 0.13, labels=expression(italic(pi)), mgp=c(3, 2.3, 0), tick=FALSE)
par(family="CM Sans")
axis(2, at=c(-pi/2, pi/2), labels=expression(-frac(, 2), frac(, 2)))
axis(2, at=0, labels="0")
dev.off()
embed_fonts("IIRHilbertPhaseResponses.pdf")

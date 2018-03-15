library(extrafont)
library(foreign)

data <- read.octave("ComplexSpectra.mat")
len <- length(data$sigSpec) / 2

freqs <- data$freqs[1:len] / 1000
sigSpec <- 20*log10(abs(data$sigSpec[1:len]))
cubeSpec <- 20*log10(abs(data$cubeSpec[1:len]))
twopfiveSpec <- 20*log10(abs(data$twopfiveSpec[1:len]))
ssbSpec <- 20*log10(abs(data$ssbSpec[1:len]))
iapSpec <- 20*log10(abs(data$iapSpec[1:len]))

plotSpectra <- function(f, spectra, names, colours, lwds)
{
	par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
	plot(f, spectra[[1]], type='n', xlab="Frequency (kHz)", ylab="Amplitude (dB)",
	     xlim=c(0, 25), ylim=c(-100, 0))

	nSpectra <- length(spectra)

	for (i in 1:nSpectra)
	{
		lines(f, spectra[[i]], col=colours[i], lwd=lwds[i])
	}

	#legend("topright", legend=names, col=colours, lty=1, lwd=lwds)

	box()
}

# original
pdf("FourHarmonics.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotSpectra(freqs, list(sigSpec), "Signal Cubed", "blue", 2)
dev.off()
embed_fonts("FourHarmonics.pdf")

# cube
pdf("CubedSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotSpectra(freqs, list(cubeSpec), "Signal Cubed", "green2", 2)
dev.off()
embed_fonts("CubedSpectra.pdf")

# 2.5
pdf("RaisedToTwoAndAHalfSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotSpectra(freqs, list(twopfiveSpec), "Signal Raised to the 2.5",
	    "green2", 2)
dev.off()
embed_fonts("RaisedToTwoAndAHalfSpectra.pdf")

# ssb
pdf("SSBA3Spectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotSpectra(freqs, list(ssbSpec), "Third Order SSBA",
	    "green2", 2)
dev.off()
embed_fonts("SSBA3Spectra.pdf")

# iap
pdf("IAP3Spectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotSpectra(freqs, list(iapSpec), "Third Order IAP",
	    "green2", 2)
dev.off()
embed_fonts("IAP3Spectra.pdf")

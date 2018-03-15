library(extrafont)
library(foreign)

data <- read.octave("BWESpectra.mat")
len <- length(data$sigSpec) / 2

sigSpec <- abs(data$sigSpec[1:len])
repSpec <- abs(data$repSpec[1:len])
fold2Spec <- abs(data$fold2Spec[1:len])
fold3Spec <- abs(data$fold3Spec[1:len])
stretchSpec <- abs(data$stretchSpec[1:len])

plotSpectra <- function(spectra, names, colours)
{
	par(xaxs='i', yaxs='i', mar=c(3, 2, 0.1, 0.6))
	f <- 1:length(spectra[[1]])
	plot(f, spectra[[1]], type='n', axes=FALSE, xlab="", ylab="")

	nSpectra <- length(spectra)

	for (i in 1:nSpectra)
	{
		lines(f, spectra[[i]], col=colours[i])
	}

	mtext(c("Frequency", "Amplitude"), side=c(1, 2), line=1)

	legend("topright", legend=names, col=colours, lty=1)

	par(family="CM Roman")
	axis(1, at=max(f), labels=expression(italic(f[s])), mgp=c(3, 0.7, 0))
	par(family="CM Sans")
	axis(1, at=min(f), labels=0, mgp=c(3, 0.7, 0))
	axis(1, at=max(f), labels=expression(frac(, 2)), mgp=c(3, 2, 0))

	box()
}

# replication
pdf("SpectralReplicationSpectrum.pdf", pointsize=9, fonts=c("CMU Serif", "CMU Sans Serif"), family="CMU Sans Serif", 
    width=4.2, height=3-0.375)
plotSpectra(list(repSpec, sigSpec), c("Output", "Input"), c("red", "blue"))
dev.off()
embed_fonts("SpectralReplicationSpectrum.pdf")

# stretching
pdf("SpectralStretchingSpectrum.pdf", pointsize=9, fonts=c("CMU Serif", "CMU Sans Serif"), family="CMU Sans Serif", 
    width=4.2, height=3-0.375)
plotSpectra(list(stretchSpec, sigSpec), c("Output", "Input"), c("red", "blue"))
dev.off()
embed_fonts("SpectralStretchingSpectrum.pdf")

# folding
pdf("SpectralFoldingSpectrum.pdf", pointsize=9, fonts=c("CMU Serif", "CMU Sans Serif"), family="CMU Sans Serif", 
    width=4.2, height=3-0.375)
plotSpectra(list(sigSpec, fold2Spec, fold3Spec), c("Input", "   = 2", "   = 3"), c("blue", "red", "green2"))
par(family="CMU Serif")
text(20050, c(567, 525), expression(italic(k)))
dev.off()
embed_fonts("SpectralFoldingSpectrum.pdf")

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
	par(xaxs='i', yaxs='i', mar=c(2, 2, 0.1, 0.1))
	f <- 1:length(spectra[[1]])
	plot(f, spectra[[1]], type='n', axes=FALSE)

	nSpectra <- length(spectra)

	for (i in 1:nSpectra)
	{
		lines(f, spectra[[i]], col=colours[i])
	}

	mtext(c("Frequency", "Amplitude"), side=c(1, 2), line=1)

	legend("topright", legend=names, col=colours, lty=1)

	box()
}

# replication
pdf("SpectralReplicationSpectrum.pdf", pointsize=9, family="CM Sans", width=4.2-0.375, height=3-0.375)
plotSpectra(list(repSpec, sigSpec), c("Output", "Input"), c("red", "blue"))
dev.off()
embed_fonts("SpectralReplicationSpectrum.pdf")

# stretching
pdf("SpectralStretchingSpectrum.pdf", pointsize=9, family="CM Sans", width=4.2-0.375, height=3-0.375)
plotSpectra(list(stretchSpec, sigSpec), c("Output", "Input"), c("red", "blue"))
dev.off()
embed_fonts("SpectralStretchingSpectrum.pdf")

# folding
pdf("SpectralFoldingSpectrum.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", 
    width=4.2-0.375, height=3-0.375)
plotSpectra(list(sigSpec, fold2Spec, fold3Spec), c("Input", "   = 2", "   = 3"), c("blue", "red", "green2"))
par(family="CM Roman")
text(20200, c(578, 542), expression(italic(k)))
dev.off()
embed_fonts("SpectralFoldingSpectrum.pdf")

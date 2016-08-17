library(R.matlab)
library(signal)
library(extrafont)
jet.colours <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
signals <- readMat("samples.mat")

minClip <- function(data, thresh)
{
	data[abs(data) < thresh] <- thresh
	return(data)
}

plotSpectrogram <- function(sig)
{
	spec <- specgram(sig, n=1024, window=512, overlap=256, Fs=44100)
	spec$S <- minClip(spec$S, 10^(-70/20))
	plot(spec, col=jet.colours(512), ylim=c(0, 7000), xlab="Time (s)", ylab="Frequency (kHz)", axes=FALSE)
	axis(1)
	axis(2, at=seq(0, 7000, 1000), labels=as.character(0:7))
	box()
}

# original signal
bass <- as.vector(signals$bass)
pdf("CelloSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(bass)
dev.off()
embed_fonts("CelloSpectrogram.pdf")

# filtered signal
bassFilt <- as.vector(signals$bassFilt)
pdf("CelloFilteredSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(bassFilt)
dev.off()
embed_fonts("CelloFilteredSpectrogram.pdf")

# SSB signal
bassSSB <- as.vector(signals$bassSSB)
pdf("CelloSSBASpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(bassSSB)
dev.off()
embed_fonts("CelloSSBASpectrogram.pdf")

# IAP signal
bassIAP <- as.vector(signals$bassIAP)
pdf("CelloIAPSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(bassIAP)
dev.off()
embed_fonts("CelloIAPSpectrogram.pdf")

# STFT signal
bassSTFT <- as.vector(signals$bassSTFT)
pdf("CelloSynthesisSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(bassSTFT)
dev.off()
embed_fonts("CelloSynthesisSpectrogram.pdf")

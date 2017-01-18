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

plotSpectrogram <- function(sig, maxFreq=7000, n=1024, window=512, overlap=256)
{
	spec <- specgram(sig, n=n, window=window, overlap=overlap, Fs=44100)
	spec$S <- minClip(spec$S, 10^(-70/20))
	plot(spec, col=jet.colours(window), ylim=c(0, maxFreq), xlab="Time (s)", ylab="Frequency (kHz)", axes=FALSE)
	axis(1)
	axis(2, at=seq(0, maxFreq, 1000), labels=as.character(0:(maxFreq / 1000)))
	box()
}

# bass
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

# clarinet
# original signal
clar <- as.vector(signals$clar)
pdf("ClarinetSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(clar)
dev.off()
embed_fonts("ClarinetSpectrogram.pdf")

# filtered signal
clarIAP <- as.vector(signals$clarIAP)
pdf("ClarinetIAPSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(clarIAP)
dev.off()
embed_fonts("ClarinetIAPSpectrogram.pdf")

# piano
# original signal
pian <- as.vector(signals$pian)
pdf("PianoSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(pian, 4000)
dev.off()
embed_fonts("PianoSpectrogram.pdf")

# filtered signal
pianIAP <- as.vector(signals$pianIAP)
pdf("PianoIAPSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(pianIAP, 4000)
dev.off()
embed_fonts("PianoIAPSpectrogram.pdf")

# synth
# original signal
syn <- as.vector(signals$syn)
pdf("SynthSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(syn, 2000, 2048, 1024, 512)
dev.off()
embed_fonts("SynthSpectrogram.pdf")

# filtered signal
synIAP <- as.vector(signals$synIAP)
pdf("SynthIAPSpectrogram.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(4, 4, 0.5, 0.5))
plotSpectrogram(synIAP, 2000, 2048, 1024, 512)
dev.off()
embed_fonts("SynthIAPSpectrogram.pdf")

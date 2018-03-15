library(extrafont)
library(foreign)

data <- read.octave("STTRData.mat")

freqs <- data$freqs/1000
sigSpec <- 20*log10(abs(data$sigSpec))
ms1Spec <- 20*log10(abs(data$ms1Spec))
ms1p5Spec <- 20*log10(abs(data$ms1p5Spec))

# window plot
winLen = length(data$saveWindow)
pdf("STTRWindow.pdf", pointsize=9, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=4.2, height=3)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
plot(1:winLen, data$saveWindow, type='l', col="blue", ylim=c(0, 1), xlab="Time (samples)", ylab="", axes=FALSE)
par(family="CM Roman")
axis(1, at=winLen, labels=expression(italic(L)), mgp=c(3, 0.7, 0))
axis(1, at=4.3, labels=expression(italic(L)), mgp=c(3, 0.7, 0), tick=FALSE)
par(family="CM Sans")
axis(1, at=c(1, winLen), labels=expression(-frac(, 2), frac(, 2)), mgp=c(3, 2, 0))
axis(1, at=(winLen + 1)/2, labels="0")
axis(2)
box()
dev.off()
embed_fonts("STTRWindow.pdf")

# spectra
pdf("STTRSpectra1.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.2)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
plot(freqs, sigSpec, type='n', xlim=c(0, 25), ylim=c(-100, 0), xlab="Frequency (kHz)", ylab=("Amplitude (dB)"))
lines(freqs, ms1Spec, col="green2", lty=1, lwd=1)
box()
dev.off()
embed_fonts("STTRSpectra1.pdf")

pdf("STTRSpectra1p5.pdf", pointsize=8, fonts=c("CM Roman", "CM Sans"), family="CM Sans", width=2.94, height=2.2)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
plot(freqs, sigSpec, type='n', xlim=c(0, 25), ylim=c(-100, 0), xlab="Frequency (kHz)", ylab=("Amplitude (dB)"))
lines(freqs, ms1p5Spec, col="red", lty=1, lwd=1)
box()
dev.off()
embed_fonts("STTRSpectra1p5.pdf")

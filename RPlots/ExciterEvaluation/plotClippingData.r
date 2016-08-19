library(extrafont)
library(foreign)

data <- read.octave("ClippingData.mat")
len <- length(data$hardSpec) / 2

freqs <- data$freqs[1:len]
hardSpec <- 20*log10(abs(data$hardSpec[1:len]))
softSpec <- 20*log10(abs(data$softSpec[1:len]))

# clipping
pdf("Clipping.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
plot(data$line, data$hardOut, type='n', xlim=c(-1, 1), ylim=c(-1, 1), xlab="Input", ylab="Output")
lines(data$line, data$hardOut, col="blue", lty=1)
lines(data$line, data$softOut, col="red", lty=1)
legend("topright", legend=c("Hard Clipping", "Soft Clipping"), col=c("blue", "red"), lty=1)
box()
dev.off()
embed_fonts("Clipping.pdf")

# spectra
pdf("ClippingSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
logFreqs = log10(freqs)
plot(freqs, hardSpec, type='n', xlim=c(10, 20000), ylim=c(-150, 0), axes=FALSE,
     xlab="Frequency (Hz)", ylab="Amplitude (dB)", log='x')
axis(1, at=c(10, 100, 1000, 10000), labels=expression(10^1, 10^2, 10^3, 10^4))
axis(2, at=c(-150, -100, -50, 0))
lines(freqs, hardSpec, col="blue", lty=1)
lines(freqs, softSpec, col="red", lty=1)
legend("topright", legend=c("Hard Clipping", "Soft Clipping"), col=c("blue", "red"), lty=1)
box()
dev.off()
embed_fonts("ClippingSpectra.pdf")

# clipping approximation
pdf("ClippingApproximation.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(xaxs='i', yaxs='i', mar=c(4, 4, 0.6, 0.6))
plot(data$line, data$hardOut, type='n', xlim=c(-1, 1), ylim=c(-1, 1), xlab="Input", ylab="Output")
lines(data$line, data$hardOut, col="blue", lty=1)
lines(data$line, data$approxHard, col="red", lty=1)
legend("topright", legend=c("Hard Clipping", expression(7^th ~ "Order Approximation")), col=c("blue", "red"), lty=1)
box()
dev.off()
embed_fonts("ClippingApproximation.pdf")

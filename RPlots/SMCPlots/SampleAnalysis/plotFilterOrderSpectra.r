library(foreign)
library(extrafont)

data <- read.octave("FilterOrderSpectra.mat")

freqs <- data$freqs / 1000
iir <- 20*log10(data$iirSpec)
fir <- 20*log10(data$firSpec)

pdf("CelloFilterOrderSpectra.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(mar=c(4.1, 4, 0.8, 0.5), xaxs='i', yaxs='i')
plot(freqs, iir, xlim=c(0, 10), ylim=c(-120, -20), xlab="Frequency (kHz)", ylab="Amplitude (dB)")
lines(freqs, iir, col="green")
lines(freqs, fir, col="blue")
box()
legend("topright", legend=c(expression(2^nd ~ "Order IIR"), expression(2001^st ~ "Order FIR")), 
       col=c("green", "blue"), lty=1)
dev.off()
embed_fonts("CelloFilterOrderSpectra.pdf")

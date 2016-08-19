library(extrafont)
library(seewave)

# make the signal
fs <- 44100
ts <- 1/fs
t <- seq(ts, 7, ts)
sig <- sin(2*pi*t)

up <- (1:(2*fs)) / (2*fs)
sig[1:fs] <- 0
sig[(1:(2*fs)) + fs] <- sig[(1:(2*fs)) + fs] * up
sig[(1:(2*fs)) + 4*fs] <- sig[(1:(2*fs)) + 4*fs] * rev(up)
sig[(1:fs) + 6*fs] <- 0

env <- abs(hilbert(sig, f=fs))

# envelope
pdf("AmplitudeEnvelope.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(mar=c(0.1, 0, 0, 0))
plot(t, env, type='n', axes=FALSE, ylim=c(-1.4, 1))
lines(t, sig, col="blue")
lines(t, env, col="red")
legend("bottom", legend=c("Signal", "Amplitude Envelope"), col=c("blue", "red"), lty=1)
dev.off()
embed_fonts("AmplitudeEnvelope.pdf")

# ADSR
attack <- (0:200)/200
decay <- seq(200, 140, -1)/200
sustain <- 14*array(1, 600)/20
release <- seq(140, 0, -0.5)/200
adsr <- c(attack, decay, sustain, release)
len <- length(adsr)

pdf("ADSR.pdf", pointsize=9, family="CM Sans", width=4.2, height=3-0.375)
par(xaxs='i', yaxs='i', mar=c(2, 4, 0.1, 0.6))
plot(1:len, adsr, type='l', col="blue", lty=1, axes=FALSE, xlab="", ylab="")
lines(c(201, 201), c(0, 1), lty=3, col="black")
lines(c(261, 261), c(0, 14/20), lty=3, col="black")
lines(c(861, 861), c(0, 14/20), lty=3, col="black")
text(c(130, 230, 560, 940), 0.3, c("Attack", "Decay", "Sustain", "Release"), srt=90)
mtext(c("Time", "Amplitude"), side=c(1, 2), line=1)
box()
dev.off()
embed_fonts("ADSR.pdf")

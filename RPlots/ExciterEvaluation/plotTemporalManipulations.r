library(extrafont)
library(seewave)

# make the signal
fs <- 44100
ts <- 1/fs
t <- seq(ts, 7, ts)
sig <- sin(2*pi*t)

up = (1:(2*fs)) / (2*fs)
sig[1:fs] = 0
sig[(1:(2*fs)) + fs] = sig[(1:(2*fs)) + fs] * up
sig[(1:(2*fs)) + 4*fs] = sig[(1:(2*fs)) + 4*fs] * rev(up)
sig[(1:fs) + 6*fs] = 0

plotTemporalManipulation <- function(t, sigs, names, yspace)
{
	par(mar=c(0.1, 0, 0, 0))
	plot(t, sig, type='n', axes=FALSE, ylim=c(yspace, 1))

	colours <- c("blue", "green4", "red")

	nSigs <- length(sigs)

	for (i in 1:nSigs)
	{
		lines(t, sigs[[i]], col=colours[i])
	}

	legend("bottom", legend=names, col=colours[1:nSigs], lty=1)
}

# clipping
clipped <- sign(sig)
pdf("InfinitePeakClipping.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, clipped), c("Original Signal", "Clipped Signal"), -1.4)
dev.off()
embed_fonts("InfinitePeakClipping.pdf")

# exp
cubed <- sig^3
cubeRoot <- sign(sig)*abs(sig)^(1/3)
pdf("ExponentiationTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, cubed, cubeRoot), c("Original Signal", "Signal Cubed", "Signal Cube Root"), -1.5)
dev.off()
embed_fonts("ExponentiationTemporalEffects.pdf")

# ssb
hilb <- hilbert(sig, f=fs)
ssb <- Re(hilb^(3))
pdf("SSBATemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, ssb), c("Original Signal", "Third Order SSBA"), -1.4)
dev.off()
embed_fonts("SSBATemporalEffects.pdf")

# iap
iap <- abs(hilb)*cos(3*Arg(hilb));
pdf("IAPTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, iap), c("Original Signal", "Third Order IAP"), -1.4)
dev.off()
embed_fonts("IAPTemporalEffects.pdf")

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

env <- array(0, length(sig))
env[(1:(2*fs)) + fs] <- up
env[(3*fs):(4*fs)] <- 1
env[(1:(2*fs)) + 4*fs] <- rev(up)

plotTemporalManipulation <- function(t, sigs, envs, names, yspace)
{
	par(mar=c(0.1, 0, 0, 0))
	plot(t, sigs[[1]], type='n', axes=FALSE, ylim=c(yspace, 1))

	colours <- c("blue", "green4", "red")
	nSigs <- length(sigs)

	for (i in 1:nSigs)
	{
		if (is.null(envs))
		{
			lines(t, sigs[[i]], col=colours[i])
		}
		else
		{
			lines(t, sigs[[i]], col=adjustcolor(colours[i], alpha=0.4))
			lines(t, envs[[i]], col=colours[i], lty=3, lwd=2)
		}
	}

	legend("bottom", legend=names, col=colours[1:nSigs], lty=1)
}

# clipping
clipped <- sign(sig)
pdf("InfinitePeakClipping.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, clipped), list(env, sign(env)), c("Original Signal", "Clipped Signal"), -1.4)
dev.off()
embed_fonts("InfinitePeakClipping.pdf")

# exp
cubed <- sig^3
pdf("MultiplierTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, cubed), list(env, env^3),
			 c("Original Signal", "Cubed Signal"), -1.4)
dev.off()
embed_fonts("MultiplierTemporalEffects.pdf")

cubeRoot <- sign(sig)*abs(sig)^(1/3)
pdf("ExponentiationTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, cubeRoot), list(env, env^(1/3)),
			 c("Original Signal", "Cube Rooted Signal"), -1.4)
dev.off()
embed_fonts("ExponentiationTemporalEffects.pdf")

# ssb
hilb <- hilbert(sig, f=fs)
ssb <- Re(hilb^(3))
ssb <- ssb / max(abs(ssb))
pdf("SSBATemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, ssb), list(env, env^3),
			 c("Original Signal", "Third Order SSBA"), -1.4)
dev.off()
embed_fonts("SSBATemporalEffects.pdf")

# iap
iap <- abs(hilb)*cos(3*Arg(hilb))
iap <- iap / max(abs(iap))
pdf("IAPTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, iap), list(env, env), c("Original Signal", "Third Order IAP"), -1.4)
dev.off()
embed_fonts("IAPTemporalEffects.pdf")

# integrator
integrator <- function(sig, k)
{
	l <- length(sig)
	px <- 0
	py <- 0
	out <- array(0, l)

	for (i in 1:l)
	{
		if ((sig[i] > 0) && (px <= 0))
			out[i] = 0
		else
			out[i] = py + k*abs(sig[i])

		px = sig[i]
		py = out[i]
	}

	return(out)
}

int <- integrator(clipped, 4)
int <- int / max(abs(int))
pdf("IntegratorTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(clipped, int), NULL, c("Original Signal", "Integrated Signal"), -1.4)
dev.off()
embed_fonts("IntegratorTemporalEffects.pdf")

# spectral replication
spectralReplication <- function(hilb, f, fs)
{
	l = length(hilb)
	t = 0:(l-1)
	e = exp(2i*pi*f*t/fs)
	return(Re(hilb*e))
}

rep <- spectralReplication(hilb, 1.5, fs)
pdf("SpectralReplicationTemporalEffects.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
plotTemporalManipulation(t, list(sig, rep), list(env, env), c("Original Signal", "Spectral Replication"), -1.4)
dev.off()
embed_fonts("SpectralReplicationTemporalEffects.pdf")

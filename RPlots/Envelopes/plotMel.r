library(extrafont)

hz <- seq(0, 20000)
mel <- 2595 * log10(1 + hz / 700)

pdf("MelScale.pdf", pointsize=9, family="CM Sans", width=4.2, height=3)
par(mar=c(4, 4, 1, 0.5), xaxs='i', yaxs='i')
plot(hz / 1000, mel, type='l', ylim=c(0, 4000), xlab="Frequency (kHz)", ylab="Frequency (mel)", col="blue")
dev.off()
embed_fonts("MelScale.pdf")

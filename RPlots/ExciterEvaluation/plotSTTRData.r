library(extrafont)
library(foreign)

data <- read.octave("STTRData.mat")
len <- length(data$sigSpec) / 2

freqs <- data$freqs[1:len]
sigSpec <- 20*log10(abs(data$sigSpec[1:len]))
ms1Spec <- 20*log10(abs(data$ms1Spec[1:len]))
ms1p5Spec <- 20*log10(abs(data$ms1p5Spec[1:len]))

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

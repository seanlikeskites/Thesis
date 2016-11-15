library(R.matlab)
library(extrafont)

plotResults <- function(res, conf)
{
	par(lwd=0.5)
	centres <- barplot(res, ylab="Normalised Grade",
			   col="blue")
	axis(1, at=centres, line=-1, lwd=0, labels=as.character(1:11))
	mtext("Stimulus Number", 1, 2)

	# find which error bars we should draw
	arrowsToDraw <- conf != 0
	centres <- centres[arrowsToDraw]
	res <- res[arrowsToDraw]
	conf <- conf[arrowsToDraw]

	# clip so we don't get yuckiness
	usr <- par("usr")
	clip(usr[1], usr[2], 0.5, usr[4])

	par(lwd=1)
	arrows(centres, res - conf, centres, res + conf, angle=90, length=0.03, code=3, col="red")
}

# plot the test results
results <- readMat("MushraResults.mat")
confs <- results$res - results$conf[,,1]

pdf("CelloResults.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(results$res[1,], confs[1,])
dev.off()
embed_fonts("CelloResults.pdf")

pdf("SynthResults.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(results$res[2,], confs[2,])
dev.off()
embed_fonts("SynthResults.pdf")

pdf("PianoResults.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(results$res[3,], confs[3,])
dev.off()
embed_fonts("PianoResults.pdf")

pdf("ClarinetResults.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(results$res[4,], confs[4,])
dev.off()
embed_fonts("ClarinetResults.pdf")

# plot the objective metrics
rnonlin <- readMat("RNonlin_Results.mat")

pdf("CelloRNonlin.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[1,], array(0, 11))
dev.off()
embed_fonts("CelloRNonlin.pdf")

pdf("SynthRNonlin.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[2,], array(0, 11))
dev.off()
embed_fonts("SynthRNonlin.pdf")

pdf("PianoRNonlin.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[3,], array(0, 11))
dev.off()
embed_fonts("PianoRNonlin.pdf")

pdf("ClarinetRNonlin.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[4,], array(0, 11))
dev.off()
embed_fonts("ClarinetRNonlin.pdf")

library(R.matlab)
library(extrafont)

plotResults <- function(res, conf, nonlin=FALSE)
{
	if (!nonlin)
		ylab = "Normalised Grade"
	else
		ylab = ""

	centres <- barplot(res, ylab=ylab,
			   col="blue")
	axis(1, at=centres, line=-1, lwd=0, labels=as.character(1:11))
	mtext("Stimulus Number", 1, 2)

	if (nonlin)
	{
		mtext("Normalised", 2, 3, at=40)
		mtext(expression(italic(R)[nonlin]), 2, 2.85, at=67, family="CM Roman")
	}

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

pdf("CelloRNonlin.pdf", pointsize=8, fonts=c("CM Sans", "CM Roman"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[1,], array(0, 11), TRUE)
dev.off()
embed_fonts("CelloRNonlin.pdf")

pdf("SynthRNonlin.pdf", pointsize=8, fonts=c("CM Sans", "CM Roman"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[2,], array(0, 11), TRUE)
dev.off()
embed_fonts("SynthRNonlin.pdf")

pdf("PianoRNonlin.pdf", pointsize=8, fonts=c("CM Sans", "CM Roman"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[3,], array(0, 11), TRUE)
dev.off()
embed_fonts("PianoRNonlin.pdf")

pdf("ClarinetRNonlin.pdf", pointsize=8, fonts=c("CM Sans", "CM Roman"), family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.8, 0))
plotResults(rnonlin$results[4,], array(0, 11), TRUE)
dev.off()
embed_fonts("ClarinetRNonlin.pdf")

# find correlations
celloCor <- cor.test(rnonlin$results[1,], results$res[1,])
synthCor <- cor.test(rnonlin$results[2,], results$res[2,])
pianoCor <- cor.test(rnonlin$results[3,], results$res[3,])
clarinetCor <- cor.test(rnonlin$results[4,], results$res[4,])
samples <- c("Cello", "Clarinet", "Synthesised", "Piano")

correlations <- matrix(c(celloCor$estimate, celloCor$p.value,
		         clarinetCor$estimate, clarinetCor$p.value,
		         synthCor$estimate, synthCor$p.value,
		         pianoCor$estimate, pianoCor$p.value), 
		       ncol=2, byrow=TRUE, 
		       dimnames=list(samples, c("r", "p")))

lines <- character()
nSamples <- length(samples)

lines <- c(lines, "\\begin{tabular}{|c|c|c|}")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\bf{Signal} & $\\boldsymbol{r}$ & $\\boldsymbol{p}$ \\tabularnewline")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\hline")

for (i in 1:nSamples)
{
	correlation <- format(correlations[i, 1], digits=2)
	pValue <- format(round(correlations[i, 2], 3), nsmall=3)
	lines <- c(lines, paste("\t", samples[i], " & ", correlation, " & ", pValue, " \\tabularnewline", sep=""))
	lines <- c(lines, "\t\\hline")
}

lines <- c(lines, "\\end{tabular}")

f <- file("SMCCorrelations.tex")
writeLines(lines, f)
close(f)

# find maximum reconstruction scores
maxScores <- results$res[c(1, 4, 2, 3),2:10]
save(maxScores, file="maxMushraScores.RData")

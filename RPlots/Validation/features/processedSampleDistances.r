library(extrafont)
load("../../SAFEAnalysis/PCAData.RData")
load("ValidationMeans.RData")
source("../../SAFEAnalysis/descriptorPositions.r")

prettyInstrumentNames <- function(names)
{
	for (i in 1:length(names))
	{
		if (names[i] == "Bass1")
			names[i] <- "B1"
		else if (names[i] == "Bass2")
			names[i] <- "B2"
		else if (names[i] == "Flute")
			names[i] <- "F"
		else if (names[i] == "Guitar1")
			names[i] <- "G1"
		else if (names[i] == "Guitar2")
			names[i] <- "G2"
		else if (names[i] == "Marimba")
			names[i] <- "M"
		else if (names[i] == "Oboe")
			names[i] <- "O"
		else if (names[i] == "Saxophone")
			names[i] <- "S"
		else if (names[i] == "Trumpet")
			names[i] <- "T"
		else if (names[i] == "Violin")
			names[i] <- "V"
	}

	return(names)
}

getPCACoords <- function(means, pca)
{
	means <- scale(means, center=pca$center, scale=pca$scale)
	coords <- means %*% pca$rotation
	return(coords)
}

calculateDistance <- function(means, pca, descriptors)
{
	nDescriptors <- length(descriptors)
	coords <- getPCACoords(means, pca)
	dists <- matrix(0, nDescriptors, nrow(coords))
	colnames(dists) <- rownames(means)

	for (i in 1:nDescriptors)
	{
		cluster <- getDescriptorPositions(pca$x, descriptors[i])
		n <- nrow(cluster)

		if (n >= 6)
			n <- 6

		cluster <- cluster[,1:(n-1)]
		center <- apply(cluster, 2, mean)
		covariance <- cov(cluster)
		dists[i,] <- sqrt(mahalanobis(coords[,1:(n-1)], center, covariance))
	}

	return(apply(dists, 2, min))
}

makeDistanceTable <- function(data, outFile)
{
	lines <- character()
	nInstruments <- ncol(data)
	intruments <- colnames(data)
	instruments <- prettyInstrumentNames(instruments)
	terms <- rownames(data)

	lines <- c(lines, paste("\\begin{tabular}{|c||", paste(rep("c|", nInstruments), collapse=""), "}", sep=""))
	lines <- c(lines, paste("\t\\cline{2-", nInstruments + 1, "}", sep=""))
	lines <- c(lines, paste("\t\\multicolumn{1}{c|}{} & \\bf{", 
				paste(instruments, collapse="} & \\bf{"), 
				"} \\tabularnewline", sep=""))
	lines <- c(lines, paste("\t\\hhline{~|",
				paste(rep("-|", nInstruments), collapse=""),
				"}", sep=""))
	lines <- c(lines, "\t\\noalign{\\vspace{\\doublerulesep}}")
	lines <- c(lines, paste("\t\\hhline{-||",
				paste(rep("-|", nInstruments), collapse=""),
				"}", sep=""))

	for (term in terms)
	{
		dists <- format(round(data[term,], 1), nsmall=1)
		lines <- c(lines, paste("\t\\bf{", term, "} & ",
					paste(dists, collapse=" & "),
					" \\tabularnewline", sep=""))
		lines <- c(lines, paste("\t\\hhline{-||",
					paste(rep("-|", nInstruments), collapse=""),
					"}", sep=""))
	}

	lines <- c(lines, "\\end{tabular}")

	f <- file(outFile)
	writeLines(lines, f)
	close(f)
}

plotDistanceBarChart <- function(means, colours)
{
	names <- colnames(means)
	descriptors <- rownames(means)
	plotNames <- prettyInstrumentNames(names)

	lims <- c(0, 11)

	centres <- barplot(means, ylab="Mahalanobis Distance", xaxt="n", ylim=lims, beside=TRUE,
			   col=colours, legend=descriptors, args.legend=list(ncol=3))
	labelPoints <- centres[seq(2, 3 * ncol(means), 3)]
	axis(1, at=labelPoints, line=-1, lwd=0, labels=plotNames)
	mtext("Test Signal", 1, 2)
}

instruments <- c("Bass1", "Bass2", "Flute", "Guitar1", "Guitar2", "Marimba", "Oboe",  "Saxophone", "Trumpet", "Violin")
nInstruments <- length(instruments)

# harsh processed distances
harshProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("warm", "bright", "harsh"), instruments))
harshProcDists["warm",] <- calculateDistance(harshMeans$Warm$Processed, combProcPCA, c("E:warm", "D:warm"))
harshProcDists["bright",] <- calculateDistance(harshMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
harshProcDists["harsh",] <- calculateDistance(harshMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))

pdf("HarshProcessedJeffsDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(harshProcDists, c("blue", "turquoise", "green"))
dev.off()
embed_fonts("HarshProcessedJeffsDistance.pdf")

# harsh difference distances
harshDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("warm", "bright", "harsh"), instruments))
harshDiffDists["warm",] <- calculateDistance(harshMeans$Warm$Processed - harshMeans$Warm$Unprocessed, 
					     combDiffPCA, c("E:warm", "D:warm"))
harshDiffDists["bright",] <- calculateDistance(harshMeans$Bright$Processed - harshMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
harshDiffDists["harsh",] <- calculateDistance(harshMeans$Harsh$Processed - harshMeans$Harsh$Unprocessed, 
					      combDiffPCA, c("E:harsh", "D:harsh"))

pdf("HarshDifferenceJeffsDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(harshDiffDists, c("blue", "turquoise", "green"))
dev.off()
embed_fonts("HarshDifferenceJeffsDistance.pdf")

# crunch processed distances
crunchProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("harsh", "bright", "crunch"), instruments))
crunchProcDists["harsh",] <- calculateDistance(crunchMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))
crunchProcDists["bright",] <- calculateDistance(crunchMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
crunchProcDists["crunch",] <- calculateDistance(crunchMeans$Crunch$Processed, combProcPCA, "D:crunch")

pdf("CrunchProcessedJeffsDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(crunchProcDists, c("green", "turquoise", "purple"))
dev.off()
embed_fonts("CrunchProcessedJeffsDistance.pdf")

# crunch difference distances
crunchDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("harsh", "bright", "crunch"), instruments))
crunchDiffDists["harsh",] <- calculateDistance(crunchMeans$Harsh$Processed - crunchMeans$Harsh$Unprocessed, 
					     combDiffPCA, c("E:harsh", "D:harsh"))
crunchDiffDists["bright",] <- calculateDistance(crunchMeans$Bright$Processed - crunchMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
crunchDiffDists["crunch",] <- calculateDistance(crunchMeans$Crunch$Processed - crunchMeans$Crunch$Unprocessed, 
					      combDiffPCA, "D:crunch")

pdf("CrunchDifferenceJeffsDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(crunchDiffDists, c("green", "turquoise", "purple"))
dev.off()
embed_fonts("CrunchDifferenceJeffsDistance.pdf")

# save things
save(harshProcDists, harshDiffDists, crunchProcDists, crunchDiffDists, file="JeffsDistances.RData")

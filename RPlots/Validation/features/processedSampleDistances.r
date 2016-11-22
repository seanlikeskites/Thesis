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
		dists[i,] <- mahalanobis(coords[,1:(n-1)], center, covariance)
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
	lines <- c(lines, paste("\t\\hhline{-::",
				paste(rep("=:", nInstruments), collapse=""),
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

instruments <- c("Bass1", "Bass2", "Flute", "Guitar1", "Guitar2", "Marimba", "Oboe",  "Saxophone", "Trumpet", "Violin")
nInstruments <- length(instruments)

# harsh processed distances
harshProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("Warm", "Bright", "Harsh"), instruments))
harshProcDists["Warm",] <- calculateDistance(harshMeans$Warm$Processed, combProcPCA, c("E:warm", "D:warm"))
harshProcDists["Bright",] <- calculateDistance(harshMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
harshProcDists["Harsh",] <- calculateDistance(harshMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))
makeDistanceTable(harshProcDists, "HarshProcessedJeffsDistance.tex")

# harsh difference distances
harshDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("Warm", "Bright", "Harsh"), instruments))
harshDiffDists["Warm",] <- calculateDistance(harshMeans$Warm$Processed - harshMeans$Warm$Unprocessed, 
					     combDiffPCA, c("E:warm", "D:warm"))
harshDiffDists["Bright",] <- calculateDistance(harshMeans$Bright$Processed - harshMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
harshDiffDists["Harsh",] <- calculateDistance(harshMeans$Harsh$Processed - harshMeans$Harsh$Unprocessed, 
					      combDiffPCA, c("E:harsh", "D:harsh"))
makeDistanceTable(harshDiffDists, "HarshDifferenceJeffsDistance.tex")

# crunch processed distances
crunchProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("Harsh", "Bright", "Crunch"), instruments))
crunchProcDists["Harsh",] <- calculateDistance(crunchMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))
crunchProcDists["Bright",] <- calculateDistance(crunchMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
crunchProcDists["Crunch",] <- calculateDistance(crunchMeans$Crunch$Processed, combProcPCA, "D:crunch")
makeDistanceTable(crunchProcDists, "CrunchProcessedJeffsDistance.tex")

# crunch difference distances
crunchDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("Harsh", "Bright", "Crunch"), instruments))
crunchDiffDists["Harsh",] <- calculateDistance(crunchMeans$Harsh$Processed - crunchMeans$Harsh$Unprocessed, 
					     combDiffPCA, c("E:harsh", "D:harsh"))
crunchDiffDists["Bright",] <- calculateDistance(crunchMeans$Bright$Processed - crunchMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
crunchDiffDists["Crunch",] <- calculateDistance(crunchMeans$Crunch$Processed - crunchMeans$Crunch$Unprocessed, 
					      combDiffPCA, "D:crunch")
makeDistanceTable(crunchDiffDists, "CrunchDifferenceJeffsDistance.tex")

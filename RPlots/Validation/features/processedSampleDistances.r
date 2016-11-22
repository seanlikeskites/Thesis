load("../../SAFEAnalysis/PCAData.RData")
load("ValidationMeans.RData")
source("../../SAFEAnalysis/descriptorPositions.r")

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

instruments <- c("Bass1", "Bass2", "Flute", "Guitar1", "Guitar2", "Marimba", "Oboe",  "Saxophone", "Trumpet", "Violin")
nInstruments <- length(instruments)

# harsh processed distances
harshProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("warm", "bright", "harsh"), instruments))
harshProcDists["warm",] <- calculateDistance(harshMeans$Warm$Processed, combProcPCA, c("E:warm", "D:warm"))
harshProcDists["bright",] <- calculateDistance(harshMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
harshProcDists["harsh",] <- calculateDistance(harshMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))

# harsh difference distances
harshDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("warm", "bright", "harsh"), instruments))
harshDiffDists["warm",] <- calculateDistance(harshMeans$Warm$Processed - harshMeans$Warm$Unprocessed, 
					     combDiffPCA, c("E:warm", "D:warm"))
harshDiffDists["bright",] <- calculateDistance(harshMeans$Bright$Processed - harshMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
harshDiffDists["harsh",] <- calculateDistance(harshMeans$Harsh$Processed - harshMeans$Harsh$Unprocessed, 
					      combDiffPCA, c("E:harsh", "D:harsh"))

# crunch processed distances
crunchProcDists <-  matrix(0, 3, nInstruments, dimnames=list(c("harsh", "bright", "crunch"), instruments))
crunchProcDists["harsh",] <- calculateDistance(crunchMeans$Harsh$Processed, combProcPCA, c("E:harsh", "D:harsh"))
crunchProcDists["bright",] <- calculateDistance(crunchMeans$Bright$Processed, combProcPCA, c("E:bright", "D:bright"))
crunchProcDists["crunch",] <- calculateDistance(crunchMeans$Crunch$Processed, combProcPCA, "D:crunch")

# crunch difference distances
crunchDiffDists <-  matrix(0, 3, nInstruments, dimnames=list(c("harsh", "bright", "crunch"), instruments))
crunchDiffDists["harsh",] <- calculateDistance(crunchMeans$Harsh$Processed - crunchMeans$Harsh$Unprocessed, 
					     combDiffPCA, c("E:harsh", "D:harsh"))
crunchDiffDists["bright",] <- calculateDistance(crunchMeans$Bright$Processed - crunchMeans$Bright$Unprocessed, 
					       combDiffPCA, c("E:bright", "D:bright"))
crunchDiffDists["crunch",] <- calculateDistance(crunchMeans$Crunch$Processed - crunchMeans$Crunch$Unprocessed, 
					      combDiffPCA, "D:crunch")

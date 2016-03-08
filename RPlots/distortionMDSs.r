library(SAFER)

# connect to the database
con <- connectToSAFE()

# get the descriptors we want
descriptors <- c("boomy", "bright", "clipped", "creamy", "crispy", "crunch", "crunchy",
		 "decimated", "destroyed", "drive", "driven", "fuzz", "fuzzy", "grainy",
		 "harsh", "heavy", "loud", "muffled", "raspy", "smooth", "warm")

# get the features
features = c(SpectralFeatures, 38:42)

numDescriptors <- length(descriptors)
numFeatures <- length(features)

featureDifferenceMatrix <- matrix(nrow=numDescriptors, ncol=numFeatures)
differenceAgreements <- vector(length=numDescriptors)

processedFeatureMatrix <- matrix(nrow=numDescriptors, ncol=numFeatures)
processedAgreements <- vector(length=numDescriptors)

index <- 1

for (descriptor in descriptors)
{
	# grab the feature data 
	featureMeans <- getDescriptorFeatureMeans("SAFEDistortion", descriptor, features, con)

	if (nrow(featureMeans$Processed) < 2)
	{
		index <- index + 1
		next
	}
	
	featureDifferences <- featureMeans$Processed - featureMeans$Unprocessed
	differenceAgreements[index] <- agreement(featureDifferences)
	featureDifferenceMatrix[index,] <- colMeans(featureDifferences, TRUE)

	processedAgreements[index] <- agreement(featureMeans$Processed)
	processedFeatureMatrix[index,] <- colMeans(featureMeans$Processed, TRUE)

	index <- index + 1
}

# set the matrix names
rownames(featureDifferenceMatrix) <- descriptors
colnames(featureDifferenceMatrix) <- colnames(featureDifferences)

rownames(processedFeatureMatrix) <- descriptors
colnames(processedFeatureMatrix) <- colnames(featureDifferences)

# normalise because Spyros has decreed it
featureDifferenceMatrix <- normalise(featureDifferenceMatrix)
processedFeatureMatrix <- normalise(processedFeatureMatrix)

# calculate MDSs
differenceDistances <- dist(featureDifferenceMatrix[complete.cases(featureDifferenceMatrix),])
differenceMDS <- cmdscale(differenceDistances, k=2, eig=TRUE)

processedDistances <- dist(processedFeatureMatrix[complete.cases(processedFeatureMatrix),])
processedMDS <- cmdscale(processedDistances, k=2, eig=TRUE)


# plot it up
descriptorsToPlot <- descriptors #c("warm", "crunch", "fuzz", "fuzzy", "bright", "harsh")

colourPalette <- rainbow(length(descriptorsToPlot))

differenceXs <- differenceMDS$points[,1][descriptorsToPlot]
differenceXRange <- diff(range(differenceXs))
differenceXLimits <- c(min(differenceXs) - 0.15 * differenceXRange, max(differenceXs) + 0.15 * differenceXRange)

differenceYs <- differenceMDS$points[,2][descriptorsToPlot]
differenceYRange <- diff(range(differenceYs))
differenceYLimits <- c(min(differenceYs) - 0.15 * differenceYRange, max(differenceYs) + 0.15 * differenceYRange)

setEPS()
postscript("DistortionDifferenceMDS.eps")

plot(differenceXs, differenceYs, type='n', xlab="First Dimension", ylab="Second Dimension", xlim=differenceXLimits, ylim=differenceYLimits)

sizes <- 3 * differenceAgreements / max(differenceAgreements[is.finite(differenceAgreements)]) + 1
text(differenceXs, differenceYs, descriptorsToPlot, col=colourPalette)

dev.off()

processedXs <- processedMDS$points[,1][descriptorsToPlot]
processedXRange <- diff(range(processedXs))
processedXLimits <- c(min(processedXs) - 0.15 * processedXRange, max(processedXs) + 0.15 * processedXRange)

processedYs <- processedMDS$points[,2][descriptorsToPlot]
processedYRange <- diff(range(processedYs))
processedYLimits <- c(min(processedYs) - 0.15 * processedYRange, max(processedYs) + 0.15 * processedYRange)

setEPS()
postscript("DistortionProcessedMDS.eps")

plot(processedXs, processedYs, type='n', xlab="First Dimension", ylab="Second Dimension", xlim=processedXLimits, ylim=processedYLimits)

sizes <- 3 * processedAgreements / max(processedAgreements[is.finite(processedAgreements)]) + 1
text(processedXs, processedYs, descriptorsToPlot, col=colourPalette)

dev.off()

# let's play with some correlations shit
differenceDim1Correlations <- cor(featureDifferenceMatrix, differenceXs)
differenceDim2Correlations <- cor(featureDifferenceMatrix, differenceYs)

processedDim1Correlations <- cor(processedFeatureMatrix, processedXs)
processedDim2Correlations <- cor(processedFeatureMatrix, processedYs)

# disconnect from the database
dbDisconnect(con)

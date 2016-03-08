library(SAFER)
library(tsne)

# connect to the database
con <- connectToSAFE()

# get the descriptors we want
descriptors <- c("bright", "warm", "fuzzy", "fuzz", "crunch", "raspy", "harsh", "creamy")
descriptorString <- paste(descriptors, collapse="', '")
numEntries <- dbGetQuery(con, paste("SELECT COUNT(Descriptors) FROM SAFEDistortionUserData WHERE Descriptors IN ('",
				    descriptorString, "');", sep=""))[1,]

# get the features
features = c(SpectralFeatures, 38:42)
numFeatures <- length(features)

featureDifferenceMatrix <- matrix(nrow=numEntries, ncol=numFeatures)
processedFeatureMatrix <- matrix(nrow=numEntries, ncol=numFeatures)

descriptorsToPlot <- character(length=numEntries)

index <- 1

for (descriptor in descriptors)
{
	entries = dbGetQuery(con, paste("SELECT ID from SAFEDistortionUserData WHERE Descriptors = '",
					descriptor, "';", sep=""))[[1]]
	
	for (entry in entries)
	{
		descriptorsToPlot[index] <- descriptor

		# grab the feature data 
		featureMeans <- getEntryFeatureMeans("SAFEDistortion", entry, features, con)

		featureDifferenceMatrix[index,] <- featureMeans$Processed - featureMeans$Unprocessed

		processedFeatureMatrix[index,] <- featureMeans$Processed

		index <- index + 1
	}
}

# normalise because Spyros has decreed it
featureDifferenceMatrix <- normalise(featureDifferenceMatrix)
processedFeatureMatrix <- normalise(processedFeatureMatrix)

# calculate TSNE
differenceTSNE <- tsne(featureDifferenceMatrix)
processedTSNE <- tsne(processedFeatureMatrix)

# plot it up
colourPalette <- rainbow(length(descriptors))
colours <- numeric(length=length(descriptors))

for (descriptorNum in 1:length(descriptors))
{
	colours[descriptorsToPlot == descriptors[descriptorNum]] = colourPalette[descriptorNum]
}

differenceXs <- differenceTSNE[,1]
differenceXRange <- diff(range(differenceXs))
differenceXLimits <- c(min(differenceXs) - 0.15 * differenceXRange, max(differenceXs) + 0.15 * differenceXRange)

differenceYs <- differenceTSNE[,2]
differenceYRange <- diff(range(differenceYs))
differenceYLimits <- c(min(differenceYs) - 0.15 * differenceYRange, max(differenceYs) + 0.15 * differenceYRange)

setEPS()
postscript("DistortionEntryDifferenceTSNE.eps")

plot(differenceXs, differenceYs, type='n', xlab="First Dimension", ylab="Second Dimension", xlim=differenceXLimits, ylim=differenceYLimits)
text(differenceXs, differenceYs, descriptorsToPlot, col=colours)
dev.off()

processedXs <- processedTSNE[,1]
processedXRange <- diff(range(processedXs))
processedXLimits <- c(min(processedXs) - 0.15 * processedXRange, max(processedXs) + 0.15 * processedXRange)

processedYs <- processedTSNE[,2]
processedYRange <- diff(range(processedYs))
processedYLimits <- c(min(processedYs) - 0.15 * processedYRange, max(processedYs) + 0.15 * processedYRange)

setEPS()
postscript("DistortionEntryProcessedTSNE.eps")
plot(processedXs, processedYs, type='n', xlab="First Dimension", ylab="Second Dimension", xlim=processedXLimits, ylim=processedYLimits)
text(processedXs, processedYs, descriptorsToPlot, col=colours)
dev.off()

# let's play with some correlations shit
#differenceDim1Correlations <- cor(featureDifferenceMatrix, differenceXs)
#differenceDim2Correlations <- cor(featureDifferenceMatrix, differenceYs)
#
#processedDim1Correlations <- cor(processedFeatureMatrix, processedXs)
#processedDim2Correlations <- cor(processedFeatureMatrix, processedYs)

# disconnect from the database
dbDisconnect(con)


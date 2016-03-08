library(SAFER)

# connect to the database
con <- connectToSAFE()

# get the descriptors we want
descriptors <- dbGetQuery(con, "SELECT Descriptors FROM SAFEDistortionUserData WHERE Descriptors != 'test' 
			  	GROUP BY Descriptors HAVING COUNT(Descriptors) > 1;");
descriptors <- tolower(descriptors[[1]])
descriptors <- unique(descriptors)

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

# calculate the PCAs
differencePCA <- princomp(featureDifferenceMatrix)
processedPCA <- princomp(processedFeatureMatrix)

library(SAFER)

# connect to the database
con <- connectToSAFE()

# get the descriptors we want
descriptors <- intersect(getUniqueDescriptors ("SAFEDistortion", con),
			 c("raspy", "warm", "bright", "fuzzy", "crunchy", 
			   "harsh", "grainy", "heavy", "crispy", "destroyed",
			   "crunch", "fuzz", "decimated", "driven"))

# get the features
features = c(6, 7, 9:12, 16, 19, 38, 39, 40, 42)

numDescriptors <- length(descriptors)
numFeatures <- length(features)

featureMatrix <- matrix(nrow=numDescriptors, ncol=numFeatures)
agreements <- vector(length=numDescriptors)

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
	agreements[index] <- agreement(featureDifferences)
	featureMatrix[index,] <- colMeans(featureDifferences)

	index <- index + 1
}

# calculate MDS
descriptorDistances <- dist(featureMatrix[complete.cases(featureMatrix),])
MDS <- cmdscale(descriptorDistances, k=2, eig=TRUE)

# plot it up
colourPalette <- rainbow(length(descriptors))
xs <- MDS$points[,1]
xRange <- diff(range(xs))
xLimits <- c(min(xs) - 0.15 * xRange, max(xs) + 0.15 * xRange)

ys <- MDS$points[,2]
yRange <- diff(range(ys))
yLimits <- c(min(ys) - 0.15 * yRange, max(ys) + 0.15 * yRange)

setEPS()
postscript("DistortionDifferenceMDS.eps")

plot(xs, ys, type='n', xlab="First Dimension", ylab="Second Dimension", xlim=xLimits, ylim=yLimits)

sizes <- 3 * agreements / max(agreements[is.finite(agreements)]) + 1
text(xs, ys, descriptors, cex=sizes, col=colourPalette)

dev.off()

# let's play with some correlations shit

# disconnect from the database
dbDisconnect(con)

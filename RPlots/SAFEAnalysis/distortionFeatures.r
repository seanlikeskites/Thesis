load("distortionData.RData")

firstTen <- function(data)
{
	#if (length(data) > 10)
	#{
	#	return(data[1:10])
	#}
	#else
	#{
		return(data)
	#}
}

###################################################################################################
# processed features
###################################################################################################
# first dimension
significantProcessedDim1 <- abs(processedMDS$Correlations[,1]) > 0.8 & processedMDS$PValues[,1] < 0.05
processedFeaturesDim1 <- processedMDS$Correlations[significantProcessedDim1,1]
processedFeaturesDim1 <- processedFeaturesDim1[order(abs(processedFeaturesDim1), decreasing=TRUE)]
write.csv(format(round(processedFeaturesDim1, 3), digits=3), "DistortionProcessedSignificanceDim1.csv")

processedDim1FeatureValues <- processedMDS$Features[,firstTen(names(processedFeaturesDim1))]
processedDim1FeatureValues <- processedDim1FeatureValues[order(rownames(processedDim1FeatureValues)),]

# second dimension
significantProcessedDim2 <- abs(processedMDS$Correlations[,2]) > 0.8 & processedMDS$PValues[,2] < 0.05
processedFeaturesDim2 <- processedMDS$Correlations[significantProcessedDim2,2]
processedFeaturesDim2 <- processedFeaturesDim2[order(abs(processedFeaturesDim2), decreasing=TRUE)]
write.csv(format(round(processedFeaturesDim2, 3), digits=3), "DistortionProcessedSignificanceDim2.csv")

processedDim2FeatureValues <- processedMDS$Features[,firstTen(names(processedFeaturesDim2))]
processedDim2FeatureValues <- processedDim2FeatureValues[order(rownames(processedDim2FeatureValues)),]

###################################################################################################
# feature differences
###################################################################################################
# first dimension
significantDifferenceDim1 <- abs(differenceMDS$Correlations[,1]) > 0.8 & differenceMDS$PValues[,1] < 0.05
differenceFeaturesDim1 <- differenceMDS$Correlations[significantDifferenceDim1,1]
differenceFeaturesDim1 <- differenceFeaturesDim1[order(abs(differenceFeaturesDim1), decreasing=TRUE)]
write.csv(format(round(differenceFeaturesDim1, 3), digits=3), "DistortionDifferenceSignificanceDim1.csv")

differenceDim1FeatureValues <- differenceMDS$Features[,firstTen(names(differenceFeaturesDim1))]
differenceDim1FeatureValues <- differenceDim1FeatureValues[order(rownames(differenceDim1FeatureValues)),]

# second dimension
significantDifferenceDim2 <- abs(differenceMDS$Correlations[,2]) > 0.8 & differenceMDS$PValues[,2] < 0.05
differenceFeaturesDim2 <- differenceMDS$Correlations[significantDifferenceDim2,2]
differenceFeaturesDim2 <- differenceFeaturesDim2[order(abs(differenceFeaturesDim2), decreasing=TRUE)]
write.csv(format(round(differenceFeaturesDim2, 3), digits=3), "DistortionDifferenceSignificanceDim2.csv")

differenceDim2FeatureValues <- differenceMDS$Features[,firstTen(names(differenceFeaturesDim2))]
differenceDim2FeatureValues <- differenceDim2FeatureValues[order(rownames(differenceDim2FeatureValues)),]

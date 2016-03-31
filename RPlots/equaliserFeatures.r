load("equaliserData.RData")

# processed features
significantProcessedDim1 <- abs(processedMDS$Correlations[,1]) > 0.8 & processedMDS$PValues[,1] < 0.05
processedFeaturesDim1 <- processedMDS$Correlations[significantProcessedDim1,1]
processedFeaturesDim1 <- processedFeaturesDim1[order(abs(processedFeaturesDim1), decreasing=TRUE)]
write.csv(format(round(processedFeaturesDim1, 3), digits=3), "EqualiserProcessedSignificanceDim1.csv")

significantProcessedDim2 <- abs(processedMDS$Correlations[,2]) > 0.8 & processedMDS$PValues[,2] < 0.05
processedFeaturesDim2 <- processedMDS$Correlations[significantProcessedDim2,2]
processedFeaturesDim2 <- processedFeaturesDim2[order(abs(processedFeaturesDim2), decreasing=TRUE)]
write.csv(format(round(processedFeaturesDim2, 3), digits=3), "EqualiserProcessedSignificanceDim2.csv")

#feature differences
significantDifferenceDim1 <- abs(differenceMDS$Correlations[,1]) > 0.8 & differenceMDS$PValues[,1] < 0.05
differenceFeaturesDim1 <- differenceMDS$Correlations[significantDifferenceDim1,1]
differenceFeaturesDim1 <- differenceFeaturesDim1[order(abs(differenceFeaturesDim1), decreasing=TRUE)]
write.csv(format(round(differenceFeaturesDim1, 3), digits=3), "EqualiserDifferenceSignificanceDim1.csv")

significantDifferenceDim2 <- abs(differenceMDS$Correlations[,2]) > 0.8 & differenceMDS$PValues[,2] < 0.05
differenceFeaturesDim2 <- differenceMDS$Correlations[significantDifferenceDim2,2]
differenceFeaturesDim2 <- differenceFeaturesDim2[order(abs(differenceFeaturesDim2), decreasing=TRUE)]
write.csv(format(round(differenceFeaturesDim2, 3), digits=3), "EqualiserDifferenceSignificanceDim2.csv")

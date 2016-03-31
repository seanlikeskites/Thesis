load("equaliserData.RData")

# processed features
significantProcessedDim1 <- abs(processedMDS$Correlations[,1]) > 0.8 & processedMDS$PValues[,1] < 0.05
processedFeaturesDim1 <- cbind(processedMDS$Correlations[significantProcessedDim1,1],
                               processedMDS$PValues[significantProcessedDim1,1])
processedFeaturesDim1 <- processedFeaturesDim1[order(abs(processedFeaturesDim1[,1]), decreasing=TRUE),]
write.csv(processedFeaturesDim1, "EqualiserProcessedSignificanceDim1.csv")

significantProcessedDim2 <- abs(processedMDS$Correlations[,2]) > 0.8 & processedMDS$PValues[,2] < 0.05
processedFeaturesDim2 <- cbind(processedMDS$Correlations[significantProcessedDim2,2],
                               processedMDS$PValues[significantProcessedDim2,2])
processedFeaturesDim2 <- processedFeaturesDim2[order(abs(processedFeaturesDim2[,1]), decreasing=TRUE),]
write.csv(processedFeaturesDim2, "EqualiserProcessedSignificanceDim2.csv")

#feature differences
significantDifferenceDim1 <- abs(differenceMDS$Correlations[,1]) > 0.8 & differenceMDS$PValues[,1] < 0.05
differenceFeaturesDim1 <- cbind(differenceMDS$Correlations[significantDifferenceDim1,1],
                               differenceMDS$PValues[significantDifferenceDim1,1])
differenceFeaturesDim1 <- differenceFeaturesDim1[order(abs(differenceFeaturesDim1[,1]), decreasing=TRUE),]
write.csv(differenceFeaturesDim1, "EqualiserDifferenceSignificanceDim1.csv")

significantDifferenceDim2 <- abs(differenceMDS$Correlations[,2]) > 0.8 & differenceMDS$PValues[,2] < 0.05
differenceFeaturesDim2 <- cbind(differenceMDS$Correlations[significantDifferenceDim2,2],
                               differenceMDS$PValues[significantDifferenceDim2,2])
differenceFeaturesDim2 <- differenceFeaturesDim2[order(abs(differenceFeaturesDim2[,1]), decreasing=TRUE),]
write.csv(differenceFeaturesDim2, "EqualiserDifferenceSignificanceDim2.csv")

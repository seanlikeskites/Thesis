load("distortionData.RData")

processedFeatures <- processedMDS$Correlations[apply(abs(processedMDS$Correlations) > 0.8, 1, FUN=sum) > 0,]
indecies <- sort(apply(abs(processedFeatures), 1, FUN=max), decreasing=TRUE, index.return=TRUE)$ix
processedFeatures <- processedFeatures[indecies,]

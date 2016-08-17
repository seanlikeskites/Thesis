load("equaliserData.RData")

processedCounts <- as.matrix(table(rownames(processedMDS$Features)))
differenceCounts <- as.matrix(table(rownames(differenceMDS$Features)))

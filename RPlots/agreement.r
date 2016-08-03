termAgreement <- function(data, nPCs=5)
{
	scaledData <- scale(data)[,1:nPCs]
	descriptors <- rownames(scaledData)
	uniqueDescriptors <- sort(unique(descriptors))
	nDescriptors <- length(uniqueDescriptors)
	agreements <- array(0, nDescriptors)

	for (i in 1:nDescriptors)
	{
		tempData <- scaledData[rownames(scaledData) == uniqueDescriptors[i],]
		eig <- eigen(cov(tempData))$values
		n <- nrow(tempData) - 1

		if (n > nPCs)
		{
			n <- nPCs
		}

		agreements[i] <- sum(1 / eig[1:2])
	}

	return(agreements)
}

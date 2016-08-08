termAgreement <- function(data, nPCs=5)
{
	descriptors <- rownames(data)
	uniqueDescriptors <- sort(unique(descriptors))
	nDescriptors <- length(uniqueDescriptors)
	agreements <- array(0, nDescriptors)

	for (i in 1:nDescriptors)
	{
		tempData <- data[rownames(data) == uniqueDescriptors[i],]
		eig <- eigen(cov(tempData))$values
		n <- nrow(tempData) - 1

		if (n > nPCs)
		{
			n <- nPCs
		}

		agreements[i] <- sum(1 / eig[1:2])
	}

	names(agreements) <- uniqueDescriptors

	return(agreements)
}

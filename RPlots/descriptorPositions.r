getDescriptorPositions <- function(data, descriptors)
{
	out <- matrix(0, 0, ncol(data))

	for (desc in descriptors)
	{
		descriptorData <- data[grep(desc, rownames(data)),]
		rownames(descriptorData) <- array(desc, nrow(descriptorData))
		out <- rbind(out, descriptorData)
	}

	return(out)
}


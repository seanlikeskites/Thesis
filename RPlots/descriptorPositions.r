getDescriptorPositions <- function(data, descriptors)
{
	out <- matrix(0, 0, ncol(data))

	for (desc in descriptors)
	{
		regex <- paste("\\b", desc, sep="")
		descriptorData <- data[grep(regex, rownames(data)),]
		descriptorData <- descriptorData[!grepl("free", rownames(descriptorData)),] # get rid of "mud-free"
		rownames(descriptorData) <- array(desc, nrow(descriptorData))
		out <- rbind(out, descriptorData)
	}

	return(out)
}

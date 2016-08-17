combineData <- function(distData, eqData)
{
	rownames(distData) <- gsub("^|\\W", " D:", rownames(distData))
	rownames(eqData) <- gsub("^|\\W", " E:", rownames(eqData))

	combData <- rbind(distData, eqData)

	return(combData)
}

matrixCorrelationTest <- function(data1, data2)
{
	if (nrow(data1) != nrow(data2))
	{
		stop("The number of rows in each dataset doesn't match")
	}

	numRows <- ncol(data1)
	numCols <- ncol(data2)
	correlations <- matrix(nrow=numRows, ncol=numCols)
	rownames(correlations) <- colnames(data1)
	colnames(correlations) <- colnames(data2)
	pvalues <- matrix(nrow=numRows, ncol=numCols)
	rownames(pvalues) <- colnames(data1)
	colnames(pvalues) <- colnames(data2)

	for (i in 1:numRows)
	{
		for (j in 1:numCols)
		{
			cortest <- cor.test (data1[,i], data2[,j])
			correlations[i,j] <- cortest$estimate
			pvalues[i,j] <- cortest$p.value
		}
	}

	output <- list()
	output$correlations <- correlations
	output$pValues <- pvalues

	return(output)
}

getSalientFeatures <- function(c, minCorr=0.9, maxP=0.05)
{
	numPCs <- nrow(c$correlations)
	features <- character()

	for (i in 1:numPCs)
	{
		topFeatures <- colnames(c$correlations)[(abs(c$correlations[i,]) > minCorr) & (c$pValues[i,] < maxP)]
		features <- c(features, topFeatures[!grepl("Bark", topFeatures)])
	}

	return(features)
}

makeCorrelationTable <- function(data, outFile, goodCorr=0.8)
{
	data <- data[,order(apply(abs(data), 2, max), decreasing=TRUE)]
	lines <- character()

	numPCs <- nrow(data)
	firstLine <- paste("\\begin{tabular}{|c|", paste(rep("c|", numPCs), collapse=""),
			   "}\n\t\\cline{2-", numPCs + 1, "}", sep="")
	lines <- c(lines, firstLine)

	titles <- paste("\t\\multicolumn{1}{c|}{} & \\multicolumn{", 
			numPCs, 
			"}{c|}{\\bf{Correlation}} \\tabularnewline\n\t\\hline", 
			sep="")
	lines <- c(lines, titles)

	subtitles <- "\t\\bf{Feature}"
	for (i in 1:numPCs)
	{
		subtitles <- paste(subtitles, " & \\bf{PC", i, "}", sep="")
	}
	subtitles <- paste(subtitles, " \\tabularnewline\n\t\\hline\n\t\\hline", sep="")
	lines <- c(lines, subtitles)

	features <- colnames(data)

	for (i in 1:ncol(data))
	{
		feature <- gsub("_", " ", features[i])
		line <- paste("\t\\bf{", feature, "}", sep="")

		for (k in 1:numPCs)
		{
			if (abs(data[k,i]) > goodCorr)
				line <- paste(line, " & \\bf{", format(data[k,i], digits=3), "}", sep="")
			else
				line <- paste(line, " & ", format(data[k,i], digits=3), sep="")
		}

		line <- paste(line, " \\tabularnewline\n\t\\hline", sep="")

		lines <- c(lines, line)
	}

	lastLine <- "\\end{tabular}"
	lines <- c(lines, lastLine)

	f <- file(outFile)
	writeLines(lines, f)
	close(f)
}

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

#getSalientFeatures <- function(c, minCorr=0.9, maxP=0.05)
#{
#	numPCs <- nrow(c$correlations)
#	features <- character()
#
#	for (i in 1:numPCs)
#	{
#		topFeatures <- colnames(c$correlations)[(abs(c$correlations[i,]) > minCorr) & (c$pValues[i,] < maxP)]
#		features <- c(features, topFeatures[!grepl("Bark", topFeatures)])
#	}
#
#	return(features)
#}

getSalientFeatures <- function(correlations, minCorr=0.9, maxP=0.01)
{
	cor <- correlations$correlations
	ps <- correlations$pValues
	nPCs <- nrow(cor)
	notBark <- !grepl("Bark", colnames(cor))
	cor <- cor[,notBark]
	ps <- ps[,notBark]
	out <- list()

	for (i in 1:nPCs)
	{
		current <- cor[i,]
		currentPs <- ps[i,]
		large <- current[(abs(current) > minCorr) & (currentPs < maxP)]
		top <- large[order(abs(large), decreasing=TRUE)]

		if (length(top))
		{
			out[[i]] <- top
		}
	}

	return(out)
}

makeCorrelationList <- function(data, outFile)
{
	lines <- character()
	firstLine <- "\\begin{itemize}"
	lines <- c(lines, firstLine)

	nPCs <- length(data)

	for (i in 1:nPCs)
	{
		line <- paste("\t\\item {\\bf{PC ", i, ":}} ", sep="")

		cors <- format(data[[i]], digits=3)
		features <- names(cors)

		featureString <- paste(paste(features, cors, sep=" ($p = "), collapse="$), ")
		line <- paste(line, featureString, "$).", sep="")

		lines <- c(lines, line)
	}

	lastLine <- "\\end{itemize}"
	lines <- c(lines, lastLine)

	f <- file(outFile)
	writeLines(lines, f)
	close(f)
}

#makeCorrelationTable <- function(data, outFile, goodCorr=0.8)
#{
#	data <- data[,order(apply(abs(data), 2, max), decreasing=TRUE)]
#	lines <- character()
#
#	numPCs <- nrow(data)
#	firstLine <- paste("\\begin{tabular}{|c|", paste(rep("c|", numPCs), collapse=""),
#			   "}\n\t\\cline{2-", numPCs + 1, "}", sep="")
#	lines <- c(lines, firstLine)
#
#	titles <- paste("\t\\multicolumn{1}{c|}{} & \\multicolumn{", 
#			numPCs, 
#			"}{c|}{\\bf{Correlation}} \\tabularnewline\n\t\\hline", 
#			sep="")
#	lines <- c(lines, titles)
#
#	subtitles <- "\t\\bf{Feature}"
#	for (i in 1:numPCs)
#	{
#		subtitles <- paste(subtitles, " & \\bf{PC ", i, "}", sep="")
#	}
#	subtitles <- paste(subtitles, " \\tabularnewline\n\t\\hline\n\t\\hline", sep="")
#	lines <- c(lines, subtitles)
#
#	features <- colnames(data)
#
#	for (i in 1:ncol(data))
#	{
#		line <- paste("\t\\bf{", features[i], "}", sep="")
#
#		for (k in 1:numPCs)
#		{
#			if (abs(data[k,i]) > goodCorr)
#				line <- paste(line, " & \\bf{", format(data[k,i], digits=3), "}", sep="")
#			else
#				line <- paste(line, " & ", format(data[k,i], digits=3), sep="")
#		}
#
#		line <- paste(line, " \\tabularnewline\n\t\\hline", sep="")
#
#		lines <- c(lines, line)
#	}
#
#	lastLine <- "\\end{tabular}"
#	lines <- c(lines, lastLine)
#
#	f <- file(outFile)
#	writeLines(lines, f)
#	close(f)
#}

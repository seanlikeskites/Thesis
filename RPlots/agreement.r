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

		agreements[i] <- sum(1 / (1 + eig[1:n]))
	}

	names(agreements) <- uniqueDescriptors

	return(agreements)
}

makeAgreementTable <- function(data, outFile)
{
	lines <- character()
	nTerms <- length(data)
	data <- sort(data, decreasing=TRUE)
	terms <- names(data)
	agreements <- format(data, digits=1)

	for (i in seq(1, nTerms, 4))
	{
		lines <- c(lines, "\\begin{tabular}{|c|c|}")
		lines <- c(lines, "\t\\hline")
		lines <- c(lines, "\t\\bf{Term} & \\bf{Agreement} \\tabularnewline")
		lines <- c(lines, "\t\\hline")
		lines <- c(lines, "\t\\hline")

		for (n in 0:3)
		{
			termIdx <- i + n
			lines <- c(lines, paste("\t", terms[termIdx], " & ",
						agreements[termIdx], " \\tabularnewline", sep=""))
			lines <- c(lines, "\t\\hline")
		}

		lines <- c(lines, "\\end{tabular}")

		if (i < nTerms - 4)
		{
			lines <- c(lines, "\\qquad")
		}
	}

	f <- file(outFile)
	writeLines(lines, f)
	close(f)
}

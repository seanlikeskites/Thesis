marginOfError <- function(data)
{
	s <- sd(data)
	n <- length(data)
	error <- qt(0.975, n-1) * s / sqrt(n)
}

termConfidence <- function(PCA)
{
	coords <- PCA$x
	termErrs <- apply(coords, 2, function(x) tapply(x, rownames(coords), marginOfError))

	pcVars <- PCA$sdev^2 / sum(PCA$sdev^2)

	confidences <- t(pcVars / t(termErrs))
}

termConfidence <- function(PCA)
{
	coords <- PCA$x
	termVars <- apply(coords, 2, function(x) tapply(x, rownames(coords), var))

	pcVars <- PCA$sdev^2 / sum(PCA$sdev^2)

	confidences <- t(pcVars / t(termVars))
}

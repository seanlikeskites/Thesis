marginOfError <- function(data)
{
	s <- sd(data)
	n <- length(data)
	error <- qt(0.975, n-1) * s
}

socialEqAgreement <- function(data)
{
	num <- log(nrow(data))
	vars <- apply(data, 2, var)
	den <- sum(vars)
	return(num / den)
}

reciprocalOfSumAgreement <- function(data)
{
	vars <- apply(data, 2, var)
	return(1 / sum(vars))
}

sumOfReciprocalsAgreement <- function(data)
{
	vars <- apply(data, 2, var)
	return(sum(1 / vars))
}

sumOfReciprocalEigenvaluesAgreement <- function(data)
{
	eigs <- eigen(cov(data))$values
	return(sum(1 / eigs))
}

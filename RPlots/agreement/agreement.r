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

sumOfReciprocalsAgreement <- function(data)
{
	num <- log(nrow(data))
	vars <- apply(data, 2, var)
	return(num * sum(1 / vars))
}

sumOfReciprocalEigenvaluesAgreement <- function(data)
{
	num <- log(nrow(data))
	eigs <- eigen(cov(data))
	return(num * sum(1 / eigs$values))
}

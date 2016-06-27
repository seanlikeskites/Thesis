marginOfError <- function(data)
{
	s <- sd(data)
	n <- length(data)
	error <- qt(0.975, n-1) * s / sqrt(n)
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
	errors <- apply(data, 2, marginOfError)
	return(sum(1 / errors))
}

sumOfReciprocalEigenvaluesAgreement <- function(data)
{
	eigs <- eigen(cov(data))
	return(sum(1 / eigs$values))
}

library(MASS)
source("agreement.r")

largeCovs <- matrix(0, 3, 3)
largeCovs[1, 1] <- 5
largeCovs[1, 2] <- 0
largeCovs[1, 3] <- 0
largeCovs[2, 1] <- 0
largeCovs[2, 2] <- 5
largeCovs[2, 3] <- 0
largeCovs[3, 1] <- 0
largeCovs[3, 2] <- 0
largeCovs[3, 3] <- 5
large <- mvrnorm(n=110, mu=c(2, 3, 4), Sigma=largeCovs)

smallCovs <- matrix(0, 3, 3)
smallCovs[1, 1] <- 0.2
smallCovs[1, 2] <- 0
smallCovs[1, 3] <- 0
smallCovs[2, 1] <- 0
smallCovs[2, 2] <- 0.2
smallCovs[2, 3] <- 0
smallCovs[3, 1] <- 0
smallCovs[3, 2] <- 0
smallCovs[3, 3] <- 0.2
small <- mvrnorm(n=90, mu=c(4, -2, 1), Sigma=smallCovs)

tallCovs <- matrix(0, 3, 3)
tallCovs[1, 1] <- 0.1
tallCovs[1, 2] <- 0
tallCovs[1, 3] <- 0
tallCovs[2, 1] <- 0
tallCovs[2, 2] <- 6
tallCovs[2, 3] <- 0
tallCovs[3, 1] <- 0
tallCovs[3, 2] <- 0
tallCovs[3, 3] <- 5
tall <- mvrnorm(n=85, mu=c(-2, 0, 2), Sigma=tallCovs)

diagCovs <- matrix(0, 3, 3)
diagCovs[1, 1] <- 5
diagCovs[1, 2] <- 3
diagCovs[1, 3] <- 0
diagCovs[2, 1] <- 3
diagCovs[2, 2] <- 2
diagCovs[2, 3] <- 0
diagCovs[3, 1] <- 0
diagCovs[3, 2] <- 0
diagCovs[3, 3] <- 4
diag <- mvrnorm(n=85, mu=c(5, -3, 4), Sigma=diagCovs)

allX <- c(large[,1], small[,1], tall[,1], diag[,1])
allY <- c(large[,2], small[,2], tall[,2], diag[,2])
allZ <- c(large[,3], small[,3], tall[,3], diag[,3])

colours <- rainbow(4)

setEPS()

postscript("ArtificialData1-2.eps")
plot(allX, allY, type='n', main="")
points(large[,1], large[,2], col=colours[1], pch=4, cex=2)
points(small[,1], small[,2], col=colours[2], pch=4, cex=2)
points(tall[,1], tall[,2], col=colours[3], pch=4, cex=2)
points(diag[,1], diag[,2], col=colours[4], pch=4, cex=2)
dev.off()

postscript("ArtificialData2-3.eps")
plot(allY, allZ, type='n', main="")
points(large[,2], large[,3], col=colours[1], pch=4, cex=2)
points(small[,2], small[,3], col=colours[2], pch=4, cex=2)
points(tall[,2], tall[,3], col=colours[3], pch=4, cex=2)
points(diag[,2], diag[,3], col=colours[4], pch=4, cex=2)
dev.off()

agreements <- matrix(0, 4, 3)
agreements[1,] <- c(socialEqAgreement(large), sumOfReciprocalsAgreement(large), sumOfReciprocalEigenvaluesAgreement(large))
agreements[2,] <- c(socialEqAgreement(small), sumOfReciprocalsAgreement(small), sumOfReciprocalEigenvaluesAgreement(small))
agreements[3,] <- c(socialEqAgreement(tall), sumOfReciprocalsAgreement(tall), sumOfReciprocalEigenvaluesAgreement(tall))
agreements[4,] <- c(socialEqAgreement(diag), sumOfReciprocalsAgreement(diag), sumOfReciprocalEigenvaluesAgreement(diag))

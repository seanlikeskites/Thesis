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
large <- mvrnorm(n=110, mu=c(2, 3, 4), Sigma=largeCovs, empirical=TRUE)

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
small <- mvrnorm(n=90, mu=c(4, -2, 1), Sigma=smallCovs, empirical=TRUE)

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
tall <- mvrnorm(n=85, mu=c(-2, 0, 2), Sigma=tallCovs, empirical=TRUE)

diagCovs <- matrix(0, 3, 3)
diagCovs[1, 1] <- 5
diagCovs[1, 2] <- 0.9 * sqrt(5 * 2)
diagCovs[1, 3] <- 0
diagCovs[2, 1] <- 0.9 * sqrt(5 * 2)
diagCovs[2, 2] <- 2
diagCovs[2, 3] <- 0
diagCovs[3, 1] <- 0
diagCovs[3, 2] <- 0
diagCovs[3, 3] <- 4
diag <- mvrnorm(n=80, mu=c(5, -3, 4), Sigma=diagCovs, empirical=TRUE)

allX <- c(large[,1], small[,1], tall[,1], diag[,1])
allY <- c(large[,2], small[,2], tall[,2], diag[,2])
allZ <- c(large[,3], small[,3], tall[,3], diag[,3])

means <- c(mean(allX), mean(allY), mean(allZ))
sds <- c(sd(allX), sd(allY), sd(allZ))

largeScaled <- scale(large, means, sds)
smallScaled <- scale(small, means, sds)
tallScaled <- scale(tall, means, sds)
diagScaled <- scale(diag, means, sds)

colours <- rainbow(4)
clusters <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

setEPS()

postscript("ArtificialData1-2.eps", pointsize=18)
par(mar=c(4, 4, 0.1, 0.1))
plot(allX, allY, type='n', main="", xlab="PC 1", ylab="PC 2")
points(large[,1], large[,2], col=colours[1], pch=4, cex=2)
points(small[,1], small[,2], col=colours[2], pch=4, cex=2)
points(tall[,1], tall[,2], col=colours[3], pch=4, cex=2)
points(diag[,1], diag[,2], col=colours[4], pch=4, cex=2)
legend("bottomright", legend=clusters, pch=4, col=colours)
dev.off()

postscript("ArtificialData3-2.eps", pointsize=18)
par(mar=c(4, 4, 0.1, 0.1))
plot(allZ, allY, type='n', main="", xlab="PC 3", ylab="PC 2")
points(large[,3], large[,2], col=colours[1], pch=4, cex=2)
points(small[,3], small[,2], col=colours[2], pch=4, cex=2)
points(tall[,3], tall[,2], col=colours[3], pch=4, cex=2)
points(diag[,3], diag[,2], col=colours[4], pch=4, cex=2)
legend("topleft", legend=clusters, pch=4, col=colours)
dev.off()

agreements <- matrix(0, 4, 5)
agreements[1,] <- c(socialEqAgreement(largeScaled), 
		    reciprocalOfSumAgreement(largeScaled),
		    sumOfReciprocalsAgreement(largeScaled), 
		    sumOfReciprocalEigenvaluesAgreement(largeScaled),
		    boundedSumOfReciprocalEigenvaluesAgreement(largeScaled))
agreements[2,] <- c(socialEqAgreement(smallScaled),
		    reciprocalOfSumAgreement(smallScaled),
		    sumOfReciprocalsAgreement(smallScaled), 
		    sumOfReciprocalEigenvaluesAgreement(smallScaled),
		    boundedSumOfReciprocalEigenvaluesAgreement(smallScaled))
agreements[3,] <- c(socialEqAgreement(tallScaled), 
		    reciprocalOfSumAgreement(tallScaled),
		    sumOfReciprocalsAgreement(tallScaled), 
		    sumOfReciprocalEigenvaluesAgreement(tallScaled),
		    boundedSumOfReciprocalEigenvaluesAgreement(tallScaled))
agreements[4,] <- c(socialEqAgreement(diagScaled), 
		    reciprocalOfSumAgreement(diagScaled),
		    sumOfReciprocalsAgreement(diagScaled), 
		    sumOfReciprocalEigenvaluesAgreement(diagScaled),
		    boundedSumOfReciprocalEigenvaluesAgreement(diagScaled))
write.csv(format(agreements, digits=2), file="ArtificialDataAgreements.csv")

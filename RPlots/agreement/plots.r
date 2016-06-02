load("../SAFE_Data.RData")
source("../stemming.r")

plotIndividualPCA <- function(points, legendPos)
{
	descriptors <- rownames(points)
	uniqueDescriptors <- sort(unique(descriptors))
	colourPalette <- rainbow(length(uniqueDescriptors))

	colours <- "black"

	for (i in 1:length(uniqueDescriptors))
	{
		colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
	}

	xs <- points[,1]
	ys <- points[,2]

	xLabel <- "PC1"
	yLabel <- "PC2"

	plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel)
	points(xs, ys, pch=4, col=colours, cex=2)
	legend(legendPos, legend=uniqueDescriptors, pch=4, col=colourPalette)
}

rownames(distDiff) <- safeStem(rownames(distDiff))
distDiffPCA <- prcomp(distDiff, scale=TRUE)

data <- distDiff[rownames(distDiff) %in% c("crunch", "warm", "fuzz"), 
		 colnames(distDiff) %in% c("Irregularity_K", "Spectral_Roll_Off")]
pca <- distDiffPCA$x[rownames(distDiffPCA$x) %in% c("crunch", "warm", "fuzz"), 
		   1:2]


descriptors <- rownames(data)
uniqueDescriptors <- sort(unique(descriptors))
colourPalette <- rainbow(length(uniqueDescriptors))

colours <- "black"

for (i in 1:length(uniqueDescriptors))
{
	colours[descriptors == uniqueDescriptors[i]] <- colourPalette[i]
}

xs <- data[,1]
ys <- data[,2]

xLabel <- "Feature 1"
yLabel <- "Feature 2"

setEPS()

postscript("FeatureSpace.eps")
plot(xs, ys, type='n', main="", xlab=xLabel, ylab=yLabel)
points(xs, ys, pch=4, col=colours, cex=2)
legend("bottomright", legend=uniqueDescriptors, pch=4, col=colourPalette)
dev.off()

theta <- seq(0, 2 * pi, length=1000)

warmPoints <- pca[rownames(pca) == "warm",]
warmCentre <- apply(warmPoints, 2, mean)
warmCov <- cov(warmPoints)
warmEig <- eigen(warmCov)
warmEig$values <- sqrt(warmEig$values)
warmVecX <- (warmEig$values) * warmEig$vectors[1,]
warmVecY <- (warmEig$values) * warmEig$vectors[2,]
warmAngle <- atan2(warmEig$vectors[2,1], warmEig$vectors[1,1])
warmX <- warmCentre[1] + warmEig$values[1] * cos(theta) * cos(warmAngle) - warmEig$values[2] * sin(theta) * sin(warmAngle)
warmY <- warmCentre[2] + warmEig$values[1] * cos(theta) * sin(warmAngle) + warmEig$values[2] * sin(theta) * cos(warmAngle)

fuzzPoints <- pca[rownames(pca) == "fuzz",]
fuzzCentre <- apply(fuzzPoints, 2, mean)
fuzzCov <- cov(fuzzPoints)
fuzzEig <- eigen(fuzzCov)
fuzzEig$values <- sqrt(fuzzEig$values)
fuzzVecX <- (fuzzEig$values) * fuzzEig$vectors[1,]
fuzzVecY <- (fuzzEig$values) * fuzzEig$vectors[2,]
fuzzAngle <- atan2(fuzzEig$vectors[2,1], fuzzEig$vectors[1,1])
fuzzX <- fuzzCentre[1] + fuzzEig$values[1] * cos(theta) * cos(fuzzAngle) - fuzzEig$values[2] * sin(theta) * sin(fuzzAngle)
fuzzY <- fuzzCentre[2] + fuzzEig$values[1] * cos(theta) * sin(fuzzAngle) + fuzzEig$values[2] * sin(theta) * cos(fuzzAngle)

crunchPoints <- pca[rownames(pca) == "crunch",]
crunchCentre <- apply(crunchPoints, 2, mean)
crunchCov <- cov(crunchPoints)
crunchEig <- eigen(crunchCov)
crunchEig$values <- sqrt(crunchEig$values)
crunchVecX <- (crunchEig$values) * crunchEig$vectors[1,]
crunchVecY <- (crunchEig$values) * crunchEig$vectors[2,]
crunchAngle <- atan2(crunchEig$vectors[2,1], crunchEig$vectors[1,1])
crunchX <- crunchCentre[1] + crunchEig$values[1] * cos(theta) * cos(crunchAngle) - crunchEig$values[2] * sin(theta) * sin(crunchAngle)
crunchY <- crunchCentre[2] + crunchEig$values[1] * cos(theta) * sin(crunchAngle) + crunchEig$values[2] * sin(theta) * cos(crunchAngle)

postscript("PCAs.eps")
plotIndividualPCA(pca, "topleft")
lines(warmX, warmY)
lines(crunchX, crunchY)
lines(fuzzX, fuzzY)
arrows (warmCentre[1], warmCentre[2], warmCentre[1] + warmVecX, warmCentre[2] + warmVecY)
arrows (fuzzCentre[1], fuzzCentre[2], fuzzCentre[1] + fuzzVecX, fuzzCentre[2] + fuzzVecY)
arrows (crunchCentre[1], crunchCentre[2], crunchCentre[1] + crunchVecX, crunchCentre[2] + crunchVecY)
dev.off() 

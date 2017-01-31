source("../SAFEAnalysis/stemming.r")
load("../SAFEAnalysis/ClusterDistances.RData")

getDistances <- function(descriptors, cols, distances)
{
	distDescriptors <- c("crunch", "fuzz", "cream", "rasp", "smooth")
	eqDescriptors <- c("clear", "air", "thin", "full", "boom", "box", "tin", "deep", "mud")
	sharedDescriptors <- c("warm", "bright", "harsh")

	outDists <- matrix(0, nrow(descriptors), 3)
	rownames(outDists) <- rownames(descriptors)
	colnames(outDists) <- colnames(descriptors)

	for (i in 1:nrow(descriptors))
	{
		for (j in 1:3)
		{
			term <- descriptors[i, j]
			target <- cols[j]

			if (term %in% distDescriptors)
				term <- paste("D:", term, sep="")
			else if (term %in% eqDescriptors)
				term <- paste("E:", term, sep="")
			else if (term %in% sharedDescriptors)
				term <- c(paste("E:", term, sep=""), paste("D:", term, sep=""))

			if (target %in% distDescriptors)
				target <- paste("D:", target, sep="")
			else if (target %in% eqDescriptors)
				target <- paste("E:", target, sep="")
			else if (target %in% sharedDescriptors)
				target <- c(paste("E:", target, sep=""), paste("D:", target, sep=""))

			outDists[i, j] <- min(distances[term, target])
		}
	}

	return(outDists)
}

groupMeans <- function(data)
{
	means <- apply(data, 2, function(x) tapply(x, rownames(data), mean))
}

groupSds <- function(data)
{
	sds <- apply(data, 2, function(x) tapply(x, rownames(data), sd))
}

descriptors <- sort(c("crunch", "fuzz", "cream", "rasp", "smooth", "clear", "air", "thin", "full", "boom", "box", "tin",
		      "deep", "mud", "warm", "bright", "harsh"))

doods <- dir("results")
nDoods <- length(doods)

harsh <- data.frame()
crunch <- data.frame()

for (dood in doods)
{
	harshFile <- paste("results/", dood, "/HarshResults.txt", sep="")
	tempHarsh <- read.csv(harshFile, header=FALSE)
	harsh <- rbind(harsh, tempHarsh)

	crunchFile <- paste("results/", dood, "/CrunchResults.txt", sep="")
	tempCrunch <- read.csv(crunchFile, header=FALSE)
	crunch <- rbind(crunch, tempCrunch)
}

harsh <- trimws(as.matrix(harsh[order(harsh$V1),]))
rownames(harsh) <- harsh[,1]
harsh <- safeStem(harsh[,2:4])
colnames(harsh) <- c("warm", "bright", "harsh")

crunch <- trimws(as.matrix(crunch[order(crunch$V1),]))
rownames(crunch) <- crunch[,1]
crunch <- safeStem(crunch[,2:4])
colnames(crunch) <- c("harsh", "bright", "crunch")

#######################################
# confusion matrices (but are they really)
#######################################
library(gplots)
library(extrafont)
colMap <- colorRampPalette(c(rgb(0.96, 0.96, 1), rgb(0.1, 0.1, 0.9)), space="rgb", bias=1)

# harsh
harshHarshCounts <- table(harsh[,"harsh"])
harshBrightCounts <- table(harsh[,"bright"])
harshWarmCounts <- table(harsh[,"warm"])

harshConfusion <- matrix(0, 3, length(descriptors), dimnames=list(c("bright", "harsh", "warm"), descriptors))
harshConfusion["harsh", rownames(harshHarshCounts)] <- harshHarshCounts
harshConfusion["bright", rownames(harshBrightCounts)] <- harshBrightCounts
harshConfusion["warm", rownames(harshWarmCounts)] <- harshWarmCounts

sHarshConfusion <- harshConfusion / apply(harshConfusion, 1, sum)
harshLabels <- format(round(sHarshConfusion, 2), nsmall=2)
harshDend <- as.dendrogram(hclust(dist(t(sHarshConfusion)), method="ward.D2"))
pdf("HarshConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=3)
heatmap.2(sHarshConfusion, Colv=harshDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=harshLabels, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=0.8, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80) / 180)
dev.off()
embed_fonts("HarshConfusion.pdf")

# crunch
crunchHarshCounts <- table(crunch[,"harsh"])
crunchBrightCounts <- table(crunch[,"bright"])
crunchCrunchCounts <- table(crunch[,"crunch"])

crunchConfusion <- matrix(0, 3, length(descriptors), dimnames=list(c("bright", "crunch", "harsh"), descriptors))
crunchConfusion["harsh", rownames(crunchHarshCounts)] <- crunchHarshCounts
crunchConfusion["bright", rownames(crunchBrightCounts)] <- crunchBrightCounts
crunchConfusion["crunch", rownames(crunchCrunchCounts)] <- crunchCrunchCounts

sCrunchConfusion <- crunchConfusion / apply(crunchConfusion, 1, sum)
crunchLabels <- format(round(sCrunchConfusion, 2), nsmall=2)
crunchDend <- as.dendrogram(hclust(dist(t(sCrunchConfusion)), method="ward.D2"))
pdf("CrunchConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=3)
heatmap.2(sCrunchConfusion, Colv=crunchDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=crunchLabels, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=0.8, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80) / 180)
dev.off()
embed_fonts("CrunchConfusion.pdf")

# combined
combConfusion <- matrix(0, 4, length(descriptors), dimnames=list(c("bright", "crunch", "harsh", "warm"), descriptors))
combConfusion["harsh",] <- harshConfusion["harsh",] + crunchConfusion["harsh",]
combConfusion["bright",] <- harshConfusion["bright",] + crunchConfusion["bright",]
combConfusion["warm",] <- harshConfusion["warm",]
combConfusion["crunch",] <- crunchConfusion["crunch",]

sCombConfusion <- combConfusion / apply(combConfusion, 1, sum)
combLabels <- format(round(sCombConfusion, 2), nsmall=2)
combDend <- as.dendrogram(hclust(dist(t(sCombConfusion)), method="ward.D2"))
pdf("CombinedConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=3.75)
heatmap.2(sCombConfusion, Colv=combDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=combLabels, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=0.8, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80) / 180)
dev.off()
embed_fonts("CombinedConfusion.pdf")

#######################################
# distances
#######################################
harshProcDists <- getDistances(harsh, c("warm", "bright", "harsh"), combProcDist)
harshProcMeans <- groupMeans(harshProcDists)
harshProcSds <- groupSds(harshProcDists)

harshDiffDists <- getDistances(harsh, c("warm", "bright", "harsh"), combDiffDist)
harshDiffMeans <- groupMeans(harshDiffDists)
harshDiffSds <- groupSds(harshDiffDists)

crunchProcDists <- getDistances(crunch, c("harsh", "bright", "crunch"), combProcDist)
crunchProcMeans <- groupMeans(crunchProcDists)
crunchProcSds <- groupSds(crunchProcDists)

crunchDiffDists <- getDistances(crunch, c("harsh", "bright", "crunch"), combDiffDist)
crunchDiffMeans <- groupMeans(crunchDiffDists)
crunchDiffSds <- groupSds(crunchDiffDists)

#######################################
# clusters
#######################################
# processed
procAirCluster <- list()
procAirCluster$height <- combProcDist["E:air", "E:tin"]
procAirCluster$col <- "brown4"
procAirCluster$name <- "air"

procWarmCluster <- list()
procWarmCluster$height <- combProcDist["E:mud", "E:warm"]
procWarmCluster$col <- "yellow4"
procWarmCluster$name <- "warmth"

procClusts <- list(procAirCluster, procWarmCluster)

# differences
diffWarmthClust <- list()
diffWarmthClust$height <- combDiffDist["E:mud", "D:cream"]
diffWarmthClust$col <- "tomato3"
diffWarmthClust$name <- "warmth"

diffDistBrightClust <- list()
diffDistBrightClust$height <- combDiffDist["D:fuzz", "D:rasp"]
diffDistBrightClust$col <- "orchid4"
diffDistBrightClust$name <- "D:bright"

diffEqBrightClust <- list()
diffEqBrightClust$height <- combDiffDist["E:clear", "E:harsh"]
diffEqBrightClust$col <- "darkcyan"
diffEqBrightClust$name <- "E:bright"

diffClusts <- list(diffWarmthClust, diffDistBrightClust, diffEqBrightClust)

#######################################
# bar charts
#######################################
prettyInstrumentNames <- function(names)
{
	for (i in 1:length(names))
	{
		if (names[i] == "Bass1")
			names[i] <- "B1"
		else if (names[i] == "Bass2")
			names[i] <- "B2"
		else if (names[i] == "Flute")
			names[i] <- "F"
		else if (names[i] == "Guitar1")
			names[i] <- "G1"
		else if (names[i] == "Guitar2")
			names[i] <- "G2"
		else if (names[i] == "Marimba")
			names[i] <- "M"
		else if (names[i] == "Oboe")
			names[i] <- "O"
		else if (names[i] == "Saxophone")
			names[i] <- "S"
		else if (names[i] == "Trumpet")
			names[i] <- "T"
		else if (names[i] == "Violin")
			names[i] <- "V"
	}

	return(names)
}

interleave <- function(x, y)
{
	lenX <- length(x)
	lenY <- length(y)
	maxLen <- max(lenX, lenY)

	out <- c(x[[1]], y[[1]])

	for (i in 2:maxLen)
	{
		if (i <= lenX)
			out <- c(out, x[[i]])
		
		if (i <= lenY)
			out <- c(out, y[[i]])
	}

	return(out)
}

plotDistanceBarChart <- function(means, sds, nDoods, colours, clusters)
{
	means <- t(means)
	sds <- t(sds)

	names <- colnames(means)
	descriptors <- rownames(means)
	plotNames <- prettyInstrumentNames(names)

	error <- qt(0.95, df=nDoods - 1) * sds / sqrt (nDoods)
	mins <- means - error
	maxs <- means + error
	lims <- c(0, 42)

	centres <- barplot(means, ylab="Cophenetic Distance", xaxt="n", ylim=lims, beside=TRUE,
			   col=colours, legend=FALSE, axes=FALSE)
	labelPoints <- centres[seq(2, 3 * ncol(means), 3)]

	clusterWidth <- 2.5
	clusterCols <- list()
	clusterNames <- list()

	for (clust in clusters)
	{
		axis(2, at=clust$height, labels=FALSE, col=clust$col, tcl=-1, lwd.ticks=clusterWidth, line=0.06)

		clusterCols <- c(clusterCols, clust$col)
		clusterNames <- c(clusterNames, clust$name)
	}

	axis(2)
	axis(1, at=labelPoints, line=-1, lwd=0, labels=plotNames)
	mtext("Test Signal", 1, 2)

	# do a legend
	nDescriptors <- length(descriptors)
	nClusters <- length(clusters)
	legendLabels <- interleave(descriptors, clusterNames)
	legendFill <- interleave(colours, array(NA, nClusters))
	legendLty <- interleave(array(NA, nDescriptors), array(1, nClusters))
	legendBorder <- interleave(array("black", nDescriptors), array("white", nClusters))
	legendCols <- interleave(array("white", nDescriptors), clusterCols)

	legend("top", legend=legendLabels, ncol=3, lwd=clusterWidth,
	       lty=legendLty, fill=legendFill, seg.len=0.6, 
	       border=legendBorder, col=legendCols, merge=TRUE)

	# clip so we don't get yuckiness
	usr <- par("usr")
	clip(usr[1], usr[2], usr[3], usr[4])
	arrows(centres, mins, centres, maxs, angle=90, length=0.01, code=3, col="red")
}

pdf("HarshProcessedCophDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(harshProcMeans, harshProcSds, nDoods, c("blue", "turquoise", "green"), procClusts)
dev.off()
embed_fonts("HarshProcessedCophDistance.pdf")

pdf("HarshDifferenceCophDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(harshDiffMeans, harshDiffSds, nDoods, c("blue", "turquoise", "green"), diffClusts)
dev.off()
embed_fonts("HarshDifferenceCophDistance.pdf")

pdf("CrunchProcessedCophDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(crunchProcMeans, crunchProcSds, nDoods, c("green", "turquoise", "purple"), procClusts)
dev.off()
embed_fonts("CrunchProcessedCophDistance.pdf")

pdf("CrunchDifferenceCophDistance.pdf", pointsize=8, family="CM Sans", width=2.94, height=2.1)
par(mar=c(3, 4, 0.5, 0))
plotDistanceBarChart(crunchDiffMeans, crunchDiffSds, nDoods, c("green", "turquoise", "purple"), diffClusts)
dev.off()
embed_fonts("CrunchDifferenceCophDistance.pdf")

#######################################
# rank correlations
#######################################
load("features/JeffsDistances.RData")

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
			cortest <- cor.test (data1[,i], data2[,j], method="s")
			correlations[i,j] <- cortest$estimate
			pvalues[i,j] <- cortest$p.value
		}
	}

	output <- list()
	output$correlations <- correlations
	output$pValues <- pvalues

	return(output)
}

harshProcCor <- matrixCorrelationTest(harshProcMeans, t(harshProcDists))
harshDiffCor <- matrixCorrelationTest(harshDiffMeans, t(harshDiffDists))
crunchProcCor <- matrixCorrelationTest(crunchProcMeans, t(crunchProcDists))
crunchDiffCor <- matrixCorrelationTest(crunchDiffMeans, t(crunchDiffDists))

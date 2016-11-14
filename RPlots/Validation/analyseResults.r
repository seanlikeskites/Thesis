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

# distances
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

# confusion matrices
brightCounts <- table(c(harsh[,"bright"], crunch[,"bright"]))
crunchCounts <- table(crunch[,"crunch"])
harshCounts <- table(c(harsh[,"harsh"], crunch[,"harsh"]))
warmCounts <- table(harsh[,"warm"])

confusion <- matrix(0, 4, length(descriptors), dimnames=list(c("bright", "crunch", "harsh", "warm"), descriptors))
confusion["bright", rownames(brightCounts)] <- brightCounts
confusion["crunch", rownames(crunchCounts)] <- crunchCounts
confusion["harsh", rownames(harshCounts)] <- harshCounts
confusion["warm", rownames(warmCounts)] <- warmCounts

library(gplots)
library(extrafont)
colMap <- colorRampPalette(c(rgb(0.96, 0.96, 1), rgb(0.1, 0.1, 0.9)), space="rgb", bias=1)
pdf("Confusion.pdf", pointsize=12, family="CM Sans", width=6, height=5)
heatmap.2(confusion, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=confusion, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=1, 
	  lhei=c(0.4, 0.6), mar=c(3.5, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80))
dev.off()
embed_fonts("Confusion.pdf")


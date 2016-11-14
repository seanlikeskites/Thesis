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

harshDend <- as.dendrogram(hclust(dist(t(harshConfusion)), method="ward.D2"))
pdf("HarshConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=4)
heatmap.2(harshConfusion, Colv=harshDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=harshConfusion, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=1, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80))
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

crunchDend <- as.dendrogram(hclust(dist(t(crunchConfusion)), method="ward.D2"))
pdf("CrunchConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=4)
heatmap.2(crunchConfusion, Colv=crunchDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=crunchConfusion, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=1, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80))
dev.off()
embed_fonts("CrunchConfusion.pdf")

# combined
combConfusion <- matrix(0, 4, length(descriptors), dimnames=list(c("bright", "crunch", "harsh", "warm"), descriptors))
combConfusion["harsh",] <- harshConfusion["harsh",] + crunchConfusion["harsh",]
combConfusion["bright",] <- harshConfusion["bright",] + crunchConfusion["bright",]
combConfusion["warm",] <- harshConfusion["warm",]
combConfusion["crunch",] <- crunchConfusion["crunch",]

combDend <- as.dendrogram(hclust(dist(t(combConfusion)), method="ward.D2"))
pdf("CombinedConfusion.pdf", pointsize=12, family="CM Sans", width=6, height=5)
heatmap.2(combConfusion, Colv=combDend, Rowv=NA, trace="none", col=colMap, dendrogram="column", key=FALSE,
	  cellnote=combConfusion, notecol="black", cexRow=1, cexCol=1, lwid=c(0.1, 100), notecex=1, 
	  lhei=c(0.4, 0.6), mar=c(3.6, 3.5), breaks=c(0, 4, 6, 10, 20, 30, 40, 50, 60, 70, 80))
dev.off()
embed_fonts("CombinedConfusion.pdf")

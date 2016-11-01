source("../SAFEAnalysis/stemming.r")
load("../SAFEAnalysis/ClusterDistances.RData")

getDistances <- function(descriptors, cols, distances)
{
	distDescriptors <- c("crunch", "fuzz", "cream", "rasp", "smooth")
	eqDescriptors <- c("clear", "air", "thin", "full", "boom", "box", "tin", "deep", "mud")
	sharedDescriptors <- c("warm", "bright", "harsh")

	outDists <- descriptors

	for (i in 1:nrow(descriptors))
	{
		for (j in 2:4)
		{
			term <- descriptors[i, j]
			target <- cols[j - 1]

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

	return(data.frame(outDists))
}

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
crunch <- trimws(as.matrix(crunch[order(crunch$V1),]))

harsh[,2:4] <- safeStem(harsh[,2:4])
crunch[,2:4] <- safeStem(crunch[,2:4])

harshProcDists <- getDistances(harsh, c("warm", "bright", "harsh"), combProcDist)
harshDiffDists <- getDistances(harsh, c("warm", "bright", "harsh"), combDiffDist)

crunchProcDists <- getDistances(crunch, c("harsh", "bright", "crunch"), combProcDist)
crunchDiffDists <- getDistances(crunch, c("harsh", "bright", "crunch"), combDiffDist)

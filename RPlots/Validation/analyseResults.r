source("../SAFEAnalysis/stemming.r")
load("../SAFEAnalysis/ClusterDistances.RData")

prependDescriptors <- function(descriptors, cols)
{
	distDescriptors <- c("crunch", "fuzz", "cream", "rasp", "smooth", "harsh")
	eqDescriptors <- c("clear", "air", "thin", "full", "boom", "box", "tin", "deep", "mud")
	sharedDescriptors <- c("warm", "bright")

	distIdx <- descriptors %in% distDescriptors
	descriptors[distIdx] <- paste("D:", descriptors[distIdx], sep="")

	eqIdx <- descriptors %in% eqDescriptors
	descriptors[eqIdx] <- paste("E:", descriptors[eqIdx], sep="")

	special <- descriptors[,cols]
	specialIdx <- special %in% sharedDescriptors
	special[specialIdx] <- paste("E:", special[specialIdx], sep="")
	descriptors[,cols] <- special

	sharedIdx <- descriptors %in% sharedDescriptors
	descriptors[sharedIdx] <- paste("D:", descriptors[sharedIdx], sep="")

	return(descriptors)
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

harsh <- prependDescriptors(harsh, 2)
crunch <- prependDescriptors(crunch, c())

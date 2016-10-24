source("../SAFEAnalysis/stemming.r")
load("../SAFEAnalysis/ClusterDistances.RData")

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

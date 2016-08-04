library(SAFER)
source("featureNames.r")

getFeatures <- function(con, plugin, features)
{
	descriptors <- dbGetQuery(con, paste("SELECT Descriptors FROM ", plugin,
					     "UserData WHERE ID != 1121 ORDER BY ID;", sep=""))[,1]
	descriptors <- trimws(tolower(descriptors))
	unproc <- as.matrix(dbGetQuery(con, paste("SELECT ", features, " FROM ", plugin, 
					          "AudioFeatureData WHERE ID != 1121 AND SignalState = 'Unprocessed' ",
					          "GROUP BY ID ORDER BY ID;", sep="")))
	proc <- as.matrix(dbGetQuery(con, paste("SELECT ", features, " FROM ", plugin, 
				                "AudioFeatureData WHERE ID != 1121 AND SignalState = 'Processed' ",
				                "GROUP BY ID ORDER BY ID;", sep="")))
	diff <- proc - unproc

	rownames(proc) <- descriptors
	colnames(proc) <- tidyFeatureNames(colnames(proc))
	rownames(diff) <- descriptors
	colnames(diff) <- tidyFeatureNames(colnames(diff))

	completes <- complete.cases(diff)

	out <- list()
	out$proc <- proc[completes,]
	out$diff <- diff[completes,]

	return(out)
}

# connect to the database
con <- connectToSAFE()

# get the feature names
featureList <- getFeatureList(con)
featureString <- paste("AVG(", paste(featureList, collapse="), AVG("), ")", sep="")

# get the feature values
dist <- getFeatures(con, "SAFEDistortion", featureString)
eq <- getFeatures(con, "SAFEEqualiser", featureString)

# disconnect from the database
dbDisconnect(con)

# save the results
distProc <- dist$proc;
distDiff <- dist$diff;
eqProc <- eq$proc;
eqDiff <- eq$diff;
save(distProc, distDiff, eqProc, eqDiff, file="SAFE_Data.RData")

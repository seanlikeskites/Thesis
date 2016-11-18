library(XML)
#harshData <- xmlToList(xmlParse("SAFEHarshData.xml"))
#crunchData <- xmlToList(xmlParse("SAFECrunchData.xml"))

instruments <- c("Bass1", "Bass2", "Flute", "Guitar1", "Guitar2", "Marimba", "Oboe",  "Saxophone", "Trumpet", "Violin")
nInstruments <- length(instruments)
features <- names(harshData[1]$SemanticData$UnprocessedAudioFeatures$Channel0$Frame0)
nFeatures <- length(features)

buildMeans <- function(data, descriptors)
{
	box <- matrix(0, nInstruments, nFeatures, dimnames=list(instruments, features))
	frame <- list(box, box)
	names(frame) <- c("Unprocessed", "Processed")
	means <- list(frame, frame, frame)
	names(means) <- descriptors

	for (entry in data)
	{
		instrument <- entry$.attrs["Descriptor0"]
		term <- entry$.attrs["Descriptor1"]

		unproc <- entry$UnprocessedAudioFeatures$Channel0
		unproc <- matrix(unlist(unproc), ncol=nFeatures, byrow=TRUE)
		print(mean(as.numeric(unproc[1,])))

		break
	}

	return(means)
}

harshMeans <- buildMeans(harshData, c("Warm", "Bright", "Harsh"))


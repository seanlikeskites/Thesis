safeStem <- function(words)
{
	for (i in 1:length(words))
	{
		words [i] <- sub("i$", "", wordStem(words [i]))

		if (words [i] == "brighter")
			words [i] = "bright"
		else if (words [i] == "mudd")
			words [i] = "mud"
		else if (words [i] == "tinn")
			words [i] = "tin"
	}

	return(words)
}

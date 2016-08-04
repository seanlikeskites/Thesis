tidyFeatureNames <- function(names)
{
	names <- gsub("_", " ", names)
	names <- gsub("AVG\\(", "", names)
	names <- gsub("\\)", "", names)

	names[names == "DataMean"] <- "Signal Mean"
	names[names == "Variance"] <- "Signal Variance"
	names[names == "Standard Deviation"] <- "Signal Standard Deviation"
	names[names == "Spectral Variance"] <- "Spectral Spread"
	names[names == "Irregularity J"] <- "Jensen Irregularity"
	names[names == "Irregularity K"] <- "Krimphoff Irregularity"
	names[names == "Fundamental"] <- "$f_{0}$"
	names[names == "Smoothness"] <- "Spectral Smoothness"
	names[names == "Peak Spectral Variance"] <- "Peak Spectral Spread"
	names[names == "Peak Irregularity J"] <- "Peak Jensen Irregularity"
	names[names == "Peak Irregularity K"] <- "Peak Krimphoff Irregularity"
	names[names == "Peak Tristimulus 1"] <- "First Peak Tristimulus"
	names[names == "Peak Tristimulus 2"] <- "Second Peak Tristimulus"
	names[names == "Peak Tristimulus 3"] <- "Third Peak Tristimulus"
	names[names == "Harmonic Spectral Variance"] <- "Harmonic Spectral Spread"
	names[names == "Harmonic Irregularity J"] <- "Harmonic Jensen Irregularity"
	names[names == "Harmonic Irregularity K"] <- "Harmonic Krimphoff Irregularity"
	names[names == "Harmonic Tristimulus 1"] <- "First Tristimulus"
	names[names == "Harmonic Tristimulus 2"] <- "Second Tristimulus"
	names[names == "Harmonic Tristimulus 3"] <- "Third Tristimulus"
	names[names == "Parity Ratio"] <- "Odd to Even Harmonic Ratio"

	return(names)
}

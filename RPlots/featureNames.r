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

plotFeatureNames <- function(names)
{
	out <- expression(names)
	out[names == "Signal Mean"] <- expression(mu)
	out[names == "Signal Variance"] <- expression(sigma^2)
	out[names == "Signal Standard Deviation"] <- expression(sigma)
	out[names == "RMS Amplitude"] <- expression(plain(RMS))
	out[names == "Zero Crossing Rate"] <- expression(plain(ZCR))
	out[names == "Spectral Centroid"] <- expression(mu[plain(s)])
	out[names == "Spectral Spread"] <- expression(sigma[plain(s)]^2)
	out[names == "Spectral Standard Deviation"] <- expression(sigma[plain(s)])
	out[names == "Spectral Skewness"] <- expression(gamma[plain(s)])
	out[names == "Spectral Kurtosis"] <- expression(kappa[plain(s)])
	out[names == "Jensen Irregularity"] <- expression(plain(JI))
	out[names == "Krimphoff Irregularity"] <- expression(plain(KI))
	out[names == "$f_{0}$"] <- expression(f[0])
	out[names == "Spectral Smoothness"] <- expression(plain(SSm))
	out[names == "Spectral Roll Off"] <- expression(plain(SRO))
	out[names == "Spectral Flatness"] <- expression(plain(SF))
	out[names == "Tonality"] <- expression(tau)
	out[names == "Spectral Crest"] <- expression(plain(SC))
	out[names == "Spectral Slope"] <- expression(plain(SSl))
	out[names == "Peak Spectral Centroid"] <- expression(mu[plain(p)])
	out[names == "Peak Spectral Spread"] <- expression(sigma[plain(p)]^2)
	out[names == "Peak Spectral Standard Deviation"] <- expression(sigma[plain(p)])
	out[names == "Peak Spectral Skewness"] <- expression(gamma[plain(p)])
	out[names == "Peak Spectral Kurtosis"] <- expression(kappa[plain(p)])
	out[names == "Peak Jensen Irregularity"] <- expression(plain(JI[p]))
	out[names == "Peak Krimphoff Irregularity"] <- expression(plain(KI[p]))
	out[names == "First Peak Tristimulus"] <- expression(T[plain(p1)])
	out[names == "Second Peak Tristimulus"] <- expression(T[plain(p2)])
	out[names == "Third Peak Tristimulus"] <- expression(T[plain(p3)])
	out[names == "Inharmonicity"] <- expression(I)

	return(out)
}

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
	out[names == "Harmonic Spectral Centroid"] <- expression(mu[plain(h)])
	out[names == "Harmonic Spectral Spread"] <- expression(sigma[plain(h)]^2)
	out[names == "Harmonic Spectral Standard Deviation"] <- expression(sigma[plain(h)])
	out[names == "Harmonic Spectral Skewness"] <- expression(gamma[plain(h)])
	out[names == "Harmonic Spectral Kurtosis"] <- expression(kappa[plain(h)])
	out[names == "Harmonic Jensen Irregularity"] <- expression(plain(JI[h]))
	out[names == "Harmonic Krimphoff Irregularity"] <- expression(plain(KI[h]))
	out[names == "First Tristimulus"] <- expression(T[1])
	out[names == "Second Tristimulus"] <- expression(T[2])
	out[names == "Third Tristimulus"] <- expression(T[3])
	out[names == "Noisiness"] <- expression(N)
	out[names == "Odd to Even Harmonic Ratio"] <- expression(plain(ROE))
	out[names == "Bark Coefficient 0"] <- expression(plain(Bark)[0])
	out[names == "Bark Coefficient 1"] <- expression(plain(Bark)[1])
	out[names == "Bark Coefficient 2"] <- expression(plain(Bark)[2])
	out[names == "Bark Coefficient 3"] <- expression(plain(Bark)[3])
	out[names == "Bark Coefficient 4"] <- expression(plain(Bark)[4])
	out[names == "Bark Coefficient 5"] <- expression(plain(Bark)[5])
	out[names == "Bark Coefficient 6"] <- expression(plain(Bark)[6])
	out[names == "Bark Coefficient 7"] <- expression(plain(Bark)[7])
	out[names == "Bark Coefficient 8"] <- expression(plain(Bark)[8])
	out[names == "Bark Coefficient 9"] <- expression(plain(Bark)[9])
	out[names == "Bark Coefficient 10"] <- expression(plain(Bark)[10])
	out[names == "Bark Coefficient 11"] <- expression(plain(Bark)[11])
	out[names == "Bark Coefficient 12"] <- expression(plain(Bark)[12])
	out[names == "Bark Coefficient 13"] <- expression(plain(Bark)[13])
	out[names == "Bark Coefficient 14"] <- expression(plain(Bark)[14])
	out[names == "Bark Coefficient 15"] <- expression(plain(Bark)[15])
	out[names == "Bark Coefficient 16"] <- expression(plain(Bark)[16])
	out[names == "Bark Coefficient 17"] <- expression(plain(Bark)[17])
	out[names == "Bark Coefficient 18"] <- expression(plain(Bark)[18])
	out[names == "Bark Coefficient 19"] <- expression(plain(Bark)[19])
	out[names == "Bark Coefficient 20"] <- expression(plain(Bark)[20])
	out[names == "Bark Coefficient 21"] <- expression(plain(Bark)[21])
	out[names == "Bark Coefficient 22"] <- expression(plain(Bark)[22])
	out[names == "Bark Coefficient 23"] <- expression(plain(Bark)[23])
	out[names == "Bark Coefficient 24"] <- expression(plain(Bark)[24])
	out[names == "MFCC 0"] <- expression(plain(MFCC)[0])
	out[names == "MFCC 1"] <- expression(plain(MFCC)[1])
	out[names == "MFCC 2"] <- expression(plain(MFCC)[2])
	out[names == "MFCC 3"] <- expression(plain(MFCC)[3])
	out[names == "MFCC 4"] <- expression(plain(MFCC)[4])
	out[names == "MFCC 5"] <- expression(plain(MFCC)[5])
	out[names == "MFCC 6"] <- expression(plain(MFCC)[6])
	out[names == "MFCC 7"] <- expression(plain(MFCC)[7])
	out[names == "MFCC 8"] <- expression(plain(MFCC)[8])
	out[names == "MFCC 9"] <- expression(plain(MFCC)[9])
	out[names == "MFCC 10"] <- expression(plain(MFCC)[10])
	out[names == "MFCC 11"] <- expression(plain(MFCC)[11])
	out[names == "MFCC 12"] <- expression(plain(MFCC)[12])

	return(out)
}

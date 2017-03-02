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
	out[names == "Signal Mean"] <- expression(italic(mu))
	out[names == "Signal Variance"] <- expression(italic(sigma)^2)
	out[names == "Signal Standard Deviation"] <- expression(italic(sigma))
	out[names == "RMS Amplitude"] <- expression(plain(RMS))
	out[names == "Zero Crossing Rate"] <- expression(plain(ZCR))
	out[names == "Spectral Centroid"] <- expression(italic(mu)[plain(s)])
	out[names == "Spectral Spread"] <- expression(italic(sigma)[plain(s)]^2)
	out[names == "Spectral Standard Deviation"] <- expression(italic(sigma)[plain(s)])
	out[names == "Spectral Skewness"] <- expression(italic(gamma)[plain(s)])
	out[names == "Spectral Kurtosis"] <- expression(italic(kappa)[plain(s)])
	out[names == "Jensen Irregularity"] <- expression(plain(JI))
	out[names == "Krimphoff Irregularity"] <- expression(plain(KI))
	out[names == "$f_{0}$"] <- expression(italic(f)[0])
	out[names == "Spectral Smoothness"] <- expression(plain(SSm))
	out[names == "Spectral Roll Off"] <- expression(plain(SRO))
	out[names == "Spectral Flatness"] <- expression(plain(SF))
	out[names == "Tonality"] <- expression(italic(tau))
	out[names == "Spectral Crest"] <- expression(plain(SC))
	out[names == "Spectral Slope"] <- expression(plain(SSl))
	out[names == "Peak Spectral Centroid"] <- expression(italic(mu)[plain(p)])
	out[names == "Peak Spectral Spread"] <- expression(italic(sigma)[plain(p)]^2)
	out[names == "Peak Spectral Standard Deviation"] <- expression(italic(sigma)[plain(p)])
	out[names == "Peak Spectral Skewness"] <- expression(italic(gamma)[plain(p)])
	out[names == "Peak Spectral Kurtosis"] <- expression(italic(kappa)[plain(p)])
	out[names == "Peak Jensen Irregularity"] <- expression(plain(JI[p]))
	out[names == "Peak Krimphoff Irregularity"] <- expression(plain(KI[p]))
	out[names == "First Peak Tristimulus"] <- expression(italic(T)[plain(p1)])
	out[names == "Second Peak Tristimulus"] <- expression(italic(T)[plain(p2)])
	out[names == "Third Peak Tristimulus"] <- expression(italic(T)[plain(p3)])
	out[names == "Inharmonicity"] <- expression(italic(I))
	out[names == "Harmonic Spectral Centroid"] <- expression(italic(mu)[plain(h)])
	out[names == "Harmonic Spectral Spread"] <- expression(italic(sigma)[plain(h)]^2)
	out[names == "Harmonic Spectral Standard Deviation"] <- expression(italic(sigma)[plain(h)])
	out[names == "Harmonic Spectral Skewness"] <- expression(italic(gamma)[plain(h)])
	out[names == "Harmonic Spectral Kurtosis"] <- expression(italic(kappa)[plain(h)])
	out[names == "Harmonic Jensen Irregularity"] <- expression(plain(JI[h]))
	out[names == "Harmonic Krimphoff Irregularity"] <- expression(plain(KI[h]))
	out[names == "First Tristimulus"] <- expression(italic(T)[1])
	out[names == "Second Tristimulus"] <- expression(italic(T)[2])
	out[names == "Third Tristimulus"] <- expression(italic(T)[3])
	out[names == "Noisiness"] <- expression(italic(N))
	out[names == "Odd to Even Harmonic Ratio"] <- expression(plain(OER))
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

latexFeatureNames <- function(names)
{
	out <- expression(names)
	out[names == "Signal Mean"] <- "$\\mu$"
	out[names == "Signal Variance"] <- "$\\sigma^{2}$"
	out[names == "Signal Standard Deviation"] <- "$\\sigma$"
	out[names == "RMS Amplitude"] <- "$\\mathrm{RMS}$"
	out[names == "Zero Crossing Rate"] <- "$\\mathrm{ZCR}$"
	out[names == "Spectral Centroid"] <- "$\\mu_{\\mathrm{s}}$"
	out[names == "Spectral Spread"] <- "$\\sigma_{\\mathrm{s}}^{2}$"
	out[names == "Spectral Standard Deviation"] <- "$\\sigma_{\\mathrm{s}}$"
	out[names == "Spectral Skewness"] <- "$\\gamma_{\\mathrm{s}}$"
	out[names == "Spectral Kurtosis"] <- "$\\kappa_{\\mathrm{s}}$"
	out[names == "Jensen Irregularity"] <- "$\\mathrm{JI}$"
	out[names == "Krimphoff Irregularity"] <- "$\\mathrm{KI}$"
	out[names == "Spectral Smoothness"] <- "$\\mathrm{SSm}$"
	out[names == "Spectral Roll Off"] <- "$\\mathrm{SRO}$"
	out[names == "Spectral Flatness"] <- "$\\mathrm{SF}$"
	out[names == "Tonality"] <- "$\\tau$"
	out[names == "Spectral Crest"] <- "$\\mathrm{SC}$"
	out[names == "Spectral Slope"] <- "$\\mathrm{SSl}$"
	out[names == "Peak Spectral Centroid"] <- "$\\mu_{\\mathrm{p}}$"
	out[names == "Peak Spectral Spread"] <- "$\\sigma_{\\mathrm{p}}^{2}$"
	out[names == "Peak Spectral Standard Deviation"] <- "$\\sigma_{\\mathrm{p}}$"
	out[names == "Peak Spectral Skewness"] <- "$\\gamma_{\\mathrm{p}}$"
	out[names == "Peak Spectral Kurtosis"] <- "$\\kappa_{\\mathrm{p}}$"
	out[names == "Peak Jensen Irregularity"] <- "$\\mathrm{JI_{p}}$"
	out[names == "Peak Krimphoff Irregularity"] <- "$\\mathrm{KI_{p}}$"
	out[names == "First Peak Tristimulus"] <- "$T_{\\mathrm{p}1}$"
	out[names == "Second Peak Tristimulus"] <- "$T_{\\mathrm{p}2}$"
	out[names == "Third Peak Tristimulus"] <- "$T_{\\mathrm{p}3}$"
	out[names == "Inharmonicity"] <- "$I$"
	out[names == "Harmonic Spectral Centroid"] <- "$\\mu_{\\mathrm{h}}$"
	out[names == "Harmonic Spectral Spread"] <- "$\\sigma_{\\mathrm{h}}^{2}$"
	out[names == "Harmonic Spectral Standard Deviation"] <- "$\\sigma_{\\mathrm{h}}$"
	out[names == "Harmonic Spectral Skewness"] <- "$\\gamma_{\\mathrm{h}}$"
	out[names == "Harmonic Spectral Kurtosis"] <- "$\\kappa_{\\mathrm{h}}$"
	out[names == "Harmonic Jensen Irregularity"] <- "$\\mathrm{JI_{h}}$"
	out[names == "Harmonic Krimphoff Irregularity"] <- "$\\mathrm{KI_{h}}$"
	out[names == "First Tristimulus"] <- "$T_{1}$"
	out[names == "Second Tristimulus"] <- "$T_{2}$"
	out[names == "Third Tristimulus"] <- "$T_{3}$"
	out[names == "Noisiness"] <- "$N$"
	out[names == "Odd to Even Harmonic Ratio"] <- "$\\mathrm{ROE}$"
	out[names == "Bark Coefficient 0"] <- "$\\mathrm{Bark}_{0}$"
	out[names == "Bark Coefficient 1"] <- "$\\mathrm{Bark}_{1}$"
	out[names == "Bark Coefficient 2"] <- "$\\mathrm{Bark}_{2}$"
	out[names == "Bark Coefficient 3"] <- "$\\mathrm{Bark}_{3}$"
	out[names == "Bark Coefficient 4"] <- "$\\mathrm{Bark}_{4}$"
	out[names == "Bark Coefficient 5"] <- "$\\mathrm{Bark}_{5}$"
	out[names == "Bark Coefficient 6"] <- "$\\mathrm{Bark}_{6}$"
	out[names == "Bark Coefficient 7"] <- "$\\mathrm{Bark}_{7}$"
	out[names == "Bark Coefficient 8"] <- "$\\mathrm{Bark}_{8}$"
	out[names == "Bark Coefficient 9"] <- "$\\mathrm{Bark}_{9}$"
	out[names == "Bark Coefficient 10"] <- "$\\mathrm{Bark}_{10}$"
	out[names == "Bark Coefficient 11"] <- "$\\mathrm{Bark}_{11}$"
	out[names == "Bark Coefficient 12"] <- "$\\mathrm{Bark}_{12}$"
	out[names == "Bark Coefficient 13"] <- "$\\mathrm{Bark}_{13}$"
	out[names == "Bark Coefficient 14"] <- "$\\mathrm{Bark}_{14}$"
	out[names == "Bark Coefficient 15"] <- "$\\mathrm{Bark}_{15}$"
	out[names == "Bark Coefficient 16"] <- "$\\mathrm{Bark}_{16}$"
	out[names == "Bark Coefficient 17"] <- "$\\mathrm{Bark}_{17}$"
	out[names == "Bark Coefficient 18"] <- "$\\mathrm{Bark}_{18}$"
	out[names == "Bark Coefficient 19"] <- "$\\mathrm{Bark}_{19}$"
	out[names == "Bark Coefficient 20"] <- "$\\mathrm{Bark}_{20}$"
	out[names == "Bark Coefficient 21"] <- "$\\mathrm{Bark}_{21}$"
	out[names == "Bark Coefficient 22"] <- "$\\mathrm{Bark}_{22}$"
	out[names == "Bark Coefficient 23"] <- "$\\mathrm{Bark}_{23}$"
	out[names == "Bark Coefficient 24"] <- "$\\mathrm{Bark}_{24}$"
	out[names == "MFCC 0"] <- "$\\mathrm{MFCC}_{0}$"
	out[names == "MFCC 1"] <- "$\\mathrm{MFCC}_{1}$"
	out[names == "MFCC 2"] <- "$\\mathrm{MFCC}_{2}$"
	out[names == "MFCC 3"] <- "$\\mathrm{MFCC}_{3}$"
	out[names == "MFCC 4"] <- "$\\mathrm{MFCC}_{4}$"
	out[names == "MFCC 5"] <- "$\\mathrm{MFCC}_{5}$"
	out[names == "MFCC 6"] <- "$\\mathrm{MFCC}_{6}$"
	out[names == "MFCC 7"] <- "$\\mathrm{MFCC}_{7}$"
	out[names == "MFCC 8"] <- "$\\mathrm{MFCC}_{8}$"
	out[names == "MFCC 9"] <- "$\\mathrm{MFCC}_{9}$"
	out[names == "MFCC 10"] <- "$\\mathrm{MFCC}_{10}$"
	out[names == "MFCC 11"] <- "$\\mathrm{MFCC}_{11}$"
	out[names == "MFCC 12"] <- "$\\mathrm{MFCC}_{12}$"

	return(out)
}

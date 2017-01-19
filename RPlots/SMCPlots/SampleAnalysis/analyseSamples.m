load("samples.mat")

windowSize = 4096;
stepSize = 1024;

[bassAmps, bassFreqs] = getHarmonicRelationships (bass, bassFs, bassF0, windowSize, stepSize);
bassAmpRatios = bassAmps ./ repmat(bassAmps(1, :), 9, 1);
bassFreqRatios = bassFreqs ./ repmat(bassFreqs(1, :), 9, 1);

[clarAmps, clarFreqs] = getHarmonicRelationships (clar, clarFs, clarF0, windowSize, stepSize);
clarAmpRatios = clarAmps ./ repmat(clarAmps(1, :), 9, 1);
clarFreqRatios = clarFreqs ./ repmat(clarFreqs(1, :), 9, 1);

[clarSTFT50Amps, clarSTFT50Freqs] = getHarmonicRelationships (clarSTFT50, clarFs, clarF0, windowSize, stepSize);
clarSTFT50AmpRatios = clarSTFT50Amps ./ repmat(clarSTFT50Amps(1, :), 9, 1);
clarSTFT50FreqRatios = clarSTFT50Freqs ./ repmat(clarSTFT50Freqs(1, :), 9, 1);

[clarSTFT100Amps, clarSTFT100Freqs] = getHarmonicRelationships (clarSTFT100, clarFs, clarF0, windowSize, stepSize);
clarSTFT100AmpRatios = clarSTFT100Amps ./ repmat(clarSTFT100Amps(1, :), 9, 1);
clarSTFT100FreqRatios = clarSTFT100Freqs ./ repmat(clarSTFT100Freqs(1, :), 9, 1);

[clarSTFT500Amps, clarSTFT500Freqs] = getHarmonicRelationships (clarSTFT500, clarFs, clarF0, windowSize, stepSize);
clarSTFT500AmpRatios = clarSTFT500Amps ./ repmat(clarSTFT500Amps(1, :), 9, 1);
clarSTFT500FreqRatios = clarSTFT500Freqs ./ repmat(clarSTFT500Freqs(1, :), 9, 1);

[pianAmps, pianFreqs] = getHarmonicRelationships (pian, pianFs, pianF0, windowSize, stepSize);
pianAmpRatios = pianAmps ./ repmat(pianAmps(1, :), 9, 1);
pianFreqRatios = pianFreqs ./ repmat(pianFreqs(1, :), 9, 1);

[synAmps, synFreqs] = getHarmonicRelationships (syn, synFs, synF0, windowSize, stepSize);
synAmpRatios = synAmps ./ repmat(synAmps(1, :), 9, 1);
synFreqRatios = synFreqs ./ repmat(synFreqs(1, :), 9, 1);

save("HarmonicRelationships.mat",
     "bassAmpRatios", "bassFreqRatios",
     "clarAmpRatios", "clarFreqRatios",
     "clarSTFT50AmpRatios", "clarSTFT50FreqRatios",
     "clarSTFT100AmpRatios", "clarSTFT100FreqRatios",
     "clarSTFT500AmpRatios", "clarSTFT500FreqRatios",
     "pianAmpRatios", "pianFreqRatios",
     "synAmpRatios", "synFreqRatios",
     "stepSize")

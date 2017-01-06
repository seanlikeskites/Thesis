load("samples.mat")

windowSize = 4096;
stepSize = 1024;

[bassAmps, bassFreqs] = getHarmonicRelationships (bass, bassFs, bassF0, windowSize, stepSize);
bassAmpRatios = bassAmps ./ repmat(bassAmps(1, :), 9, 1);
bassFreqRatios = bassFreqs ./ repmat(bassFreqs(1, :), 9, 1);

[clarAmps, clarFreqs] = getHarmonicRelationships (clar, clarFs, clarF0, windowSize, stepSize);
clarAmpRatios = clarAmps ./ repmat(clarAmps(1, :), 9, 1);
clarFreqRatios = clarFreqs ./ repmat(clarFreqs(1, :), 9, 1);

[pianAmps, pianFreqs] = getHarmonicRelationships (pian, pianFs, pianF0, windowSize, stepSize);
pianAmpRatios = pianAmps ./ repmat(pianAmps(1, :), 9, 1);
pianFreqRatios = pianFreqs ./ repmat(pianFreqs(1, :), 9, 1);

[synAmps, synFreqs] = getHarmonicRelationships (syn, synFs, synF0, windowSize, stepSize);
synAmpRatios = synAmps ./ repmat(synAmps(1, :), 9, 1);
synFreqRatios = synFreqs ./ repmat(synFreqs(1, :), 9, 1);

save("HarmonicRelationships.mat",
     "bassAmpRatios", "bassFreqRatios",
     "clarAmpRatios", "clarFreqRatios",
     "pianAmpRatios", "pianFreqRatios",
     "synAmpRatios", "synFreqRatios")

load("samples.mat")

windowSize = 4096;

[bassAmps, bassFreqs] = getHarmonicRelationships (bass, bassFs, bassF0, windowSize);
bassAmpRatios = bassAmps ./ repmat(bassAmps(1, :), 9, 1);

[clarAmps, clarFreqs] = getHarmonicRelationships (clar, clarFs, clarF0, windowSize);
clarAmpRatios = clarAmps ./ repmat(clarAmps(1, :), 9, 1);

[pianAmps, pianFreqs] = getHarmonicRelationships (pian, pianFs, pianF0, windowSize);
pianAmpRatios = pianAmps ./ repmat(pianAmps(1, :), 9, 1);

[synAmps, synFreqs] = getHarmonicRelationships (syn, synFs, synF0, windowSize);
synAmpRatios = synAmps ./ repmat(synAmps(1, :), 9, 1);

load("samples.mat")

[bassAmps, bassFreqs] = getHarmonicRelationships (bass, bassFs, bassF0);
[clarAmps, clarFreqs] = getHarmonicRelationships (clar, clarFs, clarF0);
[pianAmps, pianFreqs] = getHarmonicRelationships (pian, pianFs, pianF0);
[synAmps, synFreqs] = getHarmonicRelationships (syn, synFs, synF0);

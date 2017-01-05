function out = findPeaks(in)

firstDiff = centeredDiff(in);
secondDiff = centeredDiff(firstDiff);

idxs = zerocrossing(1:length(in), firstDiff);
rIdxs = round(idxs);
out = rIdxs(secondDiff(rIdxs) < 0);

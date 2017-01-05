function [amps, freqs] = getHarmonicRelationships(sig, fs, f0, window = 1024, step = 256)

nCoeffs = window/2;
spec = stft(sig, window, step, nCoeffs);
nFrames = size(spec)(2);

amps = zeros(9, nFrames);
freqs = zeros(9, nFrames);

binFreqs = (0:(nCoeffs - 1)) * fs / window;

for i = 1:nFrames
	frame = spec(:, i);
	peakIdxs = findPeaks(frame');
	partials = binFreqs(peakIdxs);
	harmonics = partials / f0;

	for j = 1:9
		proxs = abs(harmonics - j);
		[minProx, harmIdx] = min(proxs);

		if (length(peakIdxs) == 0) || (minProx > 0.4)
			freq = j * f0;
			amps(j, i) = frame(round(freq * window / fs));
			freqs(j, i) = freq;
		else
			amps(j, i) = frame(peakIdxs(harmIdx));
			freqs(j, i) = partials(harmIdx);
		end
	end
end

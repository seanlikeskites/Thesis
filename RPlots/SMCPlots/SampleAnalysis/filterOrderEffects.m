load("samples.mat")

o = 6;

% iir filter
q = 1 / sqrt(2);
n = 1 / tan (pi * bassF0 / bassFs);
n2 = n^2;
c1 = 1 / (1 + 1 / q * n + n2);

iirLPb = [c1, 2 * c1, c1];
iirLPa = [1, 2 * c1 * (1 - n2), c1 * (1 - 1 / q * n + n2)];

iirFiltered = filter(iirLPb, iirLPa, bass);
iirHilb = hilbert(iirFiltered);
iirHarm = abs(iirHilb) .* cos(o * arg(iirHilb));

% fir filter
t = -1000:1000;
w = bassF0 / bassFs;
firLPb = 2 * w * sinc (2 * w * t);

firFiltered = filter(firLPb, 1, bass);
firHilb = hilbert(firFiltered);
firHarm = abs(firHilb) .* cos(o * arg(firHilb));

% spectra
l = length(bass);
sl = floor(l / 2) + 1;

iirSpec = abs(fft(iirHarm)) / l;
iirSpec = iirSpec(1:sl);
iirSpec(2:end) *= 2;

firSpec = abs(fft(firHarm)) / l;
firSpec = firSpec(1:sl);
firSpec(2:end) *= 2;

freqs = (1:(l-1)) * bassFs / l;
freqs = freqs(1:sl);

save("FilterOrderSpectra.mat", "freqs", "iirSpec", "firSpec");

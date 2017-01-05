[bass, bassFs] = wavread("bass.wav");
bassF0 = 221.58714;

[clar, clarFs] = wavread("clarinet.wav");
clarF0 = 195.75648;

[pian, pianFs] = wavread("piano.wav");
pianF0 = 98.030766;

[syn, synFs] = wavread("synth.wav");
synF0 = 55.235178;

save("samples.mat", 
     "bass", "bassFs", "bassF0",
     "clar", "clarFs", "clarF0",
     "pian", "pianFs", "pianF0",
     "syn", "synFs", "synF0")

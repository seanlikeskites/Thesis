function out = centeredDiff(in)

prevs = [(2*in(1) - in(2)), in(1:end-1)];
nexts = [in(2:end), 2*in(end) - in(end-1)];

out = nexts - prevs;

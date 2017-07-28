results <- readMat("MushraResults.mat")
raws <- results$raw
raws <- raws[,2:10,]

lines <- character()
lines <- c(lines, "\\begin{tabular}{|c|c|c|c|}")
lines <- c(lines, "\t\\cline{2-4}")
lines <- c(lines, "\t\\multicolumn{1}{c|}{} & \\multicolumn{3}{c|}{\\bf{Length Comparison}} \\tabularnewline")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\bf{Method} & \\bf{50-100} & \\bf{100-500} & \\bf{50-500} \\tabularnewline")
lines <- c(lines, "\t\\hline")
lines <- c(lines, "\t\\hline")

methods <- c("Synthesis", "\\acrshort{ssba}", "\\acrshort{iap}")

for (i in 1:3)
{
	method <- methods[i]
	start <- (i - 1) * 3 + 1

	shortMed <- format(round(t.test(c(raws[,start,]), c(raws[,start+1,]), paired=TRUE)$p.value, 3), nsmall=3)
	medLong <- format(round(t.test(c(raws[,start+1,]), c(raws[,start+2,]), paired=TRUE)$p.value, 3), nsmall=3)
	shortLong <- format(round(t.test(c(raws[,start,]), c(raws[,start+2,]), paired=TRUE)$p.value, 3), nsmall=3)

	lines <- c(lines, paste("\t", method, " & ", shortMed, " & ", medLong, " & ", shortLong, " \\tabularnewline",
				sep=""))
	lines <- c(lines, "\t\\hline")
}

lines <- c(lines, "\t\\hline")
lines <- c(lines, "\\end{tabular}")
f <- file("SMCTTests.tex")
writeLines(lines, f)
close(f)

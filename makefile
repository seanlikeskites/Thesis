Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex $(shell find Chapter1 Chapter2 -type f)
	pdflatex Sean_Enderby_Thesis.tex
	bibtex Sean_Enderby_Thesis.aux
	pdflatex Sean_Enderby_Thesis.tex
	pdflatex Sean_Enderby_Thesis.tex
	pdflatex Sean_Enderby_Thesis.tex

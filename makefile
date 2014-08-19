Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex $(shell find Chapter1 Chapter2 -type f)
	@pdflatex Sean_Enderby_Thesis.tex 
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex Sean_Enderby_Thesis.aux 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex Sean_Enderby_Thesis.tex
	@pdflatex Sean_Enderby_Thesis.tex
	@pdflatex Sean_Enderby_Thesis.tex

clean:
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Cleaning Up \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@rm -rf *.pdf *.aux *.bbl *.blg *.brf *.log *.out *.Pages *.toc

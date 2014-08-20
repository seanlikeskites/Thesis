chapters: combined chapter1.pdf chapter2.pdf

chapter1.pdf: Chapter1/chapter1.tex $(shell find Chapter1/Images -type f)
	@pdflatex --jobname=chapter1 "\includeonly{Chapter1/chapter1}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex chapter1 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=chapter1 "\includeonly{Chapter1/chapter1}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter1 "\includeonly{Chapter1/chapter1}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter1 "\includeonly{Chapter1/chapter1}\input{Sean_Enderby_Thesis.tex}"

chapter2.pdf: Chapter2/chapter2.tex 
	@pdflatex --jobname=chapter2 "\includeonly{Chapter2/chapter2}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex chapter2 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=chapter2 "\includeonly{Chapter2/chapter2}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter2 "\includeonly{Chapter2/chapter2}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter2 "\includeonly{Chapter2/chapter2}\input{Sean_Enderby_Thesis.tex}"

combined: Sean_Enderby_Thesis.pdf

Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex Preamble.tex Bibliography.tex Chapter1/chapter1.tex $(shell find Chapter1/Images -type f)
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
	@rm -rf *.pdf *.aux *.bbl *.blg *.brf *.log *.out *.Pages *.toc Chapter*/*.aux

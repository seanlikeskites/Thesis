chapters: combined chapter1.pdf chapter2.pdf chapter3.pdf chapter4.pdf chapter5.pdf

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

chapter2.pdf: Chapter2/chapter2.tex $(shell find Chapter2/Images -type f)
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

chapter3.pdf: Chapter3/chapter3.tex $(shell find Chapter3/Images -type f)
	@pdflatex --jobname=chapter3 "\includeonly{Chapter3/chapter3}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex chapter3 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=chapter3 "\includeonly{Chapter3/chapter3}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter3 "\includeonly{Chapter3/chapter3}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter3 "\includeonly{Chapter3/chapter3}\input{Sean_Enderby_Thesis.tex}"

chapter4.pdf: Chapter4/chapter4.tex $(shell find Chapter4/Images -type f)
	@pdflatex --jobname=chapter4 "\includeonly{Chapter4/chapter4}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex chapter4 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=chapter4 "\includeonly{Chapter4/chapter4}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter4 "\includeonly{Chapter4/chapter4}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter4 "\includeonly{Chapter4/chapter4}\input{Sean_Enderby_Thesis.tex}"

chapter5.pdf: Chapter5/chapter5.tex $(shell find Chapter5/Images -type f)
	@pdflatex --jobname=chapter5 "\includeonly{Chapter5/chapter5}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex chapter5 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=chapter5 "\includeonly{Chapter5/chapter5}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter5 "\includeonly{Chapter5/chapter5}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=chapter5 "\includeonly{Chapter5/chapter5}\input{Sean_Enderby_Thesis.tex}"

combined: Sean_Enderby_Thesis.pdf

Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex Preamble.tex Bibliography.tex Chapter1/chapter1.tex Chapter2/chapter2.tex Chapter3/chapter3.tex Chapter4/chapter4.tex Chapter5/chapter5.tex bibl.bib
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

story: story.pdf

story.pdf: story.tex
	@pdflatex story.tex

contributions: contributions.pdf

contributions.pdf: contributions.tex
	@pdflatex contributions.tex

clean:
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Cleaning Up \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@rm -rf *.pdf *.aux *.bbl *.blg *.brf *.log *.out *.Pages *.toc Chapter*/*.aux

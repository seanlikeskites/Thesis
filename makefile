CHAPTERS = chapter1.pdf chapter2.pdf chapter3.pdf chapter4.pdf chapter5.pdf 

chapters: combined $(CHAPTERS)

combined: Sean_Enderby_Thesis.pdf

Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex Preamble.tex Bibliography.tex $(wildcard Images/*) $(wildcard chapter*/Images/*) $(wildcard chapter*/chapter*.tex) bibl.bib
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

.SECONDEXPANSION:
%.pdf: chapter*/%.tex $$(wildcard $$*/Images/*)
	@pdflatex --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}"
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Sorting References \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@bibtex $* 
	@echo
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Bunging it all Together \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo
	@pdflatex --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}"
	@pdflatex --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}"

clean:
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Cleaning Up \*\*
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@rm -rf *.pdf *.aux *.bbl *.blg *.brf *.log *.out *.Pages *.toc Chapter*/*.aux

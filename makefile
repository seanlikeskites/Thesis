CHAPTERS = chapter1.pdf chapter2.pdf chapter3.pdf chapter4.pdf chapter5.pdf chapter6.pdf chapter7.pdf chapter8.pdf

all: chapters combined $(CHAPTERS) story contributions proofs revisions

chapters: combined $(CHAPTERS)

combined: Sean_Enderby_Thesis.pdf

Sean_Enderby_Thesis.pdf: Sean_Enderby_Thesis.tex Preamble.tex Appendices.tex Postamble.tex Bibliography.tex $(wildcard Images/*) $(wildcard chapter*/Images/*) $(wildcard chapter*/Tables/*) $(wildcard chapter*/chapter*.tex) bibl.bib
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=nonstopmode -file-line-error Sean_Enderby_Thesis.tex | grep -A 2 ".*:[0-9]*:.*" | cat
	@bibtex Sean_Enderby_Thesis.aux > /dev/null
	@makeglossaries Sean_Enderby_Thesis > /dev/null
	@pdflatex -interaction=nonstopmode Sean_Enderby_Thesis.tex > /dev/null
	@pdflatex -interaction=nonstopmode Sean_Enderby_Thesis.tex > /dev/null
	@pdflatex -interaction=nonstopmode Sean_Enderby_Thesis.tex > /dev/null
	@echo

story: story.pdf

story.pdf: story.tex
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=batchmode story.tex
	@echo

contributions: contributions.pdf

contributions.pdf: contributions.tex
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=batchmode contributions.tex
	@echo

proofs: proofs.pdf

proofs.pdf: proofs.tex
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=batchmode proofs.tex
	@echo

revisions: Revisions.pdf

Revisions.pdf: Revisions.tex
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=batchmode Revisions.tex
	@echo

.SECONDEXPANSION:
%.pdf: chapter*/%.tex $$(wildcard $$*/Images/*) $$(wildcard $$*/Tables/*)
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Building $@
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@pdflatex -interaction=nonstopmode --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}" > /dev/null
	@bibtex $* > /dev/null
	@makeglossaries $* > /dev/null
	@pdflatex -interaction=nonstopmode --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}" > /dev/null
	@pdflatex -interaction=nonstopmode --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}" > /dev/null
	@pdflatex -interaction=nonstopmode --jobname=$* "\includeonly{$(<D)/$*}\input{Sean_Enderby_Thesis.tex}" > /dev/null
	@echo

clean:
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@echo \*\* Cleaning Up
	@echo \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
	@rm -rf *.pdf *.aux *.bbl *.blg *.brf *.log *.out *.Pages *.toc Chapter*/*.aux *.lof *.lot *.lod *.acn *.acr *.alg *.glg *.glo *.gls *.ist *.mlg *.mni *.mno 

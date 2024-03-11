targ = main
lhsFiles := $(wildcard sections/*.lhs)
genTexFiles := $(patsubst %.lhs,%.tex,$(lhsFiles))
otherTexFiles := $(filter-out $(genTexFiles), $(wildcard sections/*.tex))
latex = pdflatex --enable-write18 -shell-escape

default: $(targ).pdf

$(targ).pdf: $(genTexFiles) $(otherTexFiles) $(targ).tex
	$(latex) $(targ)
	bibtex $(targ)
	$(latex) $(targ)
	$(latex) $(targ)

sections/%.tex: sections/%.lhs
	ghc -fno-code $<
	lhs2TeX -o $@ $<

clean:
	latexmk -CA
	rm -f sections/*.{o,hi,bbl,ptb}
	rm -f $(genTexFiles)
	rm -f main.ptb


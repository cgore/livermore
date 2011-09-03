TARGETS=thesis abstract-only

.MAIN: all

.for TARGET in $(TARGETS)
all: $(TARGET).dvi $(TARGET).ps $(TARGET).pdf
.endfor

.SUFFIXES: .bbl .bib .dvi .idx .ind .latex .ps .pdf

.bib.bbl:
	latex $*.latex
	bibtex $*
	latex $*.latex
	latex $*.latex
.dvi.ps:
	dvips -o $@ $<
.idx.ind:
	latex $*.latex
	makeindex $<
	latex $*.latex
.latex.dvi: $*.ind
	latex $<
.ps.pdf:
	ps2pdf $<

clean:
	rm -f *.aux *.log *.dvi *.ps *.pdf

abstract-only.dvi: GoreThesis.sty abstract.latex
thesis.dvi: GoreThesis.sty abstract.latex thesis.bbl

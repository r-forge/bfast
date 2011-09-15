TEX_SOURCES=realtime realtime_gcb

all: $(TEX_SOURCES:=.dvi) clean

%.dvi: %.tex		
	texi2dvi $<

%.pdf: %.dvi
	dvipdfm $<

%.ps: %.dvi
	dvips $<

.PHONY: clean distclean

clean:
	rm -f *.aux *.bbl *.blg *.log *.nav *.out *.snm *.spl *.toc *.vrb
	rm -f *~
	rm -f Rplots.ps
	rm -f Rplots.pdf

distclean: clean
	rm -f $(TEX_SOURCES:=.dvi)
	rm -f $(TEX_SOURCES:=.pdf)

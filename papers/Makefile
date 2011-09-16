TEX_SOURCES=realtime_gcb

all: $(TEX_SOURCES:=.pdf) clean

%.pdf: %.tex		
	texi2dvi --pdf $<

%.ps: %.pdf
	ps2pdf $<

.PHONY: clean distclean

clean:
	rm -f *.aux *.bbl *.blg *.log *.nav *.out *.snm *.spl *.toc *.vrb
	rm -f *~
	rm -f Rplots.ps
	rm -f Rplots.pdf

distclean: clean
	rm -f $(TEX_SOURCES:=.dvi)
	rm -f $(TEX_SOURCES:=.pdf)
	rm -f $(TEX_SOURCES:=.ps)

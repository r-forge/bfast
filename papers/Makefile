TEX_SOURCES=realtime

all: $(TEX_SOURCES:=.pdf) clean

%.pdf: %.tex		
	texi2dvi $<
	dvipdfm $(TEX_SOURCES:=.dvi)

.PHONY: clean distclean

clean:
	rm -f *.aux *.bbl *.blg *.log *.nav *.out *.snm *.spl *.toc *.vrb
	rm -f *~
	rm -f Rplots.ps
	rm -f Rplots.pdf
	rm -f $(TEX_SOURCES:=.dvi)

distclean: clean
	rm -f $(TEX_SOURCES:=.pdf)

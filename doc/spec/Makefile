PREFIX          = ../..
WWW		= $(HOME)/vc/svn/omdoc.org/www
include $(PREFIX)/doc/Makefile.vars

# 	The name of your master tex file without ending .tex
TARGET		= spec# errata

# the macros from the OMDoc repository
OSTY.base	= #listings lstpatch lstomdoc ed draftstamp acronyms ded myref myindex 

BIB 		= ../bibs/kwarc.bib
RawTEXSRC	= $(shell ls *.tex)  $(shell ls projects/*/*.tex) macros.sty
TEXSRC 		= $(filter-out spec-errata.tex,$(RawTEXSRC))
IMG 		= $(shell ls figures/*.*)

all:	spec.pdf # errata.pdf

# the following only works for Michael Kohlhase's setup
install: all
	cp spec.pdf $(WWW)/pubs/omdoc1.3draft.pdf
	svn ci -m're-generated' $(WWW)/pubs/omdoc1.3draft.pdf
#	cp  errata.pdf $(WWW)/pubs/errata1.3draft.pdf
#	svn ci -m're-generated' $(WWW)/pubs/errata1.2.pdf

echo:	
	echo $(TEXINPUTS)

spec-errata.tex: $(TEXSRC)
	pdflatex spec

errata.pdf: spec-errata.tex errata.tex
	pdflatex errata.tex

include $(PREFIX)/doc/Makefile.in
include $(PREFIX)/lib/Makefile.clean.in


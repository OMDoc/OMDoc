STEXDIR         = ../stex
PART		= spec
TARGET		= main.tex trans.tex work.tex #errata
RawTEXSRC	= $(shell ls *.tex) 

LOCALLTXPATHS 	= --path=../macros
LOCALTEXINPUTS 	= ../macros

include $(STEXDIR)/make/Makefile.vars
include ../lib/Makefile.vars

all:	work.pdf main.pdf #errata.pdf

include $(STEXDIR)/make/Makefile.in
include ../lib/Makefile.in

echo: 
	echo $(TEXINPUTS)

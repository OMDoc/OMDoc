STEXDIR = ../stex
PART = book
TARGET		= main.tex #errata
RawTEXSRC	= $(shell ls *.tex)\
	          $(shell ls ../projects/*/*.tex)\
		  $(shell ls ../primer/*.tex)\
	 	  $(shell ls ../spec/*.tex)\
	 	  $(shell ls ../processing/*.tex)

LOCALLTXPATHS 	= --path=../macros
LOCALTEXINPUTS	= ../macros

include $(STEXDIR)/lib/Makefile.vars
include ../lib/Makefile.vars

all:	main.pdf #errata.pdf

include $(STEXDIR)/lib/Makefile.in
include ../lib/Makefile.in


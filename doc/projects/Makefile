STEXDIR         = ../stex
PART		= projects
TARGET		= main.tex #trans.tex work.tex #errata
RawTEXSRC	= $(shell ls *.tex) 

LOCALLTXPATHS 	= --path=../macros
LOCALTEXINPUTS	= ../macros

include $(STEXDIR)/lib/Makefile.vars
include ../lib/Makefile.vars

all:	main.pdf #errata.pdf

include $(STEXDIR)/lib/Makefile.in
include ../lib/Makefile.in

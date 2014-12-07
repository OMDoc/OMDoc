# omdocmode makefile (adapted from xslide)
# $Id: Makefile 4087 2003-06-20 14:50:02Z kohlhase $
# $Name$

# what emacs is called on your system
EMACS = emacs

# no csh please
SHELL = /bin/sh

# have to preload the files that define variables used by other files
PRELOADS =  -l font-lock  -l tempo -l easymenu\
            -l omdoc-config.el -l omdoc-timestamp.el\
	    -l omdoc-templates.el -l omdoc-font.el -l omdoc-process.el

# compile with noninteractive and relatively clean environment

BATCHFLAGS = -batch -q -no-site-file


OBJECTS = omdoc-font.elc omdoc-timestamp.elc omdoc-process.elc omdoc-templates.elc omdoc-mode.elc 

all:	$(OBJECTS)
clean: 	
	rm -f *.elc

%.elc:	%.el
	@echo compiling $<...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<


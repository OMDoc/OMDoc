# xslide makefile
# $Id: Makefile 4153 2003-09-01 06:59:37Z kohlhase $
# $Name$
all: 	xslide
test-result:
html:	
ps:	
pdf:
dvi:
mml:

# what emacs is called on your system
EMACS = emacs

# no csh please
SHELL = /bin/sh

# have to preload the files that define variables used by other files
PRELOADS =  -l font-lock -l sendmail \
	-l xslide-data.el -l xslide-abbrev.el -l xslide-font.el \
	-l xslide-process.el -l xslide.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

FILES = \
	changelog.txt \
	dot_emacs \
	makefile \
	README.TXT \
	xslide-abbrev.el \
	xslide-data.el \
	xslide-font.el \
	xslide-initial.xsl \
	xslide-process.el \
	xslide.el \

OBJECTS = \
	xslide-data.elc xslide-abbrev.elc xslide-process.elc \
	xslide-font.elc xslide.elc

xslide:	$(OBJECTS)

%.elc:	%.el
	@echo compiling $<...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<


xslide.zip:	$(FILES)
		@-rm -f $@
		@zip -q $@ $(FILES)

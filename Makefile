PREFIX		= .
TESTDIRS 	= examples #projects
MAKEDIRS 	= $(TESTDIRS) doc rnc xsl2
CLEANDIRS 	= $(MAKEDIRS) 
INSTALLDIRS	= dtd rnc

include $(PREFIX)/lib/Makefile.subdirs

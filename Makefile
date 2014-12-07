PREFIX		= .
TESTDIRS 	= examples #projects
MAKEDIRS 	= $(TESTDIRS) doc rnc xsl
CLEANDIRS 	= $(MAKEDIRS) 
INSTALLDIRS	= dtd rnc doc/spec

include $(PREFIX)/lib/Makefile.subdirs

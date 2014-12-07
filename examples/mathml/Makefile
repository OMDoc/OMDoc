PREFIX          = ../..
OMFILES 	= arith1.om.omdoc cmathml.om.omdoc
BUTFILES 	= $(OMFILES)
include $(PREFIX)/lib/Makefile.vars
TERMINATE	=  terminate='no'

all:		om html mml #ps
2:		mml2
include $(PREFIX)/lib/Makefile.in

om:		$(OMFILES) $(XSLDIR)/cmml2om.xsl
$(OMFILES): %.om.omdoc:	%.omdoc $(XSLDIR)/cmml2om.xsl
		$(XSLT) -o $@ $(firstword $<) $(XSLDIR)/cmml2om.xsl dtd='../../dtd/omdoc.dtd'


OMPREFIX        = $(HOME)/import/branches/omdoc
XSLDIR          = $(OMPREFIX)/xsl2

XSLT		= saxon
XMLPARSE 	= xmlnorm -V -s

CDFILES		= $(wildcard $(HOME)/import/mathweb/omdoc/examples/omstd/cd/*.ocd)
OMDOCFILES 	= $(patsubst %.ocd,%.omdoc,$(CDFILES))
OMDOCS_TESTED	= $(patsubst %.omdoc,.%.tested, $(OMDOCFILES))


all: 		$(OMDOCFILES) 

test:		$(OMDOCS_TESTED)

.%.tested:	%.omdoc
		$(XMLPARSE) $< && touch $@

test-clean:	
		-rm -f .*.tested

test-all:	test-clean test

clean:
	rm -f *.omdoc 

$(OMDOCFILES): 	%.omdoc: %.ocd ../sts/%.sts ../omdoc/%.omdoc $(XSLDIR)/cd2omdoc.xsl Makefile
		$(XSLT) $(firstword $<) $(XSLDIR)/cd2omdoc.xsl $@ update-from="$(OMPREFIX)/examples/omstd/omdoc/$@"




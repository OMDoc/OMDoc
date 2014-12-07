PREFIX          = ../../..
RNCDIR = $(PREFIX)/rnc
BINDIR = $(PREFIX)/bin
RNC		= $(shell ls $(RNCDIR)/*.rnc)
SOAP = $(shell ls *.soap)
RNGPARSE 	= xmllint --noout --relaxng $(RNCDIR)/mocksoap.rng
RNCPARSE	= $(BINDIR)/rnv -q $(RNCDIR)/mocksoap.rnc 

SOAP.rnc-tested = $(SOAP:%.soap=.%.rnc-tested)
SOAP.rng-tested = $(SOAP:%.soap=.%.rng-tested)

test-rnc: $(SOAP.rnc-tested)
test-rng: $(SOAP.rng-tested)

.%.rnc-tested:	%.soap $(RNC)
		$(RNCPARSE) $< > $@ && touch $@
.%.rng-tested:	%.soap $(RNG)
		$(RNGPARSE) $< > $@ && touch $@

clean: 
	rm -f *~
distclean: clean

make test:		 
	echo "NOTHING to test HERE, care about the SOAP!!!!!!!!!"


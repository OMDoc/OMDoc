BUT.rnc = 
TARGET.src = $(filter-out $(BUT.rnc), $(shell ls ../rnc/*-*.rnc) ../rnc/omdoc.rnc)
SRC.rnc	   = $(filter-out $(BUT.rnc), $(shell ls *.rnc)) 

PREFIX = ../..
JOMDOCDIR = $(PREFIX)/jomdoc

TARGET.dtd = $(TARGET.src:%.rnc=%.dtd)

INSTALL = $(shell ls *.dtd *.mod *.ent)

SAXON9.jar = $(JOMDOCDIR)/lib/saxon9.jar
SAXON = java -jar $(SAXON9.jar) 

omdoc.rng: ../rnc/omdoc.rng
	$(SAXON) -o jomdoc.rng ../rnc/omdoc.rng rngdtd.xsl

clean:		
		rm -f *~ 
distclean:	clean


# for the moment we do not install
#install:	$(INSTALL)
#	cp $(INSTALL) $(HOME)/vc/svn/mathweb.org/trunk/www/omdoc/dtd

$(TARGET.dtd): %.dtd: %.rnc 
		trang -I rnc -O dtd $< $@


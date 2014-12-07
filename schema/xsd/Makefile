RNCDIR = ../rnc
BUT.rnc = 
TARGET.src = $(filter-out $(BUT.rnc), $(shell ls $(RNCDIR)/*-*.rnc) $(RNCDIR)/omdoc.rnc)
SRC.rnc	   = $(filter-out $(BUT.rnc), $(shell ls $(RNCDIR)/*.rnc)) 

BUT.xsd = 
TARGET.xsd = $(filter-out $(BUT.xsd),$(TARGET.src:%.rnc=%.xsd))

all:	 #$(TARGET.xsd) 

clean:		
		rm -f *~ 
distclean:	clean

# for the moment we do not install
# install:	$(SRC.rnc) $(SRC.rng) 
# 	cp $(SRC.rnc) $(SRC.rng) $(HOME)/vc/svn/omdoc.org/www/rnc

test:

$(TARGET.xsd): %.xsd: %.rnc 
		trang -I rnc omdoc.rnc omdoc.xsd

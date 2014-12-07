RNCDIR = ../rnc
BUT.rnc = 
TARGET.src = $(filter-out $(BUT.rnc), $(shell ls $(RNCDIR)/*-*.rnc) $(RNCDIR)/omdoc.rnc)
SRC.rnc	   = $(filter-out $(BUT.rnc), $(shell ls $(RNCDIR)/*.rnc)) 

SRC.rng = $(SRC.rnc:%.rnc=%.rng)

all:		$(SRC.rng) #$(TARGET.xsd) 

clean:		
		rm -f *~ 
distclean:	clean

# for the moment we do not install
# install:	$(SRC.rnc) $(SRC.rng) 
# 	cp $(SRC.rnc) $(SRC.rng) $(HOME)/vc/svn/omdoc.org/www/rnc

test:

$(SRC.rng): %.rng: %.rnc
#		rnc2rng $< $@
		trang -I rnc -O rng $< $@

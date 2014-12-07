RNCDIR = ../rnc
BUT.rnc = 
TARGET.rnc = $(RNCDIR)/omdoc.rnc
SRC.rnc	   = $(filter-out $(RNCDIR)/$(BUT.rnc), $(shell ls $(RNCDIR)/*.rnc)) 

SRC.rng = $(SRC.rnc:$(RNGDIR)/%.rnc=%.rng) 


all:		omdoc.rng

clean:		
		rm -f *~ 
distclean:	clean

# for the moment we do not install
# install:	$(SRC.rnc) $(SRC.rng) 
# 	cp $(SRC.rnc) $(SRC.rng) $(HOME)/vc/svn/omdoc.org/www/rnc

omdoc.rng: $(SRC.rnc)
		trang -I rnc -O rng $(TARGET.rnc) $@

echo: 
	echo $(SRC.rng)

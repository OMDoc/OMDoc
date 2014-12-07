PREFIX          = ../..
COLLFILES       = 
BUTFILES	= prefixed.omdoc

include $(PREFIX)/lib/Makefile.vars
include $(PREFIX)/lib/Makefile.test.vars

all:		pres
include $(PREFIX)/lib/Makefile.in
include $(PREFIX)/lib/Makefile.test.in

linktest: all.omdoc
	$(JOMDOC) validate --imports all.omdoc

#echo:	
#	echo $(NTNFILES)



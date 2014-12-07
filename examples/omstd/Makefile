PREFIX          = ../..
COLLFILES       = 
BUTFILES	= prefixed.omdoc

include $(PREFIX)/lib/Makefile.vars
include $(PREFIX)/lib/Makefile.test.vars

all:		pres
include $(PREFIX)/lib/Makefile.in
include $(PREFIX)/lib/Makefile.test.in

linktest: arith1.omdoc
	$(JOMDOC) validate --imports arith1.omdoc

MBUTDIRS = 
CIBUTDIRS =
DIRS		?= $(filter-out $(BUTDIRS), $(shell ls))
MAKEDIRS 	?= $(filter-out Makefile errors.log $(MBUTDIRS),$(DIRS))
CIDIRS 		?= $(filter-out Makefile errors.log $(CIBUTDIRS),$(DIRS))

CLEANDIRS 	?= $(MAKEDIRS)

all pdf install::
	@for d in $(MAKEDIRS);\
	  do (cd $$d && $(MAKE) -$(MAKEFLAGS) $@); done;

clean distclean::
	@for d in $(CLEANDIRS);\
	  do (cd $$d && $(MAKE) -$(MAKEFLAGS) $@); done;

ci:
	@for d in $(MAKEDIRS); do svn ci $$d -m're-generated'; done

cleanup:
	@for d in $(MAKEDIRS); do svn cleanup $$d; done

echo: 
	echo $(MAKEDIRS)


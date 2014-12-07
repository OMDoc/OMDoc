# copy from Makefile.vars.template and adapt
include Makefile.vars

MODULES = base.n3 doc.n3 omdoc.n3 pf.n3 st.n3 
INSTALL_TARGET = www

JENA_CLASSPATH	= $(JENA_LIB)/jena.jar:$(JENA_LIB)/antlr-2.7.5.jar:$(JENA_LIB)/commons-logging-api-1.1.jar:$(JENA_LIB)/xercesImpl.jar:$(JENA_LIB)/iri.jar:$(JENA_LIB)/icu4j_3_4.jar

# RDFCAT	= java -cp $(JENA_CLASSPATH) jena.rdfcat
OWLMERGE = java -cp $(OWLMERGE_CLASSPATH):$(JENA_CLASSPATH) info.kwarc.semweb.OWLMerge

# %.owl: %.n3
# 	$(RDFCAT) -out RDF/XML $< > $@ 

install:	$(MODULES)
	$(SUBVERSION) up $(INSTALL_TARGET)/
	$(OWLMERGE) omdoc.n3 > $(INSTALL_TARGET)/omdoc.owl	
	$(SUBVERSION) ci -m"regenerated" $(INSTALL_TARGET)/


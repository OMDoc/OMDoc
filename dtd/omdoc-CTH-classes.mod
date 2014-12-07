<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3): Module CTH Entities
     $Id: omdoc-CTH-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-CTH-classes.mod $
     PUBLIC: -//OMDoc//ENTITIES OMDoc CTH V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % fromto.attrib "%from.attrib; to %omdocref; #REQUIRED">  
<!-- attributes 'to' and 'from' are URIref -->

<!ENTITY % omdoccth.theory.extra "|%omdoccth.inclusion.qname;">
<!ENTITY % omdoccth.imports.mix ",(%omdoccth.morphism.qname;)?">
<!ENTITY % omdoccth.class "|%omdoccth.theory-inclusion.qname;
                           |%omdoccth.axiom-inclusion.qname;">

<!ENTITY % omdoccth.genvia.attrib "generated-via %omdocref; #IMPLIED">
<!ENTITY % omdoccth.imports.attribs 
	 "type (local|global) 'global'
          conservativity (conservative|monomorphism|definitional) #IMPLIED
	  conservativity-just %omdocref; #IMPLIED">

<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3): Module ST Entities
     $Id: omdoc-ST-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-ST-classes.mod $
     PUBLIC: -//OMDoc//ENTITIES OMDoc ST V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % theory-unique "CDATA"> <!-- we cannot check more -->

<!ENTITY % omdocst.scope.attrib 'scope (global|local) "global"'>
<!ENTITY % omdocst.sym.attrib "name %theory-unique; #IMPLIED %id.attrib; %omdocst.scope.attrib;">

<!ENTITY % otherdefinitiontype "">
<!ENTITY % definitiontype "(simple|implicit|inductive|obj|pattern|informal
                           %otherdefinitiontype;)">

<!-- the non-constitutive statements need a theory attribute -->
<!ENTITY % omdocst.theory.attrib "theory %omdocref; #IMPLIED">

<!ENTITY % omdocst.constitutive.class
           "|%omdocst.symbol.qname;
            |%omdocst.axiom.qname;
            |%omdocst.definition.qname;
            |%omdocst.imports.qname;">


<!ENTITY % omdocst.nonconstitutive.class
           "|%omdocst.assertion.qname;
            |%omdocst.type.qname;
            |%omdocst.alternative.qname;
            |%omdocst.example.qname;
	    |%omdocst.theory.qname;">

<!ENTITY % omdocst.class "%omdocst.nonconstitutive.class;
                          %omdocst.constitutive.class;">
<!ENTITY % omdocst.group.mix "|%omdocst.tgroup.qname;">




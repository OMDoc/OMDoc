<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module ST
     $Id: omdoc-ST-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-ST-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc ST V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % omdocst.other.sym.role "">
<!ENTITY % omdocst.omdoc.sym.role "type|sort|object">
<!ENTITY % omdocst.om.sym.role "binder|attribution|semantic-attribution|error">
<!ENTITY % omdocst.sym.role
           "%omdocst.omdoc.sym.role;|%omdocst.om.sym.role;%omdocst.other.sym.role;">
<!ENTITY % justby.attrib "just-by %omdocref; #IMPLIED">           
<!ENTITY % omdocst.constitutive.attribs "%id.attrib;
     				         generated-from %omdocref; #IMPLIED 
				         %omdoccth.genvia.attrib;">

<!ELEMENT %omdocst.symbol.qname; (%omdocdoc.meta.content;(%ss;|%omdocst.type.qname;)*)>
<!ATTLIST %omdocst.symbol.qname; 
          %omdoc.common.attribs;
	  generated-from %omdocref; #IMPLIED 
          %omdocst.sym.attrib;
          role (%omdocst.sym.role;) #IMPLIED>

<!ELEMENT %omdocst.axiom.qname; (%omdocmtxt.MCFS.content;)>
<!ATTLIST %omdocst.axiom.qname; 
          %omdoc.common.attribs;
          %omdocst.constitutive.attribs; 
          %fori.attrib;
	  type CDATA #IMPLIED>

<!ENTITY % def.attribs "existence %omdocref; #IMPLIED
			uniqueness %omdocref; #IMPLIED
			consistency %omdocref; #IMPLIED
			exhaustivity %omdocref; #IMPLIED
			coherence %omdocref; #IMPLIED">

<!-- Definitions contain CMPs,  FMPs and concept specifications. 
     The latter define the set of concepts defined in this element. 
     They can be reached under this name in the content dictionary 
     of the name specified in the theory attribute of the definition. -->
<!ELEMENT %omdocst.definition.qname; (%omdocmtxt.MC.content;,
           ((%omdocmtxt.FMP.qname;)*|(%omdocst.requation.qname;)+|(%omdocmobj.class;))?,
           (%omdocst.measure.qname;)?,(%omdocst.ordering.qname;)?)>
<!ATTLIST %omdocst.definition.qname;  
          %omdoc.common.attribs;
          %omdocst.constitutive.attribs;
	  %def.attribs;
          type  %definitiontype; #IMPLIED
          generated-by %omdocref; #IMPLIED
          %for.attrib;>
 <!-- attribute just-by is an URIref points to an assertion -->

<!ELEMENT %omdocst.requation.qname; 
          ((%omdocmobj.class;),(%omdocmobj.class;))>
<!ATTLIST %omdocst.requation.qname; 
          %omdoc.common.attribs;
          %id.attrib;>

<!ELEMENT %omdocst.measure.qname; (%omdocmobj.class;)>
<!ATTLIST %omdocst.measure.qname; 
          %omdoc.common.attribs;
          %id.attrib;>

<!ELEMENT %omdocst.ordering.qname; (%omdocmobj.class;)>
<!ATTLIST %omdocst.ordering.qname; 
          %omdoc.common.attribs;
	  terminating %omdocref; #IMPLIED
          %id.attrib;>

<!ENTITY % ded.status.other "">
<!ENTITY % ded.status.class "satisfiable|counter-satisfiable|no-consequence|
		   	                       theorem|conter-theorem|contradictory-axioms| 
					       tautologous-conclusion|tautology|equivalent| 
		   			       conunter-equivalent|unsatisfiable-conclusion| 
		   			       unsatisfiable%ded.status.other;">

<!ELEMENT %omdocst.assertion.qname; (%omdocmtxt.MCFS.content;)>
<!ATTLIST %omdocst.assertion.qname;  
          %omdoc.common.attribs;
          %omdoc.toplevel.attribs;
          type (%assertiontype;) #IMPLIED
          status (%ded.status.class;) #IMPLIED
          just-by %omdocrefs; #IMPLIED>
<!-- the %assertiontype; has no formal meaning yet, it is solely 
     for human consumption. The 'generated-by' is for 
     theory-interpretations, which can  generate assertions. 
     'just-by' is a list of URIRefs that point to justifications of the proclaimed status 
   -->

<!ELEMENT %omdocst.type.qname; (%omdocmtxt.MC.content;,(%omdocmobj.class;),(%omdocmobj.class;)?)>
<!ATTLIST %omdocst.type.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %justby.attrib;
          system %omdocref; #IMPLIED>
     
<!ELEMENT %omdocst.alternative.qname; (%omdocmtxt.MCF.content;,
           ((%omdocst.requation.qname;)+|(%omdocmobj.class;))?)>
<!ATTLIST %omdocst.alternative.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %for.attrib;
          type  %definitiontype; #IMPLIED
          generated-by %omdocref; #IMPLIED
	  %def.attribs;
          entails %omdocref; #IMPLIED
          entailed-by %omdocref; #IMPLIED
          entailed-by-thm %omdocref; #IMPLIED
          entails-thm %omdocref; #IMPLIED>      
<!-- entailed-by, entails, point to other (equivalent definitions
     entailed-by-thm, entails-thm point to the theorems justifying
     the entailment relation -->

<!ELEMENT %omdocst.example.qname; 
          (%omdocmtxt.MC.content;,(%omdocmobj.class;)*)>
<!ATTLIST %omdocst.example.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          type (for|against) #IMPLIED
          assertion %omdocref; #IMPLIED 
          %for.attrib;>
<!-- attributes assertion is an URIref -->


<!ELEMENT %omdocst.theory.qname; 
      (%omdocdoc.meta.content;(%omdoc.intheory.class;)*)>
<!ATTLIST %omdocst.theory.qname; 
          %omdoc.common.attribs;
          %id.attrib;
	  cdbase CDATA #IMPLIED
	  cdurl %anyURI; #IMPLIED
	  cdreviewdate CDATA #IMPLIED
	  cdversion CDATA #IMPLIED
	  cdrevision CDATA #IMPLIED
	  cdstatus (official|experimental|private|obsolete) #IMPLIED>

<!ELEMENT %omdocst.imports.qname; 
          ((%omdocdoc.metadata.qname;)?%omdoccth.imports.mix;)>
<!ATTLIST %omdocst.imports.qname; 
          %omdoc.common.attribs;
          %id.attrib; 
          %from.attrib;
          %omdoccth.imports.attribs;>
<!-- hiding is a list of references to symbol ids -->

<!ENTITY % omdocst.tgroup.class
	        "%omdoc.class;%omdocst.constitutive.class;|%omdocst.tgroup.qname;">

<!-- grouping element for structuring theories like documents -->
<!ELEMENT %omdocst.tgroup.qname; 
	          (%omdocdoc.meta.content;(%omdocst.tgroup.class;)*)>
<!ATTLIST %omdocst.tgroup.qname; 
	  	%omdoc.common.attribs; 
            	%omdoc.toplevel.attribs;        
		%group.attribs;>

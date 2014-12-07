<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module DOC
     $Id: omdoc-DOC-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-DOC-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc DOC V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ELEMENT %omdocdoc.metadata.qname; %omdocdoc.metadata.content;>
<!ATTLIST %omdocdoc.metadata.qname; 
          %omdoc.common.attribs; 
          %id.attrib; 
          inherits %omdocref; #IMPLIED>

<!-- this element can be used in lieu of a comment, it is read 
     by the style sheet, (comments are not) and can therefore 
     be transformed by them -->
<!ELEMENT %omdocdoc.ignore.qname; ANY>
<!ATTLIST %omdocdoc.ignore.qname; 
          %omdoc.common.attribs; 
          type CDATA #IMPLIED
          comment CDATA #IMPLIED>

<!-- co-referencing  allows to use elements with an 
     'id' attribute multiple times -->
<!ELEMENT %omdocdoc.ref.qname; EMPTY>
<!ATTLIST %omdocdoc.ref.qname;
          %omdoc.common.attribs; 
          %id.attrib;
	  %xref.attrib;
  	  type NMTOKEN #IMPLIED>
<!-- the types supported (there may be more over time) are 
     - 'include' (the default) for in-text replacement 
     - 'cite' for a reference with a generated label -->

<!-- grouping defines the structure of a document-->
<!ELEMENT %omdocdoc.omgroup.qname; (%omdocdoc.meta.content;(%omdoc.class;)*)>
<!ATTLIST %omdocdoc.omgroup.qname; 
	  	%omdoc.common.attribs; 
            	%omdoc.toplevel.attribs;        
		%group.attribs;>

<!-- finally the definition of the OMDoc root element -->
<!ELEMENT %omdoc.omdoc.qname; (%omdocdoc.meta.content;(%omdoc.class;)*)>
<!ATTLIST %omdoc.omdoc.qname; 
	         %omdoc.common.attribs; 
                 %omdoc.toplevel.attribs;        
	         %group.attribs; 
		 version CDATA #FIXED "1.3">


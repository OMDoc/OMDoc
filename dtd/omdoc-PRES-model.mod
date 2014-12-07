<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module PRES
     $Id: omdoc-PRES-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-PRES-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc PRES V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % crossreftype "(no|yes|brackets|separator|lbrack|rbrack|all)">
<!ENTITY % fixitytype "(prefix|infix|postfix|assoc|infixl|infixr)">

<!ENTITY % format.attrib "%id.attrib;
                          format CDATA #REQUIRED
                          requires %omdocref; #IMPLIED
                          %xml.lang.attrib;">

<!ENTITY % omdocpres.presentation.attrib
                      "for %omdocref; #REQUIRED
                       role (applied|binding|key) #IMPLIED
                       fixity %fixitytype; #IMPLIED
                       lbrack CDATA #IMPLIED
                       rbrack CDATA #IMPLIED
                       separator CDATA #IMPLIED
                       bracket-style (lisp|math) #IMPLIED
		       precedence NMTOKEN #IMPLIED
		       %xrefi.attrib;
		       crossref-symbol  %crossreftype; #IMPLIED">

<!ELEMENT %omdocpres.presentation.qname; 
          (%ss;|%omdocmtxt.CMP.qname;|%omdocpres.use.qname;|%omdocpres.xslt.qname;|%omdocpres.style.qname;)*>
<!ATTLIST %omdocpres.presentation.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
          %omdocpres.presentation.attrib;>

<!ENTITY % omdocpres.use.extra  "">
<!ENTITY % omdocpres.use.mix "%ss;|%omdocpres.element.qname;|%omdocpres.text.qname;
                             |%omdocpres.recurse.qname;|%omdocpres.value-of.qname;
		             |%omdocpres.map.qname;%omdocpres.use.extra;">

<!ELEMENT %omdocpres.use.qname; (#PCDATA|%omdocpres.use.mix;)*>
<!ATTLIST %omdocpres.use.qname; 
          %omdoc.common.attribs; 
          %format.attrib;
          bracket-style (lisp|math) #IMPLIED
          fixity %fixitytype; #IMPLIED
	  precedence NMTOKEN #IMPLIED
          lbrack CDATA #IMPLIED
          rbrack CDATA #IMPLIED
          separator CDATA #IMPLIED
          element CDATA #IMPLIED
          attributes CDATA #IMPLIED
	  crossref-symbol %crossreftype; #IMPLIED>
<!-- the attributes in the <use> element overwrite those in the 
     <presentation> element, therefore, they do not have defaults -->

<!ELEMENT %omdocpres.omstyle.qname; 
          (%ss;|%omdocpres.xslt.qname;|%omdocpres.style.qname;)*>
<!ATTLIST %omdocpres.omstyle.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
          for %omdocref; #IMPLIED
          element CDATA #REQUIRED>

<!ELEMENT %omdocpres.xslt.qname; %xslt.template;>
<!ATTLIST %omdocpres.xslt.qname; 
          %omdoc.common.attribs; 
          %format.attrib;>
<!-- this element contains xslt elements from the xsl: namespace -->

<!ELEMENT %omdocpres.style.qname; (%omdocpres.use.mix;)*>
<!ATTLIST %omdocpres.style.qname; 
          %omdoc.common.attribs; 
          %format.attrib;>
<!-- this element contains mock xslt expressed in the elements below -->

<!ELEMENT %omdocpres.element.qname; 
          (%omdocpres.attribute.qname;|%omdocpres.use.mix;)*>
<!ATTLIST %omdocpres.element.qname; 
          %omdoc.common.attribs; 
          name NMTOKEN #REQUIRED
          ns CDATA #IMPLIED
          cr (yes|no) 'yes'
	  crid CDATA #IMPLIED>

<!ELEMENT %omdocpres.map.qname; 
	  ((%omdocpres.separator.qname;)?,
          (%omdocpres.use.mix;)*)>
<!ATTLIST %omdocpres.map.qname; 
          %omdoc.common.attribs; 
          lbrack CDATA #IMPLIED
          rbrack CDATA #IMPLIED
          precedence CDATA #IMPLIED
          select CDATA #IMPLIED>

<!ELEMENT %omdocpres.separator.qname; (%omdocpres.use.mix;)*>
<!ATTLIST %omdocpres.separator.qname; 
          %omdoc.common.attribs;>

<!ELEMENT %omdocpres.attribute.qname; 
          (%ss;|%omdocpres.value-of.qname;|%omdocpres.text.qname;)*>
<!ATTLIST %omdocpres.attribute.qname; 
          %omdoc.common.attribs; 
          name NMTOKEN #REQUIRED
          select CDATA #IMPLIED
          ns %anyURI; #IMPLIED>

<!ELEMENT %omdocpres.text.qname; (#PCDATA)>
<!ATTLIST %omdocpres.text.qname;
          %omdoc.common.attribs;>

<!ELEMENT %omdocpres.value-of.qname; EMPTY>
<!ATTLIST %omdocpres.value-of.qname; 
          %omdoc.common.attribs; 
          select CDATA #REQUIRED>

<!ELEMENT %omdocpres.recurse.qname; EMPTY>
<!ATTLIST %omdocpres.recurse.qname; 
          %omdoc.common.attribs; 
          select CDATA #IMPLIED>

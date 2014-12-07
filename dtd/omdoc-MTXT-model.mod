<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module MTXT
     $Id: omdoc-MTXT-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-MTXT-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc MTXT V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->


<!-- the attributes for parallel markup -->
<!ENTITY % verbalizes.attrib "verbalizes %omdocrefs; #IMPLIED">
<!ENTITY % parallel.attribs "%verbalizes.attrib; index NMTOKEN #IMPLIED">

<!ELEMENT %omdocmtxt.omtext.qname; 
          (%omdocdoc.meta.content;(%ss;|%omdocmtxt.CMP.qname;)+,
            (%omdocmtxt.FMP.qname;)*)>
<!ATTLIST %omdocmtxt.omtext.qname; 
          %omdoc.common.attribs;
          %omdoc.toplevel.attribs; 
	  %verbalizes.attrib;
          type (%omtexttype;) #IMPLIED
          for %omdocref; #IMPLIED  
          from %omdocref; #IMPLIED>  
<!-- attribute 'for' is a URIref, to %omdocdoc.class;s 
     it is needed by the 'type' attribute-->

<!ENTITY % omdoc.CMP.content "%omdoc.mtext.content;">
<!ELEMENT %omdocmtxt.CMP.qname; (%omdoc.CMP.content;)*>
<!ATTLIST %omdocmtxt.CMP.qname; 
	  %id.attrib;
          %omdoc.common.attribs; 
          %xml.lang.attrib;>

<!ELEMENT %omdocmtxt.phrase.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocmtxt.phrase.qname; 
          %omdoc.common.attribs; %id.attrib; 
	  %parallel.attribs;			 
	  type CDATA #IMPLIED>
<!-- identifies a text passage and 
     allows to attatch style and role information to it -->

<!ELEMENT %omdocmtxt.term.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocmtxt.term.qname; 
          %omdoc.common.attribs;
	  %id.attrib;
	  role CDATA #IMPLIED
          name CDATA #REQUIRED 
          cd CDATA #REQUIRED>

<!ELEMENT %omdocmtxt.FMP.qname; 
          (((%omdocmtxt.assumption.qname;)*,(%omdocmtxt.conclusion.qname;)*)
           |(%omdocmobj.class;))> 
<!ATTLIST %omdocmtxt.FMP.qname; 
          %omdoc.common.attribs; 
	  %id.attrib;
          logic NMTOKEN #IMPLIED>

<!-- If FMP contains a %omdocmobj.class; then this is the assertion, 
     if it contains (assumption*,conclusion*), then it is a 
     logical sequent (A1,...,An |- C1,...,Cm): 
     all the Ai entail one of the Ci -->

<!ELEMENT %omdocmtxt.assumption.qname; (%omdocmobj.class;)>
<!ATTLIST %omdocmtxt.assumption.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
          inductive (yes|no) #IMPLIED>

<!ELEMENT %omdocmtxt.conclusion.qname; (%omdocmobj.class;)>
<!ATTLIST %omdocmtxt.conclusion.qname; 
          %omdoc.common.attribs; 
          %id.attrib;>

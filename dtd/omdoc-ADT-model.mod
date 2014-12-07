<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module ADT
     $Id: omdoc-ADT-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-ADT-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc ADT V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->


<!-- adts are abstract data types, they are short forms for 
      groups of symbols and their definitions, therefore, 
      they have much the same attributes. -->

<!ELEMENT %omdocadt.adt.qname; 
          (%omdocdoc.meta.content;(%ss;|%omdocadt.sortdef.qname;)+)>
<!ATTLIST %omdocadt.adt.qname; %omdoc.toplevel.attribs;
			       parameters CDATA #IMPLIED>

<!ELEMENT %omdocadt.sortdef.qname; 
          (%omdocdoc.meta.content;
           (%ss;|%omdocadt.constructor.qname;|%omdocadt.insort.qname;)*,
           (%omdocadt.recognizer.qname;)?)>
<!ATTLIST %omdocadt.sortdef.qname; 
          %omdoc.common.attribs; 
          %omdocst.sym.attrib;
          type (loose|generated|free)  #IMPLIED
          role NMTOKEN #FIXED "sort">

<!ELEMENT %omdocadt.insort.qname; EMPTY>
<!ATTLIST %omdocadt.insort.qname; 
          %omdoc.common.attribs;
          for %omdocref; #REQUIRED>
<!-- for is a reference to a sort symbol element  -->

<!ELEMENT %omdocadt.constructor.qname; 
    (%omdocdoc.meta.content;(%ss;|%omdocadt.argument.qname;)*)>
<!ATTLIST %omdocadt.constructor.qname; 
          %omdoc.common.attribs; 
          %omdocst.sym.attrib;
          role (type|sort|object) #FIXED "object">

<!ELEMENT %omdocadt.recognizer.qname; (%ss;|%omdocdoc.metadata.qname;)?>
<!ATTLIST %omdocadt.recognizer.qname; 
          %omdoc.common.attribs; 
          %omdocst.sym.attrib;
          role (type|sort|object) #FIXED "object">

<!ELEMENT %omdocadt.argument.qname; (%ss;|(%omdocst.type.qname;,(%omdocadt.selector.qname;)?))>
<!ATTLIST %omdocadt.argument.qname; 
          %omdoc.common.attribs;>

<!ELEMENT %omdocadt.selector.qname; (%omdocdoc.metadata.qname;)?>
<!ATTLIST %omdocadt.selector.qname; 
          %omdoc.common.attribs; 
          %omdocst.sym.attrib;
          role (type|sort|object) #FIXED "object"
          total (yes|no) #IMPLIED>


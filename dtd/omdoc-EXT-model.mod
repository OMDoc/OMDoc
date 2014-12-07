<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module EXT
     $Id: omdoc-EXT-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-EXT-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc EXT V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % omdocext.private.attrib "%fori.attrib; requires %omdocref; #IMPLIED">

<!ENTITY % omdocext.private.content 
           "%omdocdoc.meta.content;(%ss;|%omdocext.data.qname;)+">
<!ELEMENT %omdocext.private.qname; (%omdocext.private.content;)>
<!ATTLIST %omdocext.private.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %omdocext.private.attrib;
          reformulates %omdocref; #IMPLIED>
<!-- 'replaces is a URIref to the omdoc elements that are replaced by the 
     system-specific information in this element -->

<!ENTITY % omdocext.code.content
           "%omdocext.private.content;,
            (%omdocext.input.qname;)?,
            (%omdocext.output.qname;)?,
            (%omdocext.effect.qname;)?">

<!ELEMENT %omdocext.code.qname; (%omdocext.code.content;)>
<!ATTLIST %omdocext.code.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %omdocext.private.attrib;>

<!ELEMENT %omdocext.input.qname; (%omdocmtxt.MCF.content;)>
<!ATTLIST %omdocext.input.qname; 
          %omdoc.common.attribs; 
          %id.attrib;>

<!ELEMENT %omdocext.output.qname; (%omdocmtxt.MCF.content;)>
<!ATTLIST %omdocext.output.qname; 
          %omdoc.common.attribs; 
          %id.attrib;>

<!ELEMENT %omdocext.effect.qname; (%omdocmtxt.MCF.content;)>
<!ATTLIST %omdocext.effect.qname; 
          %omdoc.common.attribs; 
          %id.attrib;>

<!ENTITY % omdocext.data.attrib
              "format CDATA #IMPLIED
               href %anyURI; #IMPLIED
               size CDATA #IMPLIED
               pto CDATA #IMPLIED
               pto-version CDATA #IMPLIED
	       original (local|external) #IMPLIED">

<!ELEMENT %omdocext.data.qname; ANY>
<!ATTLIST %omdocext.data.qname; 
          %omdoc.common.attribs;  
          %omdocext.data.attrib;>

<!-- the content model is much too loose for my taste here -->
<!ELEMENT %omdocext.omlet.qname;  
	  (%omdoc.mtext.content;|%omdocext.param.qname;
	  |%omdocext.code.qname;|%omdocext.private.qname;)*>
<!ATTLIST %omdocext.omlet.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
	  action (display|execute|other) #IMPLIED
	  show (new|replace|embed|other) #IMPLIED
	  actuate (onPresent|onLoad|onRequest|other) #IMPLIED
          data %anyURI;  #IMPLIED>

<!ELEMENT %omdocext.param.qname;  (%omdocmobj.class;)*>
<!ATTLIST %omdocext.param.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
          name      CDATA             #REQUIRED
          value     CDATA             #IMPLIED
          valuetype (data|ref|object) #IMPLIED>

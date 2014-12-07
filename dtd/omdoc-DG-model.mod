<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module DG
     $Id: omdoc-DG-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-DG-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc DG V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ELEMENT %omdocdg.decomposition.qname; EMPTY>
<!ATTLIST %omdocdg.decomposition.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %fori.attrib;
          links %omdocrefs; #REQUIRED>            
<!-- attribute 'for' points to a 'theory-inclusion', which this 
     element justifies; attribute 'links' is an URIrefs, points to a 
     list of axiom-inlcusions and theory-inclusions -->

<!ELEMENT %omdocdg.path-just.qname; EMPTY>
<!ATTLIST %omdocdg.path-just.qname; 
          %omdoc.common.attribs; 
          local %omdocref; #REQUIRED     
          globals %omdocrefs; #REQUIRED  
	  %fori.attrib; 
          %id.attrib;>
<!-- attribute 'local' is an URIref, points to axiom-inclusion
               'globals' is an URIrefs, points to a list of 
                         theory-inclusions -->



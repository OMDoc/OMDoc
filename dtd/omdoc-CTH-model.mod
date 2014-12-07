<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module CTH
     $Id: omdoc-CTH-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-CTH-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc CTH V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ELEMENT %omdoccth.morphism.qname; ((%ss;|%omdocst.requation.qname;)*,
                                     (%omdocst.measure.qname;)?,
                                     (%omdocst.ordering.qname;)?)>
<!ATTLIST %omdoccth.morphism.qname; 
          %omdoc.common.attribs; 
          %id.attrib;
          %justby.attrib;
          type (recursive | pattern) #IMPLIED
          hiding %omdocrefs; #IMPLIED 
          base %omdocrefs; #IMPLIED> 
<!-- base points to some other morphism it extends -->

<!ELEMENT %omdoccth.inclusion.qname; EMPTY>
<!ATTLIST %omdoccth.inclusion.qname; 
          %omdoc.common.attribs; 
          via %omdocref; #REQUIRED 
          %id.attrib;>
<!-- via points to a theory-inclusion -->

<!ELEMENT %omdoccth.theory-inclusion.qname; 
          (%omdocdoc.meta.content;(%omdoccth.morphism.qname;)?,
            ((%ss;|%omdoccth.obligation.qname;)*%omdocdg.theory-inclusion.content.mix;))>
<!ATTLIST %omdoccth.theory-inclusion.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %fromto.attrib;>

<!ELEMENT %omdoccth.axiom-inclusion.qname; 
          (%omdocdoc.meta.content;(%omdoccth.morphism.qname;)?,
            ((%ss;|%omdoccth.obligation.qname;)*%omdocdg.axiom-inclusion.content.mix;))>
<!ATTLIST %omdoccth.axiom-inclusion.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %fromto.attrib;>

<!ELEMENT %omdoccth.obligation.qname; EMPTY>
<!ATTLIST %omdoccth.obligation.qname; 
          %omdoc.common.attribs; 
	  induced-by %omdocref; #REQUIRED
          assertion %omdocref; #REQUIRED  
          %id.attrib;>
<!-- attribute 'assertion' is a URIref, points to  an assertion
     that is the proof obligation induced by the axiom or definition 
     specified by 'induced-by. -->


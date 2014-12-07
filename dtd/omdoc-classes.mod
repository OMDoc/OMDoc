<!--
   An XML Document Type Definition for Open Mathematical documents (OMDoc 1.3)
   General OMDoc Class Definitions (for mixed content)
     $Id: omdoc-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-classes.mod $
     PUBLIC: -//OMDoc//CLASSES OMDoc V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!-- This module contains entity definitions for classes and attributes that 
     allow content models that are mixed between modules. 

     The first step is to define some attributes that are generally useful-->


<!-- The DTD does not allow a lot of string checking, 
     for documentation purposes, we nuse the following entities -->
<!ENTITY % omdocref "CDATA">   <!-- a URI pointing to an OMDoc fragment -->
<!ENTITY % omdocrefs "CDATA">  <!-- a whitespace-separated list of URIs 
                                    pointing to OMDoc fragments -->
<!ENTITY % anyURI "CDATA">     <!-- any URI -->
<!ENTITY % anyURIs "CDATA">    <!-- a whitespace-separated list of URIs -->
<!-- sometimes we do not define the id attribute to be of type ID as one 
     would expect,since we only want them to be unique in a theory, 
     and we want still to be able to concatenate OMDoc files -->
<!ENTITY % document-unique "ID"> <!-- we cannot check more -->


<!--    The current XML-recommendation doesn't yet support the 
        three-letter short names for languages (ISO 693-2). So 
        the following section will be using the two-letter 
        (ISO 693-1) encoding for the languages.

	en : English,	de : German,	fr : French,
	la : Latin, 	it : Italian, 	nl : Dutch,
	ru : Russian,	pl : Polish,	es : Spanish,
	tr : Turkish,	zh : Chinese,	ja : Japanese,
	ko : Korean     ...                     -->
<!ENTITY % ISO639 "(aa|ab|af|am|ar|as|ay|az|ba|be|bg|bh|bi|bn|bo|br|ca|co|
                    cs|cy|da|de|dz|el|en|eo|es|et|eu|fa|fi|fj|fo|fr|fy|ga|
                    gd|gl|gn|gu|ha|he|hi|hr|hu|hy|ia|ie|ik|id|is|it|iu|ja|
                    jv|ka|kk|kl|km|kn|ko|ks|ku|ky|la|ln|lo|lt|lv|mg|mi|mk|
                    ml|mn|mo|mr|ms|mt|my|na|ne|nl|no|oc|om|or|pa|pl|ps|pt|
                    qu|rm|rn|ro|ru|rw|sa|sd|sg|sh|si|sk|sl|sm|sn|so|sq|sr|
                    ss|st|su|sv|sw|ta|te|tg|th|ti|tk|tl|tn|to|tr|ts|tt|tw|
                    ug|uk|ur|uz|vi|vo|wo|xh|yi|yo|za|zh|zu)">

<!ENTITY % xml.lang.attrib "xml:lang %ISO639; 'en'">
<!-- we cannot check for whitespace-separated lists of ISO639 in DTDs -->
<!ENTITY % xml.langs.attrib "xml:lang CDATA">

<!-- the attributes for CSS and PRES styling -->
<!ENTITY % css.attrib "style CDATA #IMPLIED class NMTOKEN #IMPLIED">

<!ENTITY % xref.attrib "xref %omdocref; #REQUIRED"> 
<!ENTITY % xrefi.attrib "xref %omdocref; #IMPLIED"> 
<!ENTITY % otheridrest.attribs "">
<!ENTITY % idrest.attrib "%css.attrib;%otheridrest.attribs;">
<!ENTITY % id.attrib "xml:id %document-unique; #IMPLIED %idrest.attrib;">

<!ENTITY % for.attrib "for %omdocrefs; #REQUIRED">
<!ENTITY % fori.attrib "for %omdocrefs; #IMPLIED">
<!ENTITY % from.attrib	 "from %omdocref; #REQUIRED">

<!-- we first load the class definitions from the modules -->
<!-- we first load the OpenMath and MathML representations for mathematical -->
<!-- objects. This is a safe thing to do, since there is no inter-module -->
<!-- recursion. -->
<![%omdoc.cc.module;[
  <!ENTITY % omdoc-CC-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc CC V1.3//EN" "omdoc-CC-classes.mod">
  %omdoc-CC-classes.mod;]]>
<!ENTITY % omdoccc.class "">

<!ENTITY % omdoc-DC-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc DC V1.3//EN" "omdoc-DC-classes.mod">
%omdoc-DC-classes.mod;

<![%omdoc.rt.module;[
   <!ENTITY % omdoc-RT-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc RT V1.3//EN"
                                 "omdoc-RT-classes.mod">
   %omdoc-RT-classes.mod;]]>
<!ENTITY % omdocrt.mtext.mix "">

<![%omdoc.ext.module;[
   <!ENTITY % omdoc-EXT-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc EXT V1.3//EN"
                                  "omdoc-EXT-classes.mod">
   %omdoc-EXT-classes.mod;]]>
<!ENTITY % omdocext.mtext.mix "">
<!ENTITY % omdocext.class "">

<!ENTITY % omdoc-DOC-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc DOC V1.3//EN"
                                  "omdoc-DOC-classes.mod">
%omdoc-DOC-classes.mod;

<!ENTITY % omdoc-MOBJ-classes.mod PUBLIC "-//OMDoc//DTD OMDoc MOBJ V1.3//EN" "omdoc-MOBJ-classes.mod">
%omdoc-MOBJ-classes.mod;

<!ENTITY % omdocmtxt-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc MTXT V1.3//EN"
                                  "omdoc-MTXT-classes.mod">
%omdocmtxt-classes.mod;

<![%omdoc.st.module;[
   <!ENTITY % omdoc-ST-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc ST V1.3//EN"
                                 "omdoc-ST-classes.mod">
   %omdoc-ST-classes.mod;]]>
<!ENTITY % omdocst.constitutive.class "">
<!ENTITY % omdocst.nonconstitutive.class "">
<!ENTITY % omdocst.theory.attrib "">
<!ENTITY % omdocst.theory.class "">
<!ENTITY % omdocst.group.mix "">

<![%omdoc.adt.module;[
   <!ENTITY % omdoc-ADT-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc ADT V1.3//EN"
                                  "omdoc-ADT-classes.mod">
   %omdoc-ADT-classes.mod;]]>
<!ENTITY % omdocadt.constitutive.class "">

<![%omdoc.pres.module;[
   <!ENTITY % omdoc-PRES-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc PRES V1.3//EN"
                                   "omdoc-PRES-classes.mod">
   %omdoc-PRES-classes.mod;]]>
<!ENTITY % omdocpres.class "">
<!ENTITY % xslt.xmlns.attrib "">

<![%omdoc.pf.module;[
   <!ENTITY % omdoc-PF-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc PF V1.3//EN"
                                 "omdoc-PF-classes.mod">
   %omdoc-PF-classes.mod;]]>
<!ENTITY % omdocpf.class "">
<!ENTITY % omdocpf.opt.content "">

<![%omdoc.quiz.module;[
   <!ENTITY % omdoc-QUIZ-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc QUIZ V1.3//EN"
                                  "omdoc-QUIZ-classes.mod">
    %omdoc-QUIZ-classes.mod;]]>
<!ENTITY % omdocquiz.class "">

<![%omdoc.cth.module;[
   <!ENTITY % omdoc-CTH-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc CTH V1.3//EN"
                                  "omdoc-CTH-classes.mod">
  %omdoc-CTH-classes.mod;]]>
<!ENTITY % omdoccth.class "">
<!ENTITY % omdoccth.theory.extra "">
<!ENTITY % omdoccth.imports.mix "">
<!ENTITY % omdoccth.genvia.attrib "">
<!ENTITY % omdoccth.imports.attribs "">

<![%omdoc.dg.module;[
   <!ENTITY % omdoc-DG-classes.mod PUBLIC "-//OMDoc//CLASSES OMDoc DG V1.3//EN"
                                  "omdoc-DG-classes.mod">
  %omdoc-DG-classes.mod;]]>
<!ENTITY % omdocdg.class "">
<!ENTITY % omdocdg.theory-inclusion.content.mix "">
<!ENTITY % omdocdg.axiom-inclusion.content.mix "">



<!-- OMDoc now come the top-level classes that call the other's -->
<!ENTITY % omdoc.toplevel.intheory.class 
           "%omdocdoc.class;
            %omdocmtxt.class;
            %omdocst.nonconstitutive.class;
            %omdocpf.class;
            %omdocpres.class;
            %omdocext.class;
            %omdocquiz.class;
            %omdoc.extra.class;
            %omdoccth.theory.extra;">

<!ENTITY % omdoc.intheory.class 
           "%omdoc.toplevel.intheory.class;
	    %omdocst.constitutive.class;
	    %omdocadt.constitutive.class;
	    %omdocst.group.mix;">

<!ENTITY % omdoc.class "%omdoc.toplevel.intheory.class;
                        %omdoccth.class;
                        %omdocdg.class;">

<!-- GENERAL INFRASTRUCTURE -->
<!ENTITY % other.nsp.decl "">
<!ENTITY % nsp.decl "%omdoc.nsp.decl; 
	             %omdocdc.nsp.decl; 
                     %omdoccc.nsp.decl;  	          
		     %om.nsp.decl; 
		     %mathml.nsp.decl;
		     %xslt.xmlns.attrib;
		     %other.nsp.decl;">
<!-- ****   xmlns:cc CDATA #FIXED 'http://creativecommons.org/ns' -->

<!ENTITY % omdoc.ns.decl "xmlns %anyURI; #FIXED '%omdoc.xmlns;'">

<!-- only specify the schema attributes, if explicitly wanted -->
<![%omdoc.schema.attribs.enabled;[
<!ENTITY % omdoc.schema.attrib 
           "xmlns:xsi %anyURI; #FIXED 'http://www.w3.org/2001/XMLSchema-instance'
            xsi:schemaLocation %anyURI; #IMPLIED">]]>
<!ENTITY % omdoc.schema.attrib "">

<!ENTITY % omdoc.nsschema.attrib "%omdoc.ns.decl; %omdoc.schema.attrib;">

<!-- this namespace declaration also needs to go into all the elements 
     that do not inherit from the top-level omdoc elements 
     e.g. those in 'omdoc.CMP.content' -->

<!ENTITY % other.attribs "">
<!ENTITY % omdoc.common.attribs "%omdoc.ns.decl; %nsp.decl; 
	                         %omdoc.schema.attrib; 
				 %other.attribs;">
<!ENTITY % omdoc.toplevel.attribs "%id.attrib; 
	                           %omdocst.theory.attrib;
				   generated-from %omdocref; #IMPLIED 
				   %omdoccth.genvia.attrib;">


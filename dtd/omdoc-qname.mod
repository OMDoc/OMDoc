<!--
   An XML Document Type Definition for Open Mathematical documents (OMDoc 1.3)
   General OMDoc Entities
     $Id: omdoc-qname.mod 8464 2009-08-04 15:04:33Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-qname.mod $
     PUBLIC: -//OMDoc//ENTITIES OMDoc V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!-- set this to INCLUDE to use omdoc as a prefixed module -->
<!ENTITY % NS.prefixed "IGNORE">
<!ENTITY % omdoc.prefixed "%NS.prefixed;">
<!ENTITY % omdoc.xmlns "http://omdoc.org/ns">
<!ENTITY % omdoc.prefix "omdoc">

<!-- if omdoc.prefixed=INCLUDE, then use the prefix and delare the namespace -->
<![%omdoc.prefixed;[
   <!ENTITY % omdoc.pfx "%omdoc.prefix;:">
   <!ENTITY % omdoc.nsp.decl "xmlns:%omdoc.prefix; %anyURI; #FIXED '%omdoc.xmlns;'">]]>
<!ENTITY % omdoc.pfx "">
<!ENTITY % omdoc.nsp.decl "">

<!ENTITY % omdoc-DOC-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc DOC V1.3//EN"
                                  "omdoc-DOC-qname.mod">
%omdoc-DOC-qname.mod;

<!-- we first load the OpenMath and MathML representations for mathematical -->
<!-- objects. This is a safe thing to do, since there is no inter-module -->
<!-- recursion. -->
<!ENTITY % omdoc-MOBJ-qname.mod PUBLIC "-//OMDoc//DTD OMDoc MOBJ V1.3//EN" "omdoc-MOBJ-qname.mod">
%omdoc-MOBJ-qname.mod;

<!-- Then we load some entities, since we have inter-module recursion -->

<!ENTITY % omdoc-DC-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc DC V1.3//EN"
                                  "omdoc-DC-qname.mod">
%omdoc-DC-qname.mod;

<![%omdoc.cc.module;[
  <!ENTITY % omdoc-CC-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc CC V1.3//EN"
                                "omdoc-CC-qname.mod">
  %omdoc-CC-qname.mod;]]>

<!ENTITY % omdoc-MTXT-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc MTXT V1.3//EN"
                                  "omdoc-MTXT-qname.mod">
%omdoc-MTXT-qname.mod;

<![%omdoc.rt.module;[
   <!ENTITY % omdoc-RT-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc RT V1.3//EN"
                                 "omdoc-RT-qname.mod">
   %omdoc-RT-qname.mod;]]>

<![%omdoc.st.module;[
   <!ENTITY % omdoc-ST-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc ST V1.3//EN"
                                 "omdoc-ST-qname.mod">
   %omdoc-ST-qname.mod;]]>

<![%omdoc.adt.module;[
   <!ENTITY % omdoc-ADT-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc ADT V1.3//EN"
                                  "omdoc-ADT-qname.mod">
   %omdoc-ADT-qname.mod;]]>

<![%omdoc.pres.module;[
   <!ENTITY % omdoc-PRES-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc PRES V1.3//EN"
                                   "omdoc-PRES-qname.mod">
   %omdoc-PRES-qname.mod;]]>
<!ENTITY % xslt.xmlns.attrib "">

<![%omdoc.pf.module;[
   <!ENTITY % omdoc-PF-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc PF V1.3//EN"
                                 "omdoc-PF-qname.mod">
   %omdoc-PF-qname.mod;]]>

<![%omdoc.ext.module;[
   <!ENTITY % omdoc-EXT-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc EXT V1.3//EN"
                                  "omdoc-EXT-qname.mod">
   %omdoc-EXT-qname.mod;]]>

<![%omdoc.quiz.module;[
   <!ENTITY % omdoc-QUIZ-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc QUIZ V1.3//EN"
                                  "omdoc-QUIZ-qname.mod">
    %omdoc-QUIZ-qname.mod;]]>

<![%omdoc.cth.module;[
   <!ENTITY % omdoc-CTH-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc CTH V1.3//EN"
                                  "omdoc-CTH-qname.mod">
  %omdoc-CTH-qname.mod;]]>

<![%omdoc.dg.module;[
   <!ENTITY % omdoc-DG-qname.mod PUBLIC "-//OMDoc//ENTITIES OMDoc DG V1.3//EN"
				  "omdoc-DG-qname.mod">
  %omdoc-DG-qname.mod;]]>


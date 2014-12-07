<!--
   An XML Document Type Definition for Open Mathematical documents (OMDoc 1.3)
   General OMDoc Entities
     $Id: omdoc-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-model.mod $
     PUBLIC: -//OMDoc//ELEMENTS OMDoc V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->
<!ENTITY % omdoc-MOBJ-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc MOBJ V1.3//EN"
                                  "omdoc-MOBJ-model.mod">
%omdoc-MOBJ-model.mod;

<!ENTITY % omdoc-MTXT-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc MTXT V1.3//EN"
                                  "omdoc-MTXT-model.mod">
%omdoc-MTXT-model.mod;

<![%omdoc.cc.module;[
  <!ENTITY % omdoc-CC-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc CC V1.3//EN"
                                "omdoc-CC-model.mod">
  %omdoc-CC-model.mod;]]>

<!ENTITY % omdoc-DC-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc DC V1.3//EN"
                                  "omdoc-DC-model.mod">
%omdoc-DC-model.mod;

<!-- now come the optional modules, they are loaded, if the
     switches are set to INCLUDE here. This default configuration 
     can be overridden in the internal subset of the DTD -->
<![%omdoc.rt.module;[
   <!ENTITY % omdoc-RT-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc RT V1.3//EN"
                                 "omdoc-RT-model.mod">
   %omdoc-RT-model.mod;]]>

<![%omdoc.st.module;[
   <!ENTITY % omdoc-ST-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc ST V1.3//EN"
                                 "omdoc-ST-model.mod">
   %omdoc-ST-model.mod;]]>

<![%omdoc.adt.module;[
   <!ENTITY % omdoc-ADT-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc ADT V1.3//EN"
                                  "omdoc-ADT-model.mod">
    %omdoc-ADT-model.mod;]]>

<![%omdoc.pres.module;[
   <!ENTITY % omdoc-PRES-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc PRES V1.3//EN"
                                   "omdoc-PRES-model.mod">
   %omdoc-PRES-model.mod;]]>

<![%omdoc.pf.module;[
   <!ENTITY % omdoc-PF-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc PF V1.3//EN"
                                 "omdoc-PF-model.mod">
   %omdoc-PF-model.mod;]]>

<![%omdoc.ext.module;[
   <!ENTITY % omdoc-EXT-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc EXT V1.3//EN"
                                  "omdoc-EXT-model.mod">
   %omdoc-EXT-model.mod;]]>

<![%omdoc.quiz.module;[
   <!ENTITY % omdoc-QUIZ-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc QUIZ V1.3//EN"
                                   "omdoc-QUIZ-model.mod">
    %omdoc-QUIZ-model.mod;]]>

<![%omdoc.cth.module;[
   <!ENTITY % omdoc-CTH-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc CTH V1.3//EN"
                                   "omdoc-CTH-model.mod">
   %omdoc-CTH-model.mod;]]>

<![%omdoc.dg.module;[
   <!ENTITY % omdoc-DG-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc DG V1.3//EN"
                                   "omdoc-DG-model.mod">
   %omdoc-DG-model.mod;]]>

<!ENTITY % omdoc-DOC-model.mod PUBLIC "-//OMDoc//ELEMENTS OMDoc DOC V1.3//EN"
                               "omdoc-DOC-model.mod">
%omdoc-DOC-model.mod;



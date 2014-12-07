<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module MOBJ
     $Id: omdoc-MOBJ-qname.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-MOBJ-qname.mod $
      PUBLIC -//OMDoc//DTD OMDoc MOBJ V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->
<!-- We include the MathML2 DTD first since it does not interact with the others
     We include it with  namespace prefix m:, we need this, since there is 
     a name clash (selector) between omdoc and MathML2 -->


<!-- include the MathML DTD, if desired -->
<![%omdoc.mathml.module;[
  <!--  The %NamespaceDecl.attrib; parameter entity aggregates all previous 
        namespace declaration attributes by including the %omdoc.nsp.decl; 
        parameter entity into that of its embedding vocabulary. -->
  <![%NS.prefixed;[<!ENTITY % NamespaceDecl.attrib "%omdoc.nsp.decl;">]]>
  <!ENTITY % NamespaceDecl.attrib "">
  <!ENTITY % MATHML.prefixed "INCLUDE">
  <!ENTITY % MathMLstrict "INCLUDE">		
  <!ENTITY % mathmldtd  PUBLIC "-//W3C//DTD MathML 2.0//EN" "mathml2/mathml2.dtd">
  %mathmldtd;
  <!ENTITY % mathml.nsp.decl
             "xmlns:%MATHML.prefix; CDATA #FIXED '%MATHML.xmlns;'">
  <!ENTITY % omdocmobj.mathml.mix "|%math.qname;">]]>
<!ENTITY % omdocmobj.mathml.mix "">
<!ENTITY % mathml.nsp.decl "">

<!-- include the OpenMath DTD, if desired -->
<![%omdoc.openmath.module;[
  <!ENTITY % omdobj.mod PUBLIC "-//OMDoc//DTD OMDoc OMDOBJ V1.3//EN" 
                                 "omdobj.dtd">
    %omdobj.mod;
    <!ENTITY % omdocmobj.om.mix "|%om.OMOBJ.qname;">]]>
<!ENTITY % omdocmobj.om.mix "">

<!-- the OMDoc extension -->
<!ENTITY % omdocmobj.legacy.qname "%omdoc.pfx;legacy">


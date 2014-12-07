<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module MOBJ
     $Id: omdoc-MOBJ-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-MOBJ-classes.mod $
      PUBLIC -//OMDoc//DTD OMDoc MOBJ V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->
<!ENTITY % omdocmobj.extra "">
<!ENTITY % omdocmobj.class "%ss;|%omdocmobj.legacy.qname;
                                %omdocmobj.om.mix;
				%omdocmobj.mathml.mix;
				%omdocmobj.extra;">

<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3): Module PF Entities
     $Id: omdoc-PF-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-PF-classes.mod $
     PUBLIC: -//OMDoc//ENTITIES OMDoc PF V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % omdocpf.opt.content ",(%omdocpf.proof.qname;
                                 |%omdocpf.proofobject.qname;)?">
<!ENTITY % omdocpf.class "|%omdocpf.proof.qname;
                          |%omdocpf.proofobject.qname;">


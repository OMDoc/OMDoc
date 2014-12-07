<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3): Module RT Entities
     $Id: omdoc-RT-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-RT-classes.mod $
     PUBLIC: -//OMDoc//ENTITIES OMDoc RT V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % omdocrt.mtext.extra "">
<!ENTITY % omdocrt.mtext.mix "|%omdocrt.ul.qname;
                              |%omdocrt.ol.qname;		     
                              |%omdocrt.dl.qname;		     
                              |%omdocrt.p.qname;		     
                              |%omdocrt.note.qname;
                              |%omdocrt.link.qname;
                              |%omdocrt.idx.qname;
                              |%omdocrt.table.qname;
			      %omdocrt.mtext.extra;">
	

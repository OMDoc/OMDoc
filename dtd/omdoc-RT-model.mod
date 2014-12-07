<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module DOC
     $Id: omdoc-RT-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-RT-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc RT V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ENTITY % omdocrt.common.attrib "%id.attrib; %fori.attrib; %parallel.attribs;">

<!ENTITY % index.attribs "sort-by CDATA #IMPLIED
	                 see %omdocrefs; #IMPLIED
	                 seealso %omdocrefs; #IMPLIED
		         links %anyURIs; #IMPLIED">

<!ELEMENT %omdocrt.li.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.li.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.dt.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.dt.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.dd.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.dd.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.ul.qname; (%ss;|(%omdocdoc.meta.content;(%omdocrt.li.qname;)+))>
<!ATTLIST %omdocrt.ul.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.ol.qname; (%ss;|(%omdocdoc.meta.content;(%omdocrt.li.qname;)+))>
<!ATTLIST %omdocrt.ol.qname; 
          %omdoc.nsschema.attrib;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.dl.qname; (%ss;|(%omdocdoc.meta.content;(%omdocrt.di.qname;)+))>
<!ATTLIST %omdocrt.dl.qname; 
          %omdoc.nsschema.attrib;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.di.qname; (%omdocdoc.meta.content;(%ss;|((%omdocrt.dt.qname;)*,(%omdocrt.dd.qname;)*)))>

<!ATTLIST %omdocrt.di.qname; 
          %omdoc.nsschema.attrib;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.p.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.p.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.note.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.note.qname; 
          %omdoc.common.attribs; 
          %omdocrt.common.attrib;
          type NMTOKEN #IMPLIED>

<!-- a simplified table -->
<!ELEMENT %omdocrt.table.qname; (%ss;|(%omdocdoc.meta.content;(%omdocrt.tr.qname;)+))>
<!ATTLIST %omdocrt.table.qname; %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.tr.qname; (%ss;|(%omdocdoc.meta.content;(%omdocrt.td.qname;|%omdocrt.th.qname;)+))>
<!ATTLIST %omdocrt.tr.qname; 
          %omdoc.common.attribs;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.td.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.td.qname; 
          %omdoc.common.attribs;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.th.qname; (%omdoc.mtext.content;|%omdocdoc.metadata.qname;)*>
<!ATTLIST %omdocrt.th.qname; 
          %omdoc.common.attribs;
          %omdocrt.common.attrib;>

<!ELEMENT %omdocrt.link.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocrt.link.qname; 
          %omdoc.common.attribs;
          %omdocrt.common.attrib;
          href %anyURI; #REQUIRED>

<!ELEMENT %omdocrt.idx.qname; ((%omdocrt.idt.qname;)?,(%omdocrt.ide.qname;)+)>
<!ATTLIST %omdocrt.idx.qname; %omdoc.common.attribs; %id.attrib;>

<!ELEMENT %omdocrt.ide.qname; ((%omdocrt.idp.qname;)+)>
<!ATTLIST %omdocrt.ide.qname;
	  %omdoc.common.attribs; 
	  %index.attribs;
          index CDATA #IMPLIED>

<!ELEMENT %omdocrt.idp.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocrt.idp.qname; %omdoc.common.attribs;  %index.attribs;>

<!ELEMENT %omdocrt.idt.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocrt.idt.qname; %omdoc.common.attribs; %id.attrib;>

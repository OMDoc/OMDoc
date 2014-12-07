<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module DC 
     $Id: omdoc-DC-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-DC-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc DC V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->


<!-- first the Dublin Core Metadata model of the
     Dublin Metadata initiative (http://purl.org/dc) -->

<!ELEMENT %omdocdc.contributor.qname; %omdocdc.person.content;>
<!ATTLIST %omdocdc.contributor.qname; %dcrole;>

<!ELEMENT %omdocdc.creator.qname; %omdocdc.person.content;>
<!ATTLIST %omdocdc.creator.qname; %dcrole;>

<!ELEMENT %omdocdc.title.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocdc.title.qname; %dclang;>

<!ELEMENT %omdocdc.subject.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocdc.subject.qname; %dclang;>

<!ELEMENT %omdocdc.description.qname; (%omdoc.mtext.content;)*>
<!ATTLIST %omdocdc.description.qname; %dclang;>

<!ELEMENT %omdocdc.publisher.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.publisher.qname; %omdocdc.ns.decl; %id.attrib;>

<!ELEMENT %omdocdc.type.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.type.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.format.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.format.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.source.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.source.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.language.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.language.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.relation.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.relation.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.rights.qname; %omdocdc.rest.content;>
<!ATTLIST %omdocdc.rights.qname; %omdocdc.ns.decl;>

<!ELEMENT %omdocdc.date.qname; %omdocdc.date.content;>
<!ATTLIST %omdocdc.date.qname; 
          %omdocdc.ns.decl; 
          action NMTOKEN #IMPLIED 
	  who %omdocref; #IMPLIED>

<!ELEMENT %omdocdc.identifier.qname; %omdocdc.ident.content;>
<!ATTLIST %omdocdc.identifier.qname; %omdocdc.ns.decl; scheme NMTOKEN "ISBN">




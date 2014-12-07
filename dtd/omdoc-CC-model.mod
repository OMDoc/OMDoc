<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module CC
     $Id: omdoc-CC-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-CC-model.mod $
     PUBLIC -//OMDoc//ELEMENTS OMDoc CC V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 2004-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ELEMENT %omdoccc.license.qname; ((%ss;|%omdoccc.permissions.qname;),
                                   (%ss;|%omdoccc.prohibitions.qname;),
                                   (%ss;|%omdoccc.requirements.qname;))>
<!ATTLIST %omdoccc.license.qname; %omdoccc.ns.decl; 
                                  jurisdiction %iana.tld; #IMPLIED
                                  version CDATA #IMPLIED>

<!ELEMENT %omdoccc.permissions.qname; EMPTY>
<!ATTLIST %omdoccc.permissions.qname; 
          %omdoccc.ns.decl;
          reproduction (permitted|prohibited) #IMPLIED
	  distribution (permitted|prohibited) #IMPLIED
          derivative_works (permitted|prohibited) #IMPLIED>

<!ELEMENT %omdoccc.prohibitions.qname; EMPTY>
<!ATTLIST %omdoccc.prohibitions.qname; 
          %omdoccc.ns.decl;
          commercial_use (prohibited|permitted) #IMPLIED>

<!ELEMENT %omdoccc.requirements.qname; EMPTY>
<!ATTLIST %omdoccc.requirements.qname; 
	  %omdoccc.ns.decl;
	  notice (required|not_required) #IMPLIED
          attribution (required|not_required) #IMPLIED
          copyleft (required|not_required) #IMPLIED>

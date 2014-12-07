<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module PF
     $Id: omdoc-PF-model.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-PF-model.mod $
      PUBLIC -//OMDoc//ELEMENTS OMDoc PF V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<!ELEMENT %omdocpf.proof.qname; 
          (%omdocdoc.meta.content;
                 (%ss;|%omdocmtxt.omtext.qname;
		 |%omdocst.symbol.qname;
		 |%omdocst.definition.qname;
                 |%omdocpf.derive.qname;
                 |%omdocpf.hypothesis.qname;)*)>
<!ATTLIST %omdocpf.proof.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %fori.attrib;>

<!ELEMENT %omdocpf.proofobject.qname; 
          (%omdocdoc.meta.content;(%omdocmobj.class;))>
<!ATTLIST %omdocpf.proofobject.qname; 
          %omdoc.common.attribs; 
          %omdoc.toplevel.attribs;
          %fori.attrib;>

<!ELEMENT %omdocpf.derive.qname; 
          (%omdocmtxt.MCF.content;,(%ss;|%omdocpf.method.qname;)?)>
<!ATTLIST %omdocpf.derive.qname; 
          %omdoc.common.attribs; 
	  type CDATA #IMPLIED
          %id.attrib;>

<!ELEMENT %omdocpf.hypothesis.qname; (%omdocmtxt.MCFS.content;)>
<!ATTLIST %omdocpf.hypothesis.qname; 
          %omdoc.common.attribs; 
          %id.attrib; 
          inductive (yes|no) #IMPLIED>

<!ELEMENT %omdocpf.method.qname; 
	 	  (%omdocmobj.class;|%omdocpf.premise.qname;
                  |%omdocpf.proof.qname;|%omdocpf.proofobject.qname;)*>
<!ATTLIST %omdocpf.method.qname; %omdoc.common.attribs; %id.attrib; %xrefi.attrib;>
<!-- 'xref' is a pointer to the element defining the method -->

<!ELEMENT %omdocpf.premise.qname; EMPTY>
<!ATTLIST %omdocpf.premise.qname; %omdoc.common.attribs; 
	  %xref.attrib; rank CDATA #IMPLIED>
<!-- The rank of a premise specifies its importance in the 
     inference rule. Rank 0 (the default) is a real premise, 
     whereas positive rank signifies sideconditions of 
     varying degree. -->


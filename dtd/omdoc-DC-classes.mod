<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module DC Entities
     $Id: omdoc-DC-classes.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-DC-classes.mod $
     PUBLIC -//OMDoc//ENTITIES OMDoc DC V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 1999-2006 Michael Kohlhase, released under the GNU Public License (GPL)
-->

<![%omdocdc.prefixed;[
	<!ENTITY % omdocdc.nsp.decl "xmlns:%omdocdc.prefix; %anyURI; #FIXED '%omdocdc.xmlns;'">
	<!ENTITY % omdocdc.ns.decl "">]]>
<!ENTITY % omdocdc.nsp.decl "">
<!ENTITY % omdocdc.ns.decl "xmlns %anyURI; #FIXED '%omdocdc.xmlns;'">

<!-- Module DC -->
<!ENTITY % omdocdc.class
           "| %omdocdc.contributor.qname; 
            | %omdocdc.creator.qname; 
            | %omdocdc.subject.qname; 
            | %omdocdc.title.qname; 
            | %omdocdc.description.qname; 
            | %omdocdc.publisher.qname; 
            | %omdocdc.date.qname; 
            | %omdocdc.type.qname; 
            | %omdocdc.format.qname; 
            | %omdocdc.identifier.qname; 
            | %omdocdc.source.qname; 
            | %omdocdc.language.qname; 
            | %omdocdc.relation.qname; 
            | %omdocdc.rights.qname;">

<!-- Persons in Dublin Core Metadata -->
<!ENTITY % omdocdc.person.content "(#PCDATA)*">
<!-- the date format in Dublin Core -->
<!ENTITY % omdocdc.date.content "(#PCDATA)">
<!-- the identifier format for Dublin Core -->
<!ENTITY % omdocdc.ident.content "(#PCDATA)">
<!-- the rest of Dublin Core content -->
<!ENTITY % omdocdc.rest.content "ANY">

<!-- the MARC relator set; see http://www.loc.gov/marc/relators -->
<!ENTITY % omdocdc.marc.value 
           "act|adp|aft|ann|ant|app|aqt|arc|arr|art|asg|asn|att|auc|aud|aui|aus|aut
           |bdd|bjd|bkd|bkp|bnd|bpd|bsl|ccp|chr|clb|cli|cll|clt|cmm|cmp|cmt|cnd|cns|coe
           |col|com|cos|cot|cov|cpc|cpe|cph|cpl|cpt|cre|crp|crr|csl|csp|cst|ctb|cte|ctg
           |ctr|cts|ctt|cur|cwt|dfd|dfe|dft|dgg|dis|dln|dnc|dnr|dpc|dpt|drm|drt|dsr|dst
           |dte|dto|dub|edt|egr|elt|eng|etr|exp|fac|flm|fmo|fnd|fpy|frg|hnr|hst|ill|ilu
           |ins|inv|itr|ive|ivr|lbt|lee|lel|len|let|lie|lil|lit|lsa|lse|lso|ltg|lyr|mdc
           |mod|mon|mrk|mte|mus|nrt|opn|org|orm|oth|own|pat|pbd|pbl|pfr|pht|plt|pop|ppm
           |prc|prd|prf|prg|prm|pro|prt|pta|pte|ptf|pth|ptt|rbr|rce|rcp|red|ren|res|rev
           |rpt|rpy|rse|rsp|rst|rth|rtm|sad|sce|scl|scr|sec|sgn|sng|spk|spn|spy|srv|stl
           |stn|str|ths|trc|trl|tyd|tyg|voc|wam|wdc|wde|wit">

<!ENTITY % dcrole "%omdocdc.ns.decl; %id.attrib;  %xml.lang.attrib;
           role (%omdocdc.marc.value;) #IMPLIED">
<!ENTITY % dclang "%omdocdc.ns.decl; %id.attrib; %xml.lang.attrib;">



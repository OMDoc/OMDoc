<!--
   An XML DTD for Open Mathematical documents (OMDoc 1.3) Module CC Entities
     $Id: omdoc-CC-qname.mod 8458 2009-08-04 10:23:53Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/dtd/omdoc-CC-qname.mod $
     PUBLIC -//OMDoc//ENTITIES OMDoc CC V1.3//EN
   See the documentation and examples at http://www.omdoc.org 
   Copyright (c) 2004 Michael Kohlhase, released under the GNU Public License (GPL)
-->
<!ENTITY % omdoccc.prefixed "%NS.prefixed;">
<!ENTITY % omdoccc.xmlns "http://creativecommons.org/ns">
<!ENTITY % omdoccc.prefix "cc">

<![%omdoccc.prefixed;[<!ENTITY % omdoccc.pfx "%omdoccc.prefix;:">]]>
<!ENTITY % omdoccc.pfx "">

<!ENTITY % omdoccc.license.qname "%omdoccc.pfx;license">
<!ENTITY % omdoccc.permissions.qname  "%omdoccc.pfx;permissions">
<!ENTITY % omdoccc.prohibitions.qname  "%omdoccc.pfx;prohibitions">
<!ENTITY % omdoccc.requirements.qname  "%omdoccc.pfx;requirements">

<!ENTITY % iana.tld "(ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|
                      ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|
                      ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|
                      de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|fi|fj|fk|fm|fo|fr|
                      ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|
                      hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|
                      ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|
                      ma|mc|md|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|
                      na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|
                      pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|ru|rw|
                      sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|sv|sy|sz|
                      tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|
                      ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)">


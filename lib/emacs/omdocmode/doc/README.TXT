OMDoc mode -- README.TXT				September 2002.

omdoc-mode.el: An Emacs major mode for editing OMDoc Documents
==============================================================

OMDoc is an XML format for Open Mathematical Documents.

The OMDoc format is maintained at http://omdoc.org,
where you can also find
- a language specification (http://omdoc.org/omdoc.{ps,pdf})
- a Document Type Definition (http://omdoc.org/dtd/omdoc.dtd)
- some examples of OMdoc documents (http://omdoc.org/examples)

This package contains several files, which together make up an emacs
'major mode' (written in Emacs Lisp) for editing OMDoc documents.
It contains code to help 
- create (by means of template insertion of a document skeleton
  and/or each of the possible OMDoc elements),
- visualize (by means of fonts and colors), 
- edit (by means of special mode operations), 
- lay out (e.g. by means of a customizable indent function), and 
- test (validate) 
OMDoc documents.

The mode has grown out of a combination of several pieces of existing
modes for xml, html, xsl, and sgml. In this process, we are most indebted
to Nelson Minar's html-helper-mode from which we borrowed, for example,
some code to set up the tempo.el template mechanism, and the menus.

We are aware of several bugs, portability issues, hacks, and as of
yet missing functionality in this pre-release, most of which we
hope to iron out in the near future. Hence, use the mode at your
own risk. But any feedback (bugs, wishes, suggestions, etc.)
will be greatly appreciated!

Practical note:
- manifest.txt lists the files making up this package with a
               brief description
- instructions.txt explains how to install the mode.
- functionality.txt explains the functionality of the mode.
- buglist.txt describes known bugs and contains a wishlist (please help)

DOWNLOAD: http://www.cs.cmu.edu/~ccaps (menu item software)

Contacts:
Peter Jansen (pjj@cs.cmu.edu).
Michael Kohlhase (kohlhase+@cs.cmu.edu).



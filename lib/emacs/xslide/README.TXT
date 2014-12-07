This package contains an Emacs major mode for editing XSL stylesheets.
The current revision is 0.2, dated 27 August, 2001.

The xslide home page is http://www.menteith.com/xslide/

xslide source code is also available at
http://www.sourceforge.net/projects/xslide/.

* MANIFEST

File                    Contents

README.TXT              The file you are reading
changelog.txt           Record of changes made to xslide elisp files
dot_emacs               Some things for your .emacs file
xslide.el               Base file for the XSL major mode
xslide-abbrev.el        Abbreviations for XSL mode
xslide-data.el          Data about XSL elements and formatting objects
xslide-font.el          Font lock keywords for XSL stylesheets
xslide-initial.xsl      Default initial stylesheet inserted in empty buffers
xslide-process.el       Procedures for processing XSL stylesheets
makefile                Makefile for byte-compiling 

Note that make-regexp.el, which was used by earlier version of xslide,
is not required by xslide 0.2.

Note that "xslide" is pronounced as one word, similar to "slide".  It
is not spelled out as "x-s-l-i-d-e".


* FEATURES

 - XSL customization group for setting some variables

 - Initial stylesheet inserted into empty XSL buffers;

 - "Template" menu for jumping to template rules, named templates, key
   declarations, and attribute-set declarations in the buffer;

 - `xsl-process' function that runs an XSL processor and collects the
   output;

 - Predefined command line templates and error regexps for Java and
   Windows executable versions of both XT and Saxon;

 - Font lock highlighting so that the important information stands
   out;

 - `xsl-complete' function for inserting element and attribute names;

 - `xsl-insert-tag' function for inserting matching start- and
    end-tags;

 - Automatic completion of end-tags;

 - Automatic indenting of elements with user-definable indentation
   step; and

 - Comprehensive abbreviations table to further ease typing.

xsl-mode was tested using NTEmacs 20.7.1.


* BUG REPORTS/ENCHANCEMENTS

I would be glad to accept bug reports and, especially, enhancements.
Use `xsl-submit-bug-report' for bug reports.


* INSTALLATION

1. Unzip the distribution.

   This should extract the files listed in the manifest above.

   The files have DOS-style line breaks.  You may need to use "unzip
   -a" or "gunzip -a" to convert the line ends in the files to your
   local convention.

2. Copy the .el files to your emacs site-lisp directory (e.g.,
   /usr/local/lib/emacs/site-lisp) or some other directory that is on
   your Emacs load path.

3. Byte compile the .el files using M-x byte-compile-file and
   supplying the path name of each file.

   The provided makefile also has rules for byte-compiling the files.

   If you byte-compiled a previous version of the files, you must
   byte-compile the new files (or remove the old .elc files) so that
   autoload loads the correct versions.

4. Add the elisp code in dot_emacs to your .emacs file.

5. Restart Emacs (or, if it's easier for you, evaluate the code that
   you just added to your .emacs file).

6. When you visit a file with an extension of .xsl or .fo, xsl-mode
   will automatically be loaded.

7. To find out more about xsl-mode, type C-h m from within an XSL
   file.

8. To customize xsl-mode font-lock colours, etc., browse the `Xsl'
   Customize group after loading xsl-mode.

-----
$Id: README.TXT 3883 2003-04-11 19:13:01Z kohlhase $

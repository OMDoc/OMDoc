;;;; xslide-process.el --- Process an XSL stylesheet
;; $Id: xslide-process.el 3883 2003-04-11 19:13:01Z kohlhase $

;; Copyright (C) 1998, 1999, 2000, 2001 Tony Graham

;; Author: Tony Graham <tkg@menteith.com>

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Copied almost wholesale from psgml.el by Lennart Staflin

;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' for bug reports


;;;; Variables:



(defcustom xsl-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[xsl-process] is run."
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil))
  :group 'xsl-process)

(defcustom xsl-process-command
  (list
   ;; XT Windows executable
   "xt %i %s %o"
   ;; XT Java
   "java com.jclark.xsl.Driver %i %s %o"
   ;; Instant Saxon
   "saxon -o %o %i %s"
   ;; Instant Saxon using xml-stylesheet PI
   "saxon -o %o %i"
   ;; Saxon
   "java com.icl.saxon.StyleSheet -o %o %i %s"
   ;; Saxon using xml-stylesheet PI
   "java com.icl.saxon.StyleSheet -o %o %i")
  "*The shell command to process an XSL document.

This is a `format' control string that by default should contain three
`%s' conversion specifications: the first will be replaced by the
value of xsl-process-input-file \(or the empty string, if nil\); the
second will be replaced by xsl-process-stylesheet-file \(or the empty
string, if nil\); the third will be replaced by
xsl-process-output-file \(or the empty string, if nil\).

If `xsl-process-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If xsl-process-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%i means the value of xsl-process-input-file
%s means the value of xsl-process-stylesheet-file
%o means the value of xsl-process-output-file
"
  :type '(repeat :tag "Commands" string)
  :group 'xsl-process)

(defvar xsl-process-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `xsl-process-command'
format control string instead of the defaults.")

(defcustom xsl-process-error-regexps
  '(("file:\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
    ("file:/\\(\\([A-Za-z]:\\)?[^:]+\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?" 1 3 5)
    ("^Error at [^ ]+ on line \\([0-9]+\\) of file:\\([^:]+\\):$" 2 1)
    ("^Error at [^ ]+ on line \\([0-9]+\\) of file:/\\(\\([a-z]:\\)?[^:]+\\):$" 2 1))
  "Alist of regexps to recognize error messages from `xsl-process'.
See `compilation-error-regexp-alist'."
  :type '(repeat :tag "Regexps"
		 (list (regexp :tag "Regexp")
		       (integer :tag "File index")
		       (integer :tag "Line index")
		       (choice :tag "Column index"
			       (integer)
			       (const :tag "None" nil))))
  :group 'xsl-process)

(defvar xsl-xml-source nil
  "*If non-nil, this is the name of the XML source file.")
(put 'xsl-xml-source 'xsl-type 'string)

(defvar xsl-xsl-result nil
  "*If non-nil, this is the name of the XSL result file.")
(put 'xsl-xsl-result 'xsl-type 'string)

(defvar xsl-process-command-history nil
  "The minibuffer history list for `xsl-process''s COMMAND argument.")
;;(make-variable-buffer-local 'xsl-process-command-history)

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun xsl-subst-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun xsl-subst-expand (s parts)
  (loop for i from 0 to (1- (length s))
	as c = (aref s i)
	concat (if (eq c ?%)
		   (or (xsl-subst-expand-char (aref s (incf i)) parts)
		       (return nil)) 
		 (char-to-string (aref s i)))))

(defun xsl-populate-process-command-history ()
  (cond
   ((consp xsl-process-command)
    (let ((process-subst
	   (list
	    (cons ?b (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))))
	    (cons ?i xsl-process-input-file)
	    (cons ?s xsl-process-stylesheet-file)
	    (cons ?o xsl-process-output-file))))
      (setq xsl-process-command-history
	    (append
	     (mapcar (lambda (x)
		       (xsl-subst-expand x process-subst))
		     xsl-process-command)
	     xsl-process-command-history))))
;;      (loop for template in xsl-process-command
;;	    append
;;	    (xsl-subst-expand template process-subst)
;;	    into
;;	    xsl-process-command-history)))
   (t
    (apply 'format xsl-process-command
	   (if xsl-process-files
	       (funcall xsl-process-files)
	     (list (or xsl-xml-source "")
		   (let ((name (buffer-file-name)))
		     (if name
			 (file-name-nondirectory name)
		       ""))
		   (or xsl-xsl-result "")))))))

(defvar xsl-process-input-file nil
  "Filename of input file for `xsl-process' command")

(defvar xsl-process-input-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s INPUT argument.")

(defvar xsl-process-stylesheet-file nil
  "Filename of stylesheet file for `xsl-process' command")

(defvar xsl-process-stylesheet-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s STYLESHEET argument.")

(defvar xsl-process-output-file nil
  "Filename of output file for `xsl-process' command")

(defvar xsl-process-output-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s OUTPUT argument.")

(defun xsl-get-process-parameters ()
  "Get and set the parameters for the `xsl-process' command"
  (interactive)
  (setq xsl-process-input-file
	(xsl-read-from-minibuffer "Input file: "
			      (concat (file-name-sans-extension
				       (file-name-nondirectory
					(buffer-file-name)))
				      ".xml")
			      'xsl-process-input-history))
  (setq xsl-process-stylesheet-file
	(xsl-read-from-minibuffer "Stylesheet file: "
			      (file-name-nondirectory
			       (buffer-file-name))
			      'xsl-process-stylesheet-history))
  (setq xsl-process-output-file
	(xsl-read-from-minibuffer "Output file: "
			      (concat (file-name-sans-extension
				       (file-name-nondirectory
					xsl-process-input-file))
				      ".html")
			      'xsl-process-output-history)))

(defun xsl-process (command)
  "Process an XSL stylesheet.

Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *XSL process*.  You can then use the
command \\[next-error] to find the next error message and move to the
line in the XSL document that caused it.

The first time that the program is run and whenever you provide a
prefix argument, e.g. \\[universal-argument] \\[xsl-process], prompts
for input filename, stylesheet file, and output filename.  Those
values are used with the templates in `xsl-process-command' to
populate this command's command history with the command lines to run
several XSLT processors using those values.  Use M-p and M-n to step
through the predefined commands, edit a command if necessary, or enter
a new command line.  The next time that this command is run, the
previously executed command is used as the default."
  (interactive
   (list (progn
	   (if (or
		current-prefix-arg
		(null xsl-process-command-history))
	       (progn
		 (xsl-get-process-parameters)
		 (xsl-populate-process-command-history)))
	   (read-from-minibuffer "Process command: "
				     (car xsl-process-command-history)
				     nil nil
				     'xsl-process-command-history))))
  (if xsl-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "XSL process"
		    nil
		    xsl-process-error-regexps))


(provide 'xslide-process)

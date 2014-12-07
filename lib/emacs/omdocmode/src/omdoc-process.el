;;; omdoc-process.el --- Validation code for omdoc-mode.el

;; Author: Peter Jansen <pjj@cs.cmu.edu>
;; Maintainer: Peter Jansen <pjj@cs.cmu.edu>
;; Contacts: pjj@cs.cmu.edu, kohlhase+@cs.cmu.edu
;; Keywords: OMDoc major-mode (Open Mathematical Documents)
;; Created: August 2002

;; This file is NOT part of GNU Emacs

;; Copyright (C) 2002 CMU-SCS CCAPS project.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;;{{{ 

;;; Validation of OMDoc documents. Two functions are provided,
;;;   - a 'real' process validation (with rxp, xmllint, or nsgmls, 
;;;     to be edited by the user to fit to path on the system
;;;   - a simple local syntax check with some error correction
;;;     (not really a process).
;;; 
;;; The validation program and its parameters are set in omdoc-config.el

;;}}}

;;{{{ Code:

;;{{{ Dependencies

(require 'omdoc-config "omdoc-config")

;;}}}

;;{{{ Simple syntactic validation

(defun omdoc-simple-validate ()
; simple validation:
  "Simple parser to detect possibly corrupted tags."
  (interactive)
;   initialize the stack to nil
;   errors-so-far = 0
  (let ((omdoc-stack nil)
	last-error
	(errors-so-far 0))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
;   find the initial omdoc tag and put point before it
	(if (null (re-search-forward "<omdoc ?[^>]*>" nil t))
	    (message "OMDoc Warning: malformed document: no starting tag?")
	  (setq omdoc-stack (cons "omdoc" omdoc-stack))
	  (goto-char (match-end 0))
; (should really define error an error handler for this!)
;   while not done:
	  (while (and omdoc-stack
		      (<= errors-so-far 3))
;   - find the next tag
;   - if no next tag, error message end of file before end.
	    (let ((next-bracket (re-search-forward "<" nil t)))
	      (backward-char)
	     (if (null next-bracket)
		 (message "OMDoc Warning: premature end of file encountered.")
	      (if (looking-at "<!\\[ ?CD")
		  (let ((beg (match-beginning 0)))
		    (if (null (re-search-forward "\\]\\]>" nil t))
			(message "OMDoc Error: unterminated CDATA.")
		      (goto-char (match-end 0))))
		(if (looking-at "<[!?-]")
;   - if it is a comment tag, do nothing
		    (let ((beg (match-beginning 0)))
		      (if (null (re-search-forward ">" nil t))
			  (message "OMDoc Error: EOF in comment-type field.")
			(goto-char (match-end 0))))
;   - if it is a close tag,
		  (if (looking-at "</\\([^>]*\\)>")
		      (let ((tag (match-string-no-properties 1)))
;			(message "In closing tag")
;			(sit-for 1)
;			(omdoc-blink (match-beginning 0) (match-end 0) 0.2)
			(goto-char (match-end 0))
;     + if it is equal to the top el of the stack, pop it
			(if (equal tag (car omdoc-stack))
			    (progn
			      (setq omdoc-stack (cdr omdoc-stack))
;       if the stack is now empty, we are done.			  
			      )
;     + elsif it is equal to the 2nd element:
			  (if (equal tag (cadr omdoc-stack))
;        we are guessing that the closing tag was omitted
;        so, pop the first and insert its closing (with error) before this one.
			      (let ((newtag (car omdoc-stack)))
				(save-excursion
				  (goto-char (1- (match-beginning 0)))
				  (insert (concat "</error:" newtag ">"))
				  (setq last-error (point))
				  (setq errors-so-far (1+ errors-so-far)))
;          then pop the 2nd.
				(setq omdoc-stack (cddr omdoc-stack))
;				(message (prin1-to-string omdoc-stack))
;				(sit-for 1)
				)
;     + else: we are guessing the wrong closing is used:
;         so, replace tag by the closing for pop.
			    (let ((newtag (car omdoc-stack)))
			      (save-excursion
				(goto-char (match-beginning 1))
				(insert "error:")
				(goto-char (+ (match-end 0) 5))
				(insert (concat ":=" newtag))
				(setq last-error (point))
				(setq errors-so-far (1+ errors-so-far)))
;          then pop the 2nd.
			      (setq omdoc-stack (cdr omdoc-stack))
;			      (message (prin1-to-string omdoc-stack))
;			      (sit-for 1)
			      ))))
; It is a 'both' tag or open tag.
;   - if it is a both tag, do nothing.
		    (if (looking-at "<\\([^ >]+\\)\\( +[^ >]+\\)*/>")
			(progn
;			  (omdoc-blink (match-beginning 0) (match-end 0) 0.2)
			  (goto-char (match-end 0)))
;   - if it is an open tag, push the tag on the stack
		      (if (looking-at "<\\([^ >]+\\)\\( +[^ >]+\\)*>")
			  (let ((tag (match-string-no-properties 1)))
;			    (omdoc-blink (match-beginning 0) (match-end 0) 0.2)
			    (goto-char (match-end 0))
			    (setq omdoc-stack (cons tag omdoc-stack))
;			    (message (prin1-to-string omdoc-stack))
;			    (sit-for 1)
			    ))
	)))))))
; end of the while
	  (if (eq errors-so-far 0)
	      (message "No errors found: open/close tags match up within the first <omdoc>..</omdoc> element!")
	    (message (concat "Found and attempted to correct at least "
			     errors-so-far " errors."))
	    ))))
    (if last-error (goto-char last-error))))

;;}}}

;;{{{ 'Real' validation with an extermal process.

(defun omdoc-validate ()
  "Save current buffer, and run omdoc-validate-program on it."
  (interactive)
  (let ((buffer (get-buffer-create "*Validation Stats*"))
	(omdoc-buffer (current-buffer))
	(omdoc-file buffer-file-name))
    (save-buffer omdoc-buffer)
    (set-buffer buffer)
    (erase-buffer)
    (setq validate-error-point (point-min))
    (insert (format "-- Starting Validation: %s %s %s --\n\n"
		    omdoc-validate-program
		    omdoc-file
 		    omdoc-validate-params))
    (call-process omdoc-validate-program
		  nil t nil
		  omdoc-validate-params omdoc-file)
    (let ((time (current-time-string)))
      (insert "\n-- Validation by "
	      omdoc-validate-program
	      " completed at "
	      (substring time 0 20)
	      (nth 1 (current-time-zone))
	      " "
	      (substring time -4)))
    (display-buffer buffer)))

;;}}}

;;{{{ Generating HTML from omdoc buffer in external shell.

(defun tohtml-template ()
  "Generate template for xxx2omdoc.xsl"
  (concat
   "<?xml version='1.0'?>\n<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' xmlns:exsl='http://exslt.org/common' extension-element-prefixes='exsl' version='1.0'>\n<!-- we specialize -->\n<xsl:include href='"
   omdochome
   "/xsl/omdoc2html.xsl'/>\n<!-- by the specialized templates contained in this style sheet -->\n<xsl:include href='"
   xxxincl
   "'/>\n</xsl:stylesheet>"
))


(defun omdoc-htmlify ()
  "Save current buffer, and generate html file from it."
  (interactive)
  (let* ((buffer (get-buffer-create "*Saxon Shell*"))
         (omdoc-buffer (current-buffer))
         (omdoc-file buffer-file-name)
         (title (omdoc-current-title))
         (xxx2html (concat title "2html.xsl"))
         (xxxtmpl (concat title "-tmpl.xsl"))
         (xxxincl (concat title "-incl.xsl"))
         (xxxhtml (concat title ".html"))
         (command (concat omdochome "/bin/saxon"))
         (params1 (concat "-o " xxxtmpl " " omdoc-file " "
                   omdochome "/xsl/expres.xsl"))
         (params2 (concat "-o " xxxincl " " omdoc-file " "
                   omdochome "/xsl/exincl.xsl self="
                   xxxtmpl))
         (params3 (concat "-o " xxxhtml " " omdoc-file " "
                   xxx2html " css=" omdochome "/lib/omdoc-default.css"))
         )
    (save-buffer omdoc-buffer)
; first create the xxx2html file.
    (with-temp-file xxx2html (insert (tohtml-template)))
; then switch to the log buffer
    (set-buffer buffer)
    (erase-buffer)
    (insert (format "-- Created: %s --\n\n"
                    xxx2html))
    (let ((time (current-time-string)))
      (insert "\n-- HTML generation started at"
              (substring time 0 20)
              (nth 1 (current-time-zone))
              " "
              (substring time -4)))
    (display-buffer buffer)
;run expres.xsl on xxx.omdoc first: ../../bin/saxon -o xxx-tmpl.xsl xxx.omdoc ../../xsl/expres.xsl 
    (insert (format "-- Running expres.xsl: %s %s --\n\n"
                    command
                    params1))
    (display-buffer buffer)
    (call-process command
                  nil t nil
                  params1)
;run exincl by ../../bin/saxon -o xxx-incl.xsl xxx.omdoc ../../xsl/exincl.xsl self='xxx-tmpl.xsl' -- ;this will generate xxx-incl.xsl
    (insert (format "-- Running incl.xsl: %s %s --\n\n"
                    command
                    params2))
    (display-buffer buffer)
    (call-process command
                  nil t nil
                  params2)
;then run xxx2html.xsl on xxx.omdoc via ../../bin/saxon -o xxx.html xxx.omdoc xxx2html.xsl css='../../lib/omdoc-default.css' -- this should generate xxx.html
    (display-buffer buffer)
    (insert (format "-- Generating HTML: %s %s --\n\n"
                    command
                    params3))
    (display-buffer buffer)
    (call-process command
                  nil t nil
                  params3)
    (display-buffer buffer)
;    (call-process omdoc-validate-program
;                 omdoc-file t nil
;                 omdoc-validate-params)
    (let ((time (current-time-string)))
      (insert "\n-- HTML generation completed at"
              (substring time 0 20)
              (nth 1 (current-time-zone))
              " "
              (substring time -4)))
    (insert "\n-- loading browser --\n\n")
    (display-buffer buffer)
    (browse-url-of-file xxxhtml)
    (delete-buffer buffer)
    ))

;;}}}

;;; Parsing the validation output (currently only works for rxp) 

;;; - Global Parser Variables

(defvar validate-error-point nil
  "How far we have parsed until now.")
(make-variable-buffer-local 'validate-error-point)

(defun xmllint-parse-error ()
  "Compute filename, line, column, and description, for next error from xmllint output"
  (re-search-forward (concat "\\(.*\\):"            ; filename
			     "\\([0-9]*\\): "        ; line
			     ".* error: \\(.*\\)\n"    ; description
			     "\\(.*\\)\n"           ; error line
			     "\\(.*\\)\n"           ; error location
			     "\\|Validation by xmllint completed"))
  (if (match-string 1)
      (list (match-string 1)
	    (string-to-int (match-string 2))
	    (length (match-string 5))
	    (match-string 2))
    nil))

(defun rxp-parse-error ()
  "Compute filename, line, column, and description, for next error from rxp output"
  (re-search-forward (concat "in unnamed entity at line \\([0-9]*\\)"  ; line
			     " char \\([0-9]*\\)"                      ; column
			     " of file://\\(.*\\)"                     ; file name
			     "\\|Validation by rxp completed"))         
  (if (match-string 1)
      (list (match-string 3)
	    (string-to-int (match-string 1))
	    (string-to-int (match-string 2))
	    "none given yet")
    nil))

(defun omdoc-next-error ()
  "Find the next error produced by running validation
   If the file occurs in an included file, the file is loaded (if not
   already in an Emacs buffer) and the cursor is placed at the error."
  (interactive)
  (cond ((null (get-buffer "*Validation Stats*"))
	 (error "No Validation output buffer"))
	((not (or (string= omdoc-validate-program "rxp")
		  (string= omdoc-validate-program "xmllint")))
	 (error "cannot parse output of validator %s" omdoc-validate-program))
	(t (let ((old-buffer (current-buffer)))
	     (pop-to-buffer (get-buffer "*Validation Stats*"))
	     (goto-char validate-error-point)
	     (let ((result (cond ((string= omdoc-validate-program "rxp")
				  (rxp-parse-error))
				 ((string= omdoc-validate-program "xmllint")
				  (xmllint-parse-error)))))
	       (cond (result 
		      ;; remember were we were
		      (setq validate-error-point (point))
		      ;; go to the error point
		      (find-file-other-window (first result))
		      (goto-line (nth 1 result))
		      (forward-char (nth 2 result)))
		     (t (message "No more errors")
			(beep)
			(pop-to-buffer old-buffer)
			nil)))))))

(provide 'omdoc-process)
;;; omdoc-process.el ends here

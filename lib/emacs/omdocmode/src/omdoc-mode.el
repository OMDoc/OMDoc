;;; omdoc-mode.el --- Major mode for composing/editing/looking at OMDoc files.

;; Author: Peter Jansen <pjj@cs.cmu.edu>
;; Maintainer: Peter Jansen <pjj@cs.cmu.edu>
;; Contacts: pjj@cs.cmu.edu, kohlhase+@cs.cmu.edu
;; Keywords: OMDoc major-mode (Open Mathematical Documents).
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

;; History:
;;   Originally based on html-helper-mode.el by Nelson Minar
;;    <nelson@santafe.edu>, and modified for OMDoc constructs.
;;   Later additions/modifications are borrowing from xslide.el
;;   and other modes.
;;   For more information of a general nature: see the README.TXT
;;    file in the documentation.

;; Installation:
;;   Add this line in your .emacs:
;;    (autoload 'omdoc-mode "omdoc-mode" "OMDoc" t)
;;   To invoke omdoc-mode automatically on .omdoc files, do this:
;;    (setq auto-mode-alist (cons '("\\.omdoc$" . omdoc-mode) auto-mode-alist))
;;   Note: like HTML-helper-mode, this mode requires tempo.el. If this
;;     was not part of your emacs distribution, you can retrieve it from
;;     ftp://ftp.lysator.liu.se/pub/emacs/tempo.el
;;   See the file instructions.txt for additional details.

;; Description:
;;   omdoc-mode is intended to make it easier to write and look at 
;;   OMDoc documents:
;;   - Fonts (via the font-lock mechanism), each with their separate color,
;;     indicate comments, strings, and each of the different tag types
;;     defined in the OMDoc DTD. This helps with browsing and reviewing
;;     OMDoc files.
;;   - OMdoc constructs can be inserted via keybindings or menu options
;;   - For some of the required attribute fields, empty strings are inserted
;;     which can be visited via the tempo mechanism
;;       (<CTRL><ALT> f and <CTRL><ALT> b)
;;   - New documents start from an OMDoc skeleton
;;   - Commands are available to indent OMDoc documents (per line, per
;;     element, or per buffer).
;;   - There are commands to validate or syntax-check OMDoc documents
;;   - Completion or tags / elements. Etc.
;;   For a more complete/detailed overview, see the file functionality.txt
;;   in the documentation.

;; Known bugs / future work etc.
;;
;;  This release is provided as is, and could be considered experimental
;;  even though it has been tested by fairly experienced OMDoc users
;;  in our group (e.g. Kohlhase).
;;  Input (suggestions, feedback, improvements, code...) is welcomed,
;;  and will be gratefully acknowledged in future releases.
;;  
;;  In the mean time, there are several issues on our todo-list that
;;  will eventually appear in the code. A partial list is given in the
;;  buglist.txt file that comes with the documentation.
;;

;;}}}

;;{{{ Code:

;(defconst omdoc-mode-version "0.8")

;;{{{ Dependencies

(require 'omdoc-config "omdoc-config")
(require 'omdoc-timestamp "omdoc-timestamp")
(require 'omdoc-templates "omdoc-templates")
(require 'omdoc-font "omdoc-font")
(require 'omdoc-process "omdoc-process")
(require 'tempo)
(require 'easymenu)

;;}}}

;;{{{ omdoc-mode-syntax-table and omdoc-mode-abbrev-table

;; Give < and > matching semantics

(defvar omdoc-mode-syntax-table nil
  "Syntax table for OMDoc.")

(if omdoc-mode-syntax-table
    ()
  (setq omdoc-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " omdoc-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " omdoc-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " omdoc-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " omdoc-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " omdoc-mode-syntax-table))

(defvar omdoc-mode-abbrev-table nil
  "Abbrev table used while in omdoc-mode.")
(define-abbrev-table 'omdoc-mode-abbrev-table ())

;;}}}

;;{{{ indentation

(defun omdoc-test-message (msg)
  (if omdoc-debug
      (progn
	(message msg)
	(sit-for omdoc-debug-pause))))

(defun omdoc-indent-command ()
  "Command for indenting OMDoc to the appropriate column.
Calls `omdoc-indent-line' which tries to examine how many levels down
in nested tags we are and does the appropriate indentation."
  (interactive)
  (omdoc-indent-line))

(defun omdoc-indent-line ()
  "Indent current line as OMDoc code."
; consider extending to take an argument to rigidly indent a number of lines
  (interactive)
  (if omdoc-never-indent
      ()
    (let* ((tagl (omdoc-start-of-current-tag))
	   (tagpos (nth 0 tagl))
	   (tagindent1 (nth 4 tagl))
	   (tag (nth 2 tagl))
	   (tagindent2 (nth 3 tagl))
	   (init-indent 0)
	   (m (point-marker))
	   (bol (progn (beginning-of-line) (point))))
      (delete-region bol (progn (skip-chars-forward " \t")(point)))
      (if tagl ; we are presumably in the middle of a tag.
	  (progn
	    (omdoc-test-message (format "In tag: %s %s %s %s"
					tag tagpos tagindent1 tagindent2))
	    (if tagindent2
		(indent-to tagindent2)
	      (indent-to tagindent1)); this part appears to work
	    )
;	  (indent-to tagindent1)
	; otherwise -- this line is normal start. Use normal indent.
	(omdoc-test-message "Doing regular indent")
	(if (looking-at "</\\(\\w+\\)") ; close tag: indent back
	    (let ((tag (match-string-no-properties 1)))
	      (omdoc-test-message
	       (format "Looking at close tag for %s. Adding following to indent: %d" tag (- (omdoc-tag-indent tag))))
	      (setq init-indent (- (omdoc-tag-indent tag)))))
	(backward-char)
	(let ((newindent (omdoc-calculate-indent init-indent)))
	  (omdoc-test-message (format "From calculating indent with param %d we got indent %d" init-indent newindent))
	  (if (< newindent 0) (setq newindent 0))
	  (forward-char)
	  (indent-to newindent)))

	;; adjust point to where it was before, or at start of indentation
      (goto-char (marker-position m))
      (if (< (current-column) (current-indentation))
	  (back-to-indentation)))))
	
(defun omdoc-closing-follows-p ()
  "If the next thing is a closing tag, return tag. Else return nil."
  (save-excursion
    (save-match-data
      (skip-chars-forward " \t")
      (if (looking-at "</\\(\\w+\\)")
	  (match-string-no-properties 1)))))

(defun omdoc-in-both-tag-p ()
  "Return t if we are in a 'both' tag. Else return nil."
  (save-excursion
    (save-match-data
      (forward-char)
      (if (looking-at "<") (forward-char))
      (re-search-forward "[^&][<>]" nil t)
      (backward-char 2)
      (looking-at "/>"))))

; need to generalize this to set of functions on any region
;   based on pattern data on init, end, conditions, and
;   information to be extracted
(defvar omdoc-cdata-init "<!\\[CDATA\\[")
(defvar omdoc-cdata-final "\\]\\]>")
(defvar omdoc-cdata-both (concat "\\(" omdoc-cdata-init "\\|"
				 omdoc-cdata-final "\\)"))

(defun omdoc-next-cdata (limit)
  "Set match-data to beginning and end of next cdata if any (for font lock)."
  (interactive)
;  (save-excursion
    (if (re-search-forward omdoc-cdata-init limit t)
	(let ((cbeg (match-beginning 0)))
	  (when (re-search-forward omdoc-cdata-final limit t)
	    (let ((l (match-data)))
	      (setcar l cbeg)
	      (set-match-data l)
	      (match-data))
	      ))))
;)
  
(defun omdoc-current-cdata ()
  "Return beginning and end of current 'cdata' segment. Else return nil."
  (interactive)
  (let (cbeg cend
	(currentpoint (point)))
    (save-excursion
      (save-match-data
	(forward-char)
	(search-backward "<" 0 t)
	(if (looking-at omdoc-cdata-init)
	    (setq cbeg (match-beginning 0))
	  (re-search-backward omdoc-cdata-both 0 t)
	  (if (looking-at omdoc-cdata-init)
	    (setq cbeg (match-beginning 0))))
	(when cbeg
	  (forward-char)
	  (if (re-search-forward omdoc-cdata-final nil t)
	      (setq cend (match-end 0)))
	  (list cbeg cend))))))

; function for testing
(defun omdoc-blink-current-cdata ()
  "Blink any cdata region we might be in."
  (interactive)
  (let* ((matchl (omdoc-current-cdata))
	 (beg (car matchl))
	 (end (cadr matchl)))
    (if (and beg end)
	(omdoc-blink beg end))))

(defun omdoc-start-of-current-tag ()
  "If the beginning of the current line is in a tag, return starting point."
  (interactive)
  (omdoc-test-message "In start-of-current-tag")
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (let ((tagpos (re-search-backward "\\([^&]\\|^\\)[<>]" 0 t)))
	(if (looking-at ".?<\\(\\w+\\)") ; we are in a tag
	    (list tagpos (match-beginning 1)
		  (match-string-no-properties 1)
		  (+ 1 (current-column) (match-end 1) (- tagpos))
		  (+ (current-column) (match-beginning 1) (- tagpos))
		     ))))))

(defun omdoc-tag-indent (tag)
  (omdoc-test-message (concat "In omdoc-tag-indent with tag: " tag))
  (let ((ti (cdr (assoc tag omdoc-indent-alist))))
    (if ti ti
      omdoc-indent-default)))

(defun omdoc-calculate-indent (arg)
  "Find indentation to be added to get to this point."
  (interactive "P")
; argument can be dropped, but will worry about that later
  (let ((init-indent (if arg arg 0)))
    (omdoc-test-message (format "In calculate-indent with init-indent: %d" init-indent))
    (save-excursion
     (save-match-data
      (let ((tagpos (re-search-backward "\\([^&]<.\\|\n\\)" 0 t)))
       (save-match-data (if (looking-at "\n\n")
			    (omdoc-calculate-indent init-indent)
	  (forward-char) ; go to beginning of tag (or to line we came from)
	  (if (or (not tagpos)
		  (and (equal (match-string-no-properties 1) "\n")
		       (looking-at "<")))
;		       (equal (current-column) 0)))
	      (prog1 init-indent
		(omdoc-test-message "hitting newline or top"))
	    (if (looking-at "</\\(\\w+\\)") ; we are in a closing tag
					; do whatever we do with closing tags
		(progn
		  (omdoc-test-message (format "calc_indent: closing tag %s"
					      (match-string-no-properties 1)))
					;		(omdoc-calculate-indent init-indent))
		  (omdoc-calculate-indent
		   (- init-indent
		      (omdoc-tag-indent (match-string-no-properties 1)))))
	      (if (looking-at "<\\(\\w+\\)") ; we are in an open or both tag
					;do whatever we do with open tags
		  (progn
		    (omdoc-test-message (format "calc_indent: open tag %s"
						(match-string-no-properties 1)))
		    (if (omdoc-in-both-tag-p)
			(progn
			  (omdoc-test-message "BOTH: no additional")
			  (omdoc-calculate-indent init-indent))
		      (omdoc-test-message "OPEN: add")
		      (omdoc-calculate-indent
		       (+ init-indent
			  (omdoc-tag-indent
			   (match-string-no-properties 1))))))
					;	(looking-at "\n") -- at beginning of line
		(omdoc-test-message "calc_indent: probably a newline.")
		(let* ((tagl (omdoc-start-of-current-tag))
		       (tag (nth 2 tagl))
		       (tagpos (car tagl)))
		  (if tag ; we need to continue.
		      (progn
			(goto-char tagpos)
			(omdoc-test-message (format "calc_indent: in tag(?) %s" tag))
;		      (insert (concat tag ":" (omdoc-tag-indent tag) " "))
			(if (omdoc-in-both-tag-p)
			    (progn
			      (omdoc-test-message "BOTH: no additional")
			      (omdoc-calculate-indent init-indent))
			  (omdoc-test-message "OPEN: add")
			  (omdoc-calculate-indent
			   (+ init-indent (omdoc-tag-indent tag)))))
					; Not in the middle of a tag and valid indent: we can stop!
					; However, if the last thing we looked at was a closing tag,
					; we shouldn't count that indent!
		    (let ((tag (omdoc-closing-follows-p)))
		      (if tag
			  (progn
			    (omdoc-test-message
			     (format "undoing close tag %s" tag))
			    (setq init-indent
				  (+ init-indent (omdoc-tag-indent tag))))))
		    (omdoc-test-message (format "calc_indent: let's return! %d." (+ (current-indentation) init-indent)))
		    (+ (current-indentation) init-indent)))
		))))))))
))
;;}}}

;;{{{ timestamps and buffers

(defun omdoc-current-buffer ()
  "Return the string name of file belonging to current buffer."
  (buffer-name (current-buffer)))

(defun omdoc-current-title ()
  "Return the presumed title (first part of buffer name)."
  (let ((bname (omdoc-current-buffer)))
    (substring bname 0 (or (string-match "\\." bname)(length bname)))))

(defun omdoc-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`omdoc-timestamp-start', deletes all text up to
`omdoc-timestamp-end', and runs `omdoc-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward omdoc-timestamp-start nil t))
	(message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length omdoc-timestamp-start)))
	    (ts-end (if (search-forward omdoc-timestamp-end nil t)
			(- (point) (length omdoc-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'omdoc-timestamp-hook)))))
  nil)

(defun omdoc-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert (substring time -4)
	    "-"
	    (cdr (assoc (substring time 4 7) omdoc-month-alist))
	    "-"
;;;;;;******* this is stillwrong ****** gives " 7" instead of "07"
	    (substring time 8 10)
	    "T"
	    (substring time 11 19)
	    "Z"
)))

(defun omdoc-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert omdoc-timestamp-start)
  (insert omdoc-timestamp-end))

;;}}}
;;{{{ omdoc-insert-new-buffer-strings

(defun omdoc-insert-new-buffer-strings ()
  "Insert `omdoc-new-buffer-strings'."
  (tempo-template-omdoc-skeleton))

;;}}}

;;{{{ Functions for tree and paragraph manipulations

(defun omdoc-find-enclosing-open-tag ()
  "Find the minimally enclosing open tag."
  (interactive)
; assuming validity, just count open and close tags, and skip CDATA fields.
  (save-excursion
    (save-match-data
      ; first move to the enclosure of an open tag if we are on it.
      (if (null (looking-at "<"))
	  (re-search-backward "[<>]" 0 t))
;      (goto-char (match-beginning 0))
;      (omdoc-blink (point) (+ (point) 1) 0.5)
      (if (looking-at "<\\([^/ >]+\\)\\([ \t\n]+[^ >]+\\)*>") ; open tags only
	  (search-forward ">" nil t)
	(forward-char))
;      (omdoc-blink (point) (+ (point) 1) 0.5)
      (let ((tag-levels 1))
	(while (> tag-levels 0)
	  (if (null (search-backward ">" 0 t))
;	      (error "Warning: no enclosing open tag found")
	      (message "Warning: no enclosing open tag found")
;	    (omdoc-blink (point) (+ (point) 1) 0.2)
	    (backward-char 2)
	    (if (looking-at "\\]\\]>")
  ; if it's a CDATA field, skip over it.      
		(if (null (re-search-backward "<!\\[CDATA\\[" 0 t))
		    (message "Warning: could not find beginning of CDATA field"))
; else, find the beginning of the tag and determine its name and type
	      (forward-char 1) ; allow also 1-character tags.
	      (search-backward "<" 0 t)
;	      (omdoc-blink (point) (+ (point) 1))
	      (if (looking-at "<\\([^/ >]+\\)\\([ \t\n]+[^ >]+\\)*/>")
		  () ; do nothing with both-tags
		(if (looking-at "</\\([^/ >]+\\)>")
		    (if (or (null omdoc-top-level)
			     (string-match omdoc-main-tags-expr (match-string-no-properties 1)))
			(setq tag-levels (1+ tag-levels))) ; close tag: skip 1 more.
		  (if (looking-at "<\\([^/ >]+\\)\\([ \t\n]+[^ >]+\\)*>")
		      (save-match-data
			(if (or (null omdoc-top-level)
				 (string-match omdoc-main-tags-expr (match-string-no-properties 1)))
			    (setq tag-levels (- tag-levels 1)))) ; open tag
		    ))))))
	; at the beginning of the open tag: return point and tag name
	(list (match-beginning 0) (match-string-no-properties 1))))))

(defun omdoc-find-enclosing-close-tag (tag)
  "Find the corresponding enclosing close tag."
; assuming validity, just count open and close tags, and skip CDATA fields.  
  (save-excursion
    (save-match-data
; first, if we're in a tag, go towards the enclosure.
      (if (null (looking-at "<"))
	  (re-search-backward "[<>]" 0 t))
;      (goto-char (match-beginning 0))
;      (omdoc-blink (point) (+ (point) 1) 0.5)
;      (if (looking-at "<\\([^/ >]+\\)\\([ \t\n]+[^/ >]+\\)*>") ; open tags only
;      (if (looking-at "<\\([^/ >]+\\)") ; open or both tags
      (if (looking-at "<[^/ >!?]") ; open or both tags
	  (search-forward ">" nil t)
	(if (looking-at ">")
	    (forward-char)
	  (backward-char))) ; just in front of the closing/both tag
;      (omdoc-blink (point) (+ (point) 1) 0.5)
      (let ((tag-levels 1))
	(while (> tag-levels 0)
	  (forward-char)
	  (if (null (search-forward "<" nil t))
	      (message "Warning: no enclosing close tag found")
;	    (message (format "tag-levels: %d" tag-levels))
;	    (omdoc-blink (point) (+ (point) 1) 1)
	    (backward-char)
	    (if (looking-at "<!\\[CDATA\\[")
  ; if it's a CDATA field, skip over it.      
		(if (null (re-search-forward "\\]\\]>" nil t))
		    (message "Warning: could not find end of CDATA field"))
; else, check if it's a beginning or end tag of TAG
;	      (search-forward ">" 0 t)
;	      (omdoc-blink (point) (+ (point) 1))
;	      (if (looking-at (concat "<" tag "\\( *[^ >]+\\)*/>"))
	      (if (looking-at (concat "<" tag "\\( [^>/]+\\)?/>"))
		  () ; do nothing with both-tags
		(if (looking-at (concat "</" tag ">"))
		    (setq tag-levels (- tag-levels 1)) ; close tag: skip 1 more.
;		  (if (looking-at (concat "<" tag "\\( *[^ >]+\\)*>"))
		  (if (looking-at (concat "<" tag "\\( [^>]+\\)?>"))
		      (setq tag-levels (1+ tag-levels)) ; open tag
		    ))))))
	; at the beginning of the close tag: return final point
	(match-end 0)))))

; possible improvements: take indentation and edit distance into account
;   to find misspellings and missing (extra) tags

 (defun omdoc-compact-enclosing ()
   "Remove newlines and leading whitespace in the enclosing field."
   (interactive)
   (let* ((matchl (omdoc-find-enclosing-open-tag))
        (beg (car matchl))
        (tag (cadr matchl)))
     (if (and beg tag)
       (let ((end (omdoc-find-enclosing-close-tag tag)))
         (if end
           (save-restriction
             (save-excursion
               (goto-char beg)
               (narrow-to-region beg end)
               (while (re-search-forward "\n[ \t]*" nil t)
                 (replace-match " " nil t)))))))))

 (defun omdoc-unroll-enclosing ()
   "Split the enclosing field up to one tag per line."
   (interactive)
   (let* ((matchl (omdoc-find-enclosing-open-tag))
        (beg (car matchl))
        (tag (cadr matchl)))
     (if (and beg tag)
       (let ((end (omdoc-find-enclosing-close-tag tag)))
         (when end
           (save-restriction
             (save-excursion
               (goto-char beg)
               (narrow-to-region beg end)
               (while (re-search-forward ">[^\n]" nil t)
                 (backward-char)
                 (insert "\n")
                 )))
           (indent-region beg end nil))))))

(defun omdoc-blink-enclosing ()
  "Blink the enclosing field we are in."
  (interactive)
  (let* ((matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
    (if (and beg tag)
	(let ((end (omdoc-find-enclosing-close-tag tag)))
	  (if end
	      (omdoc-blink beg end 1))))))

(defun omdoc-indent-enclosing ()
  "Blink and indent the enclosing field we are in."
  (interactive)
  (let* ((matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
    (if (and beg tag)
	(let ((end (omdoc-find-enclosing-close-tag tag)))
	  (if end
	      (progn
		(omdoc-blink beg end 1)
		(indent-region beg end nil)))))))

(defun omdoc-blink-enclosing-main ()
  "Blink the enclosing top-level field we are in."
  (interactive)
  (let* ((omdoc-top-level t)
	 (matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
    (if (and beg tag)
	(let ((end (omdoc-find-enclosing-close-tag tag)))
	  (if end
	      (omdoc-blink beg end 1))))))

(defun omdoc-indent-enclosing-main ()
  "Blink and indent the enclosing top-level field we are in."
  (interactive)
  (let* ((omdoc-top-level t)
	 (matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
    (if (and beg tag)
	(let ((end (omdoc-find-enclosing-close-tag tag)))
	  (if end
	      (progn
		(omdoc-blink beg end 1)
		(indent-region beg end nil)))))))

(defun omdoc-insert-closing-and-indent (tag)
  "Insert a closing </TAG> on a separate line, and indent."
  (save-excursion
    (save-match-data
;      (beginning-of-line)
;      (when (null (looking-at "[ \t]*\="))
;	(goto-char (point))
;	(insert "\n"))
;      (insert (concat "</" tag ">"))
      (insert (concat "\n</" tag ">"))
      (omdoc-indent-command)
      (when (null (looking-at "[ \t]*$"))
	(insert "\n")))
))

; find the context, move appropriately, find the enclosing tag,
;  insert the closing tag, and indent
(defun omdoc-insert-current-closing-and-indent ()
  "Insert a closing tag on a separate line, and indent."
  (interactive)
  (save-excursion
    (save-match-data
; if inside a cdata, move past it
  (let* ((cdl (omdoc-current-cdata))
	 (cde (cadr cdl)))
    (if cde (goto-char (+ cde 1))))
; else if inside an open tag, move before it
; or if inside a closing or both tag, move past it
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (if (eq curtagtype 'open)
	      (goto-char (- curtagstart 1))
	    (goto-char (+ curtagend 1))))))
; find the enclosing open tag
  (let* ((matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
    (if (and beg tag
	     (null (looking-at (concat "\[ \\t\\n\]*</" tag))))
;    (if (and beg tag)
	(omdoc-insert-closing-and-indent tag))))
))
; insert the corresponding closing tag (for now without checking).

(defun omdoc-insert-closing-tag-and-indent ()
  "Insert the closing tag for the current open tag."
  (interactive)
  (let* ((matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
;    (save-excursion
;      (save-match-data
	(re-search-backward "[<>]")
	(if (looking-at ">") (forward-char))
	(if (and beg tag
	     (null (looking-at (concat "\[ \\t\\n\]*</" tag))))
;(concat "[ \t\n]*</" tag))))
	    (omdoc-insert-closing-and-indent tag))))
;	(let ((end (omdoc-find-enclosing-close-tag tag)))
;	  (if end
;	      (omdoc-blink beg end 1)))
;))

(defun omdoc-insert-main-closing-and-indent ()
  "Insert the closing tag for the current open main tag."
  (interactive)
  (let* ((matchl (omdoc-find-enclosing-open-tag))
	 (beg (car matchl))
	 (tag (cadr matchl)))
;    (save-excursion
;      (save-match-data
	(re-search-backward "[<>]")
	(if (looking-at ">") (forward-char))
	(if (and beg tag
		 (null (looking-at (concat "\[ \\t\\n\]*</" tag))))
;(concat "[ \t\n]*</" tag))))
	    (omdoc-insert-closing-and-indent tag))))
;	(let ((end (omdoc-find-enclosing-close-tag tag)))
;	  (if end
;	      (omdoc-blink beg end 1)))
;))

(defun omdoc-electric-slash ()
  "Function called when \"/\" is pressed in OMDoc mode."
  (interactive)
;  (omdoc-indent-line)
  (insert "/")
  (if (looking-at "$")
      (let ((element-name
	     (save-excursion
	       (backward-char 2)
	       (if (looking-at "</")
		   (omdoc-find-enclosing-open-tag) ; place / tag
		 nil))))
	(if element-name
	    (progn
	      (insert (cadr element-name))
	      (insert ">")
	      (omdoc-indent-command)
	      (if font-lock-mode
		  (save-excursion
		    (font-lock-fontify-region
		     (car element-name)
		     (point)))))))))

;;}}}

;;{{{ Functions for finding and going to matching tags, if any

(defun omdoc-current-tag ()
  "Determine the tag we are on and its type (open or close)."
  (interactive)
; need to check for quoted occurrences and eliminate
; (princ
  (save-excursion
    (save-match-data
      (or (looking-at "<")
	  (re-search-backward "[<>]"))
      (if (looking-at "<\\(\\w+\\)\\( [^>]*\\)?/>")
	  (list (match-string-no-properties 1) 'both
		(match-beginning 0)(match-end 0))
	(if (looking-at "<\\(\\w+\\)\\( [^>]*\\)?>")
	    (list (match-string-no-properties 1) 'open
		  (match-beginning 0)(match-end 0))
	  (if (looking-at "</\\(\\w+\\)\\( [^>]*\\)?>")
	      (list (match-string-no-properties 1) 'close
		    (match-beginning 0)(match-end 0))
	    )))))
;  )
)

(defun omdoc-find-open-tag ()
  (interactive)
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (if (eq curtagtype 'close)
	      (let ((opentagl (omdoc-find-open-tag1 curtag 1)))
		(if opentagl
		    (let ((opentagstart (nth 0 opentagl))
			  (opentagend (nth 1 opentagl)))
		      (omdoc-blink opentagstart opentagend)))))))))

(defun omdoc-find-open-tag1 (tag depth)
  "Back out of DEPTH levels of tag TAG by looking back for open tags."
; ignoring quotes for now
  (save-excursion
    (save-match-data
      (if (re-search-backward (concat "</?" tag "\\( [^>]*[^>/]\\)?>"))
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    (if (looking-at "</")
		(omdoc-find-open-tag1 tag (+ depth 1))
	      (if (<= depth 1) ;found it!
		  (list beg end)
		(omdoc-find-open-tag1 tag (- depth 1)))))))))

(defun omdoc-find-close-tag ()
  (interactive)
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (if (eq curtagtype 'open)
	      (let ((closetagl (omdoc-find-close-tag1 curtag 1)))
		(if closetagl
		    (let ((closetagstart (nth 0 closetagl))
			  (closetagend (nth 1 closetagl)))
		      (omdoc-blink closetagstart closetagend)))))))))

(defun omdoc-find-close-tag1 (tag depth)
  "Back out of DEPTH levels of tag TAG by looking forward for close tags."
; ignoring quotes for now
  (save-excursion
    (save-match-data
      (if (re-search-forward (concat "</?" tag "\\( [^>]*[^>/]\\)?>"))
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    (goto-char (+ beg 1))
	    (if (looking-at tag)
		(omdoc-find-close-tag1 tag (+ depth 1))
	      (if (<= depth 1) ;found it!
		  (list beg end)
		(omdoc-find-close-tag1 tag (- depth 1)))))))))

(defun omdoc-find-matching-tag ()
  (interactive)
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (if (eq curtagtype 'open)
	      (save-excursion
		(forward-char)
		(let ((closetagl (omdoc-find-close-tag1 curtag 1)))
		  (if closetagl
		      (let ((closetagstart (nth 0 closetagl))
			    (closetagend (nth 1 closetagl)))
			(omdoc-blink closetagstart closetagend)))))
	    (if (eq curtagtype 'close)
		(let ((opentagl (omdoc-find-open-tag1 curtag 1)))
		  (if opentagl
		      (let ((opentagstart (nth 0 opentagl))
			    (opentagend (nth 1 opentagl)))
			(omdoc-blink opentagstart opentagend))))
	      (if (eq curtagtype 'both)
		  (omdoc-blink curtagstart curtagend))))))))

(defun omdoc-goto-matching-tag ()
  (interactive)
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (if (eq curtagtype 'open)
	      (let ((gc (save-excursion
			  (forward-char)
			  (car (omdoc-find-close-tag1 curtag 1)))))
		(if gc (goto-char gc)))
	    (if (eq curtagtype 'close)
		(let ((gc (car (omdoc-find-open-tag1 curtag 1))))
		  (if gc (goto-char gc)))))))))

(defun omdoc-blink (start end &optional delay hface)
  "Blink the region between START and END for a duration of TIME seconds."
  (save-excursion
    (setq delay (or delay 0.8))
    (setq hface (or hface 'highlight))
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'face hface)
      (goto-char start)
      (sit-for delay)
      (delete-overlay ov))))

(defun omdoc-blink-current-tag ()
; todo: check if within visible buffer. If not, display instead in minibuffer
  (interactive)
  (let ((curtagl (omdoc-current-tag)))
    (if curtagl
	(let ((curtag (car curtagl))
	      (curtagtype (nth 1 curtagl))
	      (curtagstart (nth 2 curtagl))
	      (curtagend (nth 3 curtagl)))
	  (omdoc-blink curtagstart curtagend)))))

;;}}}
;;{{{ type based keymap and menu variable and function setup

;; type based keymaps and element-insertion commands are defined
;; in omdoc-template.el

;; special mode keys
(mapcar
 (function (lambda (l) (define-key omdoc-mode-map (car l) (nth 1 l))))
 '(
; tempo commands
   ("\M-\C-f"  tempo-forward-mark)
   ("\M-\C-b"  tempo-backward-mark)
   ("\M-\t"    tempo-complete-tag)
; validation
   ("\C-cv"    omdoc-simple-validate)
   ("\C-c\C-V" omdoc-validate)
   ("\C-c`"    omdoc-next-error)
; navigation and visualisation
   ("\C-c\C-c" omdoc-goto-matching-tag)
   ("\C-cm"    omdoc-find-matching-tag)
   ("\C-c\C-b" omdoc-blink-enclosing-main)
   ("\C-cb"    omdoc-blink-enclosing)
; unrolling and compacting
   ("\C-cu"    omdoc-unroll-enclosing)
   ("\C-cc"    omdoc-compact-enclosing)
; editing: finish open tags
   ("/"        omdoc-electric-slash)
; leaving the following undefined for now
;   ("\C-ci"    omdoc-insert-current-closing-and-indent)
;   ("\C-c\C-i" omdoc-insert-main-closing-and-indent)
;   ("\C-cI"    omdoc-insert-closing-tag-and-indent)
; re-fontify if things went wrong
   ("\C-c\C-f" font-lock-fontify-buffer)

   ("\C-ch" omdoc-htmlify)
  ))

;; indentation keys - only rebind these if the user wants indentation
(if omdoc-never-indent
    ()
  (define-key omdoc-mode-map "\t" 'omdoc-indent-command)
  (define-key omdoc-mode-map "\C-m" 'newline-and-indent)
  (define-key omdoc-mode-map "\C-c\C-q" 'omdoc-indent-enclosing-main)
  (define-key omdoc-mode-map "\C-cq" 'omdoc-indent-enclosing)
; while we're at it, do global indentation keys, so indent-buffer
;  and indent-region will work.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'omdoc-indent-line))

;; Extra commands that OMDoc mode supports that aren't insertions
;; leftover from html-helper mode, mostly unused.
(defvar omdoc-mode-functions-map nil
  "Keymap for extra OMDoc mode functions")
(define-prefix-command 'omdoc-mode-functions-map)
(define-key omdoc-mode-map "\C-c\C-z"
  'omdoc-mode-functions-map)
(define-key omdoc-mode-functions-map "t"
  'omdoc-insert-timestamp-delimiter-at-point)

;;}}}
;;{{{ Major mode setup for the OMDoc mode

(defun omdoc-mode ()
  "Mode for editing OMDoc documents.

The main function omdoc-mode provides is a menu and keybindings
for the OMDoc tags one inserts when writing OMDoc documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, some tags are instead wrapped around the region.
Alternately, one can type part of the tag and complete it with M-TAB.

There is (or will be) also code for indentation, timestamps, skeletons
for new documents, etc..

\\{omdoc-mode-map}
 [Adapted from various modes (mainly html-helper-mode)
  by pjj@cs.cmu.edu]"
  (interactive)
  (kill-all-local-variables)

  (use-local-map omdoc-mode-map)
  (setq local-abbrev-table omdoc-mode-abbrev-table)
  (set-syntax-table omdoc-mode-syntax-table)

  (setq mode-name "OMDoc")
  (setq major-mode 'omdoc-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)

;  (setq debug-on-error t)
  (setq debug-on-error nil)

  ;; font-lock setup
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(omdoc-font-lock-keywords t t))
  (setq font-lock-maximum-size 2048000)

  (setq comment-start "<!-- "
	comment-end " -->"
	comment-start-skip "<!--[ \t]*"
	comment-column 0
	indent-line-function 'omdoc-indent-line)

  (tempo-use-tag-list 'omdoc-tempo-tags omdoc-completion-finder)
  
  (if omdoc-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'omdoc-update-timestamp))

  (if (and omdoc-build-new-buffer (zerop (buffer-size)))
      (omdoc-insert-new-buffer-strings))

  (easy-menu-add (omdoc-menu) omdoc-mode-map)

  (run-hooks 'text-mode-hook)
  (run-hooks 'omdoc-mode-hook))

;;}}}
;;{{{ Make the mode available

(provide 'omdoc-mode)
(run-hooks 'omdoc-load-hook)

;;}}}
;;; omdoc-mode.el ends here

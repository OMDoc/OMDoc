;;;; xslide.el --- XSL Integrated Development Environment
;; $Id: xslide.el 3883 2003-04-11 19:13:01Z kohlhase $

;; Copyright (C) 1998, 1999, 2000, 2001 Tony Graham

;; Author: Tony Graham <tkg@menteith.com>
;; Contributors: Simon Brooke, Girard Milmeister, Norman Walsh,
;;               Moritz Maass, Lassi Tuura, Simon Wright, KURODA Akira
;; Created: 21 August 1998
;; Version: $Revision: 3883 $
;; Keywords: languages, xsl, xml

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

;; Functions for editing XSL stylesheets

;; Requires xslide-font.el, xslide-data.el, xslide-abbrev.el, xslide-process.el
;; Requires 'etags for `find-tag-default'
;; Requires 'reporter for `xsl-submit-bug-report'
;; Requires 'imenu for "Goto" menu
;;
;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' for bug reports

;;;; Code:
(provide 'xslide)

(require 'cl)
(require 'compile)
(require 'font-lock)
;; XEmacs users don't always have imenu.el installed, so use
;; condition-case to cope if xslide causes an error by requiring imenu.
(eval-and-compile
  (condition-case nil
	(require 'imenu)
    (error nil)))
;; Need etags for `find-tag-default'
(require 'etags)

(require 'xslide-data "xslide-data")
(require 'xslide-abbrev "xslide-abbrev")
(require 'xslide-font "xslide-font")
(require 'xslide-process "xslide-process")


;; Define core `xsl' group.
(defgroup xsl nil
  "Major mode for editing XSL."
  :prefix "xsl-"
  :group 'languages)

(defgroup xsl-faces nil
  "Font faces used in XSL mode."
  :group 'xsl
  :group 'faces)

(defgroup xsl-process nil
  "Running XSL processors from XSL mode."
  :group 'xsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version information

(defconst xslide-version "0.2"
  "Version number of xslide XSL mode.")

(defun xslide-version ()
  "Returns the value of the variable xslide-version."
  xslide-version)

(defconst xslide-maintainer-address "xslide-bug@menteith.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar xsl-indent-tabs-mode nil
  "*Initial value of indent-tabs-mode on entering xsl-mode")

(defvar xsl-default-filespec "*.xsl"
  "*Inital prompt value for `xsl-etags''s FILESPEC argument.")

(defvar xsl-filespec-history (list xsl-default-filespec)
  "Minibuffer history list for `xsl-etags' and `xsl-grep''s FILESPEC argument.")

(defvar xsl-grep-pattern-history nil
  "Minibuffer history list for `xsl-grep''s PATTERN argument.")

(defvar xsl-grep-case-sensitive-flag nil
  "*Non-nil disables case insensitive searches by `xsl-grep'.")

(defvar xsl-comment-start "<!--"
  "*Comment start character sequence")

(defvar xsl-comment-end "-->"
  "*Comment end character sequence")

(defvar xsl-comment-max-column 70
  "*Maximum column number for text in a comment")

(defcustom xsl-initial-stylesheet-file (locate-library "xslide-initial.xsl" t)
  "*File containing initial stylesheet inserted into empty XSL buffers"
  :type '(choice (file :must-match t) (const :tag "No initial stylesheet" nil))
  :group 'xsl)

(defcustom xsl-initial-stylesheet-initial-point 0
  "*Initial position of point in initial stylesheet"
  :type '(integer)
  :group 'xsl)

(defcustom xsl-indent-attributes nil
  "*Whether to indent attributes on lines following an open tag.
If non-nil, attributes will be aligned with the space after the
element name, otherwise by two spaces."
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil))
  :group 'xsl)

(defcustom xsl-element-indent-step 2
  "*Amount by which to indent success levels of nested elements."
  :type '(integer)
  :group 'xsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun xsl-read-from-minibuffer (prompt default history)
  "Read from minibuffer with default and command history."
(let ((value nil))
  (if (string-equal
       ""
       (setq value
	     (read-from-minibuffer (if default
				       (format
					"%s(default `%s') "
					prompt default)
				     (format "%s" prompt))
				   nil nil nil
				   history)))
	     default
	     value)))

;; XSLIDE house style puts all comments starting on a favourite column
(defun xsl-comment (comment)
  "Insert COMMENT starting at the usual column.

With a prefix argument, e.g. \\[universal-argument] \\[xsl-comment], insert separator comment
lines above and below COMMENT in the manner of `xsl-big-comment'."
  (interactive "sComment: ")
  (insert "\n")
  (backward-char)
  (xsl-electric-tab)
  (let ((fill-column (1- xsl-comment-max-column))
	(fill-prefix (make-string (1+ (length xsl-comment-start)) ?\ ))
;;	(comment-start xsl-init-comment-fill-prefix)
	(saved-auto-fill-function auto-fill-function))
    (auto-fill-mode 1)
    (insert xsl-comment-start)
    (insert " ")
    (indent-to (length fill-prefix))
    (fill-region (point) (save-excursion
			   (insert comment)
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but it always ends with
    ;; an extra newline, so delete the newline.
    (delete-backward-char 1)
    (if (not saved-auto-fill-function)
	(auto-fill-mode 0))
    (insert " ")
    (insert xsl-comment-end)
    (insert "\n")
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-keywords-region
	   (xsl-font-lock-region-point-min)
	   (xsl-font-lock-region-point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map stuff

(defvar xsl-mode-map nil
  "Keymap for XSL mode.")

(if xsl-mode-map
    ()
  (setq xsl-mode-map (make-sparse-keymap))
  (define-key xsl-mode-map [tab]	  'xsl-electric-tab)
;;  (define-key xsl-mode-map "\M-\t"	  'xsl-complete)
  (define-key xsl-mode-map [(meta tab)]	  'xsl-complete)
;;  (define-key xsl-mode-map "\""   	  'xsl-electric-quote)
;;  (define-key xsl-mode-map "'"   	  'xsl-electric-apos)
  (define-key xsl-mode-map "/"   	  'xsl-electric-slash)
  (define-key xsl-mode-map "<"   	  'xsl-electric-less-than)
  (define-key xsl-mode-map ">"            'xsl-electric-greater-than)
;;  (define-key xsl-mode-map "["   	  'xsl-electric-lsqb)
;;  (define-key xsl-mode-map "("   	  'xsl-electric-lpar)
;;  (define-key xsl-mode-map "{"   	  'xsl-electric-lcub)
  (define-key xsl-mode-map [(control c) (control c)]
				   	  'xsl-comment)
  (define-key xsl-mode-map [(control c) (control p)]
				   	  'xsl-process)
  (define-key xsl-mode-map [(control o)]
				   	  'xsl-open-line)
  (define-key xsl-mode-map "\C-c<"  	  'xsl-insert-tag)
;;  (define-key xsl-mode-map [(control m)]	  'xsl-electric-return)
;;  (define-key xsl-mode-map \10	  'xsl-electric-return)
  (define-key xsl-mode-map "\177"	  'backward-delete-char-untabify)
;;  (define-key xsl-mode-map "\M-\C-e" 'xsl-next-rule)
;;  (define-key xsl-mode-map "\M-\C-a" 'xsl-previous-rule)
;;  (define-key xsl-mode-map "\M-\C-h" 'mark-xsl-rule)
)

(defun xsl-electric-greater-than (arg)
  "Insert a \">\" and, optionally, insert a matching end-tag.

If the \">\" closes a start-tag and the start-tag is the last thing on
the line, `xsl-electric-greater-than' inserts the matching end-tag.
Providing a prefix argument, e.g., \\[universal-argument] \\[xsl-electric-greater-than], stops the inserting of the
matching end-tag.

If the element being terminated is listed as a block element in
`xsl-all-elements-alist', then point is left on the next line at the
correct indent and the end-tag is inserted on the following line at
the correct indent.

`xsl-electric-greater-than' also fontifies the region around the
current line."
  (interactive "P")
  (insert ">")
  (if (and
       (not arg)
       (looking-at "$")
       (save-excursion
	 (let ((limit (point)))
	   (backward-char)
	   (search-backward "<")
;;	   (message "%s:%s" (point) limit)
	   (and
	    (looking-at "<\\(\\(\\sw\\|\\s_\\)+\\)\\(\\s-+\\(\\sw\\|\\s_\\)+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?\\)>")
;;	    (message "%s:%s" limit (match-end 0))
	    (= (match-end 0) limit)
;;	    (message ":%s:" (match-string 6))
	    (not (string-equal (match-string 6) "/"))
	    (not (save-match-data
		   (string-match "^/" (match-string 1))))))))
      (if (string-equal (nth 1 (assoc (match-string 1) xsl-all-elements-alist)) "block")
	  (progn
	    (xsl-electric-return)
	    (save-excursion
	      (insert "\n<")
	      (xsl-electric-slash)))
	(save-excursion
	  (insert (format "</%s>" (match-string 1))))))
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-region
	 (xsl-font-lock-region-point-min)
	 (xsl-font-lock-region-point-max)))))

(defun xsl-electric-apos ()
  "Function called when \"'\" is pressed in XSL mode"
  (interactive)
  (insert "'")
  (if (looking-at "\\([\"/})]\\|$\\)")
      (save-excursion
	(insert "'"))))

(defun xsl-electric-quote ()
  "Function called when '\"' is pressed in XSL mode"
  (interactive)
  (insert "\"")
  (if (looking-at "\\(['/})]\\|$\\)")
      (save-excursion
	(insert "\""))))

(defun xsl-electric-lsqb ()
  "Function called when \"[\" is pressed in XSL mode"
  (interactive)
  (insert "[")
  (if (looking-at "\\([\"'/})]\\|$\\)")
      (save-excursion
	(insert "]"))))

(defun xsl-electric-lpar ()
  "Function called when \"(\" is pressed in XSL mode"
  (interactive)
  (insert "(")
  (if (looking-at "\\([\]\"'/}]\\|$\\)")
      (save-excursion
	(insert ")"))))

(defun xsl-electric-lcub ()
  "Function called when \"{\" is pressed in XSL mode"
  (interactive)
  (insert "{")
  (if (looking-at "\\([\])\"'/}]\\|$\\)")
      (save-excursion
	(insert "}"))))

(defun xsl-electric-less-than ()
  "Function called when \"<\" is pressed in XSL mode"
  (interactive)
  (insert "<")
  (xsl-electric-tab))

(defun xsl-match-opening-tag (a)
  "Function called to match the next opening tag to a closing tag"
  (if (looking-at "</")
      (catch 'start-tag
        (while (re-search-backward
                (concat "\\(<\\|</\\)" a "[ \t\n\r>]") nil t)
          (cond
           ((looking-at (concat "</" a))
            (xsl-match-opening-tag a))
           ((looking-at (concat "<" a))
            (throw 'start-tag a))
           )))
    nil)
)
(defun xsl-electric-slash ()
  "Function called when \"/\" is pressed in XSL mode"
  (interactive)
  (insert "/")
  (xsl-electric-tab)
  (if (looking-at "$")
      (let ((element-name
	     (save-excursion
	       (backward-char 2)
	       (if (looking-at "</")
		   (catch 'start-tag
		     (while (re-search-backward "<" nil t)
		       (cond
			((looking-at "</\\([^/> \t]+\\)>")
;;			 (message "End tag: %s" (match-string 1))
; find matching tag:
                         (xsl-match-opening-tag (match-string 1)))
;;original
;;                       (re-search-backward
;;                        (concat "<" (match-string 1) "[ \t\n\r>]") nil t))
			((looking-at "<\\(\\([^/>]\\|/[^>]\\)+\\)/>"))
;;			 (message "Empty tag: %s" (match-string 1)))
			((looking-at "<!--[^-]*\\(-[^-]+\\)*-->"))
			;; skip CDATA sections
			((looking-at "<!\\[CDATA\\["))
			((looking-at "<\\([^/> \n\t]+\\)")
;;			 (message "Start tag: %s" (match-string 1))
			 (throw 'start-tag (match-string 1)))
			((bobp)
			 (throw 'start-tag nil)))))
		 nil))))
	(if element-name
	    (progn
	      (insert element-name)
	      (insert ">")
	      (if font-lock-mode
		  (save-excursion
		    (font-lock-fontify-region
		     (xsl-font-lock-region-point-min)
		     (xsl-font-lock-region-point-max)))))))))

(defun xsl-electric-return ()
  (interactive)
  (insert "\n")
  (xsl-electric-tab))

(defun xsl-open-line (arg)
  (interactive "p")
  (if (not arg)
      (setq arg 1))
  (save-excursion
    (while (> arg 0)
      (setq arg (1- arg))
      (insert "\n"))
    (if (looking-at "<")
	(xsl-electric-tab))))

(defun xsl-electric-tab ()
  "Function called when TAB is pressed in XSL mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (if (looking-at "</")
	(indent-to (max 0 (- (xsl-calculate-indent) xsl-element-indent-step)))
      (indent-to (xsl-calculate-indent))))
  (if (and
       (bolp)
       (looking-at "[ \t]+"))
      (goto-char (match-end 0)))
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-keywords-region
	 (xsl-font-lock-region-point-min)
	 (xsl-font-lock-region-point-max)))))


(defun xsl-close-open-tab-p nil
  "Return 'T' if the current line contains more right than left angle-brackets"
  (save-excursion
    (beginning-of-line)
    (let ((open 0))
      (while (not (eolp))
	(let ((here (char-after (point))))
	  (cond
	   ((eq here '?\>) (setq open (1- open)))
	   ((eq here '?\<) (setq open (1+ open)))
	   )
	  )
	(forward-char)
	)
      (< open 0)			; true if we've counted more
					; closes than opens
      )
    )
  )

(defun xsl-calculate-indent ()
  "Calculate what the indent should be for the current line"
  (interactive)
  (let* ((limit   (point))
 	 (name    "[^<>=\"' \t\n]+")
 	 (string  "\\(\"[^<>\"]*\"\\|'[^<>']*'\\)")
 	 (ostring "\\(\"[^<>\"]*\\|'[^<>']*\\)")
 	 (attval  (concat name "=" string))
 	 (oattval (concat name "=" ostring))
 	 (element (concat "<\\(" name "\\)"
 			  "\\([ \t\n]+" attval "\\)*"))
 	 (meta    (concat "<!\\(DOCTYPE\\|ENTITY\\)"
 			  "\\([ \t\n]+\\(" name "\\|" string "\\)\\)*")))
    (save-excursion
      (if (re-search-backward "^\\([ \t]*\\)<" nil t)
	  (goto-char (match-end 1))
	(beginning-of-line))
      (cond
       ;; closed comment => stay put
       ((save-excursion
 	  (re-search-forward "<!--[^-]*\\(-[^-]+\\)*-->" limit t))
 	(current-column))
       ;; open comment => indent by five
       ((looking-at "<!--")
 	(+ (current-column) 5))
       ;; end tag, closed empty tag, open tag immediately followed by
       ;; other tags/char data or a complete meta tag => stay put
       ((save-excursion
 	  (or (looking-at (concat "</" name ">"))
 	      (re-search-forward (concat element "/>") limit t)
 	      (re-search-forward (concat element ">[ \t]*[^\n]") limit t)
 	      (re-search-forward (concat meta ">[ \t]*\n") limit t)))
 	(current-column))
       ;; closed open tag followed by new line, or an opening meta tag
       ;; => indent by xsl-element-indent-step
       ((save-excursion
 	  (or (re-search-forward (concat element ">[ \t]*\n") limit t)
 	      (re-search-forward (concat meta "\\[[ \t]*\n") limit t)))
 	(+ (current-column) xsl-element-indent-step))
       ;; incomplete open tag or open meta => indent after tag name
       ((save-excursion
 	  (and (or (re-search-forward (concat element "[ \t\n]*") limit t)
 		   (re-search-forward (concat meta "[ \t\n]*") limit t))
 	       (= (point) limit)))
 	(if xsl-indent-attributes
 	    (progn (goto-char (match-end 1))
 		   (+ (current-column) 1))
 	  (+ (current-column) xsl-element-indent-step)))
       ;; incomplete attribute value => indent to string start
       ((save-excursion
 	  (and (or (re-search-forward (concat element "[ \t\n]+" oattval)
 				      limit t))
 	       (= (point) limit)))
 	(goto-char (match-beginning 4))
 	(+ (current-column) 1))
       ;; beginning of buffer => stay put (beginning of line)
       ((bobp)
 	(current-column))
       ;; otherwise => indent by xsl-element-indent-step
       (t
 	(+ (current-column) xsl-element-indent-step))))))

(defun xsl-complete ()
  "Complete the tag or attribute before point.
If it is a tag (starts with < or </) complete with allowed tags,
otherwise complete with allowed attributes."
  (interactive "*")
  (let ((tab				; The completion table
	 nil)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
;;    (message "%s" c)
    (cond
     ;; entitiy
;;     ((eq c ?&)
;;      (sgml-need-dtd)
;;      (setq tab
;;	    (sgml-entity-completion-table
;;	     (sgml-dtd-entities (sgml-pstate-dtd sgml-buffer-parse-state)))))
     ;; start-tag
     ((eq c ?<)
;;      (save-excursion
;;	(backward-char 1)
;;	(sgml-parse-to-here)
	(setq tab xsl-all-elements-alist))
     ;; end-tag
;;     ((eq c ?/)
;;      (save-excursion
;;	(backward-char 2)
;;	(sgml-parse-to-here)
;;	(setq tab (sgml-eltype-completion-table
;;		   (sgml-current-list-of-endable-eltypes)))))
     ;; markup declaration
;;     ((eq c ?!)
;;      (setq tab sgml-markup-declaration-table))
     ((eq c ? )
;;      (save-excursion
;;	(backward-char 1)
;;	(sgml-parse-to-here)
	(setq tab xsl-all-attribute-alist))
     (t
      (goto-char here)
      (ispell-complete-word)))
    (when tab
      (let ((completion (try-completion pattern tab)))
	(cond ((null completion)
	       (goto-char here)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((eq completion t)
	       (goto-char here)
	       (message "[Complete]"))
	      ((not (string= pattern completion))
	       (delete-char (length pattern))
	       (insert completion))
	      (t
	       (goto-char here)
	       (let ((res (completing-read
			   "xsl-complete: " tab nil t pattern)))
		 (insert
		  ;; insert completed strings(res) after pattern
		  (substring res (length pattern))))))))))

(defun xsl-insert-tag (tag)
  "Insert a tag, reading tag name in minibuffer with completion."
  (interactive 
   (list
    (completing-read "Tag: " xsl-all-elements-alist)))
  ;;  (xsl-find-context-of (point))
  ;;  (assert (null xsl-markup-type))
  ;; Fix white-space before tag
  ;;  (unless (xsl-element-data-p (xsl-parse-to-here))
  (skip-chars-backward " \t")
  (cond
   ((looking-at "^\\s-*$")
    (xsl-electric-tab))
   ((looking-at "^\\s-*</")
    (save-excursion
      (insert "\n"))
    (xsl-electric-tab))
   ((looking-at "$")
    (insert "\n")
    (xsl-electric-tab)))
  (let ((tag-type (nth 1 (assoc tag xsl-all-elements-alist))))
    (cond
     ((or
       (equal tag-type "block")
       (equal tag-type nil))
      (insert "<")
      (insert tag)
      (insert ">")
      (save-excursion
	(insert "\n")
	(xsl-electric-tab)
	(insert "<")
	(if (looking-at "<")
	    (progn
	      (insert "\n")
	      (backward-char)))
	(xsl-electric-slash)))
     ((equal tag-type "inline")
      (insert "<")
      (insert tag)
      (insert ">")
      (save-excursion
	(insert "</")
	(insert tag)
	(insert ">")))
     (t
      (insert "<")
      (insert tag)
      (save-excursion
	(insert "/>"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xsl-insert-template (match)
  "Insert a template"
  (interactive "smatch=")
  (xsl-electric-tab)
  (insert (format "<xsl:template match=\"%s\">\n" match))
  (xsl-electric-tab)
  (save-excursion
    (insert "\n<")
    (xsl-electric-slash)
    (insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table stuff

(defvar xsl-mode-syntax-table nil
  "Syntax table used while in XSL mode.")

(if xsl-mode-syntax-table
    ()
  (setq xsl-mode-syntax-table (make-syntax-table))
  ;; set the non-alphanumeric characters in XML names to
  ;; 'symbol constituent' class
  (modify-syntax-entry ?: "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?_ "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?- "_ 1234" xsl-mode-syntax-table)
  (modify-syntax-entry ?. "_" xsl-mode-syntax-table)
  ;; "-" is a special case because it is the first and second characters
  ;; of the start- and end-comment sequences.
  (modify-syntax-entry ?- "_ 1234" xsl-mode-syntax-table)
  ;; "%" does double duty in parameter entity declarations and references.
  ;; Not necessary to make "%" and ";" act like parentheses since the
  ;; font lock highlighting tells you when you've put the ";" on the
  ;; end of a parameter entity reference.
  (modify-syntax-entry ?% "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?\; "_" xsl-mode-syntax-table)
  ;; "/" is just punctuation in XSLs, and really only has a role in
  ;; Formal Public Identifiers
  (modify-syntax-entry ?/ "." xsl-mode-syntax-table)
  ;; Sometimes a string is more than just a string, Dr Freud.
  ;; Unfortunately, the syntax stuff isn't fussy about matching
  ;; on paired delimiters, and will happily match a single quote
  ;; with a double quote, and vice versa.  At least the font
  ;; lock stuff is more fussy and won't change colour if the
  ;; delimiters aren't paired.
  (modify-syntax-entry ?\" "$" xsl-mode-syntax-table)
  (modify-syntax-entry ?\' "$" xsl-mode-syntax-table)
  ;; The occurrence indicators and connectors are punctuation to us.
  (modify-syntax-entry ?| "." xsl-mode-syntax-table)
  (modify-syntax-entry ?, "." xsl-mode-syntax-table)
  (modify-syntax-entry ?& "." xsl-mode-syntax-table)
  (modify-syntax-entry ?? "." xsl-mode-syntax-table)
  (modify-syntax-entry ?+ "." xsl-mode-syntax-table)
  (modify-syntax-entry ?* "." xsl-mode-syntax-table)
  ;; `<' and `>' are also punctuation
  (modify-syntax-entry ?< "." xsl-mode-syntax-table)
  (modify-syntax-entry ?> "." xsl-mode-syntax-table)
  ;; "#" is syntax too
  (modify-syntax-entry ?# "_" xsl-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu stuff

(defun xsl-sort-alist (alist)
  "Sort an alist"
  (sort
   alist
   (lambda (a b) (string< (car a) (car b)))))

(defun xsl-imenu-create-index-function ()
  "Create an alist of elements, etc. suitable for use with imenu."
  (interactive)
  (let ((template-alist '())
	(mode-alist '())
	(key-alist '())
	(attribute-set-alist '())
	(name-alist '()))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:template\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      ;; Don't move point because the 'mode' attribute may be before
      ;; the 'match' attribute.
      (if (save-excursion
	    (re-search-forward
	     "match\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>" nil t)))
	     t))
	  (let* ((pattern (buffer-substring-no-properties
			   ;; Rely on the pattern that didn't match
			   ;; returning nil and on `or' evaluating the
			   ;; second form when the first returns nil.
			   (or
			    (match-beginning 2)
			    (match-beginning 3))
			   (or
			    (match-end 2)
			    (match-end 3))))
		 (pattern-position (or
				    (match-beginning 2)
				    (match-beginning 3))))
	    ;; Test to see if there is a 'mode' attribute.
	    ;; Match on either single-quoted or double-quoted attribute value.
	    ;; The expression that doesn't match will have return nil for
	    ;; `match-beginning' and `match-end'.
	    (if (save-excursion
		  (re-search-forward
		   "mode\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
		   (save-excursion
		     (save-match-data
		       (re-search-forward "<\\|>" nil t)))
		   t))
		(let* ((mode-name (buffer-substring-no-properties
				   ;; Rely on the pattern that didn't match
				   ;; returning nil and on `or' evaluating the
				   ;; second form when the first returns nil.
				   (or
				    (match-beginning 2)
				    (match-beginning 3))
				   (or
				    (match-end 2)
				    (match-end 3))))
		       (mode-name-alist (assoc mode-name mode-alist)))
		  (if mode-name-alist
		      (setcdr mode-name-alist
			      (list (car (cdr mode-name-alist))
				    (cons pattern pattern-position)))
		    (setq mode-alist
			  (cons
			   (list mode-name (cons pattern pattern-position))
			   mode-alist))))
	      (setq template-alist
		    (cons (cons pattern pattern-position)
			  template-alist)))))
      ;; When there's no "match" attribute, can still have "name"
      ;; attribute
      (if (save-excursion
	    (re-search-forward
	     "\\s-+name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>" nil t)))
	     t))
	  (setq name-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 name-alist))))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:attribute-set\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      (if (save-excursion
	    (re-search-forward
	     "name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>$" nil t)))
	     t))
	  (setq attribute-set-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 attribute-set-alist))))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:key\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      (if (save-excursion
	    (re-search-forward
	     "name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>$" nil t)))
	     t))
	  (setq key-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 key-alist))))
    (append
     (if key-alist
	 (list (cons "xsl:key" (xsl-sort-alist key-alist))))
     (if attribute-set-alist
	 (list (cons "xsl:attribute-set"
		     (xsl-sort-alist attribute-set-alist))))
     (if name-alist
	 (list (cons "name=" (xsl-sort-alist name-alist))))
     (if mode-alist
	 ;; Sort the mode-alist members, format the mode names nicely,
	 ;; and sort the templates within each mode.
	 (append
	  (mapcar (lambda (x)
		    (cons (format "mode=\"%s\"" (car x))
			  (xsl-sort-alist (cdr x))))
		  (xsl-sort-alist mode-alist))))
     (xsl-sort-alist template-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep stuff

;;;###autoload
(defun xsl-grep (pattern filespec)
  "Grep for PATTERN in files matching FILESPEC.

Runs `grep' with PATTERN and FILESPEC as arguments.

PATTERN is the pattern on which `grep' is to match.  PATTERN is quoted
with single quotes in the `grep' command arguments to avoid
interpretation of characters in PATTERN.  `xsl-grep' maintains a
history of PATTERNs so you can easily re-use a previous value.

FILESPEC is the names or regular expression for the files to be
scanned by grep.  Since `xsl-grep' uses `grep', regular expressions
and multiple filenames are supported, and \"*.xsl\" and \"*.XSL
*.ent\" are both valid FILESPEC values.

When called interactively, the initial FILESPEC is taken from
xsl-default-filespec, but `xsl-grep' also maintains a history of
FILESPEC arguments so you can easily re-use a previous value.  The
history is shared with `xsl-etags' so you can re-use the same FILESPEC
with both functions.
"
  (interactive
   (list
    (xsl-read-from-minibuffer "Pattern: "
			      (find-tag-default)
			      'xsl-grep-pattern-history)
    (xsl-read-from-minibuffer "Files: "
			      (car xsl-filespec-history)
			      'xsl-filespec-history)))
  ;; Include "--" in the command in case the pattern starts with "-"
  (grep (format "grep -n %s -- '%s' %s"
		(if (not xsl-grep-case-sensitive-flag)
		    "-i")
		pattern
		filespec)))


;;;###autoload
(defun xsl-mode ()
  "Major mode for editing XSL stylesheets.

Special commands:
\\{xsl-mode-map}
Turning on XSL mode calls the value of the variable `xsl-mode-hook',
if that value is non-nil.

Abbreviations:

XSL mode includes a comprehensive set of XSL-specific abbreviations
preloaded into the abbreviations table.

Font lock mode:

Turning on font lock mode causes various XSL syntactic structures to be 
hightlighted. To turn this on whenever you visit an XSL file, add
the following to your .emacs file:
  \(add-hook 'xsl-mode-hook 'turn-on-font-lock\)

Processing stylesheets:

\\[xsl-process] runs a shell command, in a separate process
asynchronously with output going to the buffer *XSL process*.  You can
then use the command \\[next-error] to find the next error message and
move to the line in the XSL document that caused it.

The first time that the program is run and whenever you provide a
prefix argument, e.g. \\[universal-argument] \\[xsl-process], prompts
for input filename, stylesheet file, and output filename.  Those
values are used with the templates in `xsl-process-command' to
populate this command's command history with the command lines to run
several XSLT processors using those values.  Use M-p and M-n to step
through the predefined commands, edit a command if necessary, or enter
a new command line.  The next time that this command is run, the
previously executed command is used as the default.

Searching multiple files:

To search multiple files, use \"\\[execute-extended-command] xsl-grep\" and supply the pattern to
search for and the specification of files to search in response to
the prompts.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map xsl-mode-map)
  (setq mode-name "XSL")
  (setq major-mode 'xsl-mode)
  (setq local-abbrev-table xsl-mode-abbrev-table)
  ;; XEmacs users don't all have imenu
  (if (featurep 'imenu)
      (progn
	;; If you don't have imenu, you'll get a "free variable"
	;; warning for imenu-create-index-function when you
	;; byte-compile, but not having imenu won't cause problems
	;; when you use xslide
	(setq imenu-create-index-function 'xsl-imenu-create-index-function)
	(setq imenu-extract-index-name-function 'xsl-imenu-create-index-function)
	(imenu-add-to-menubar "Templates")))
  ;; comment stuff
;;  (make-local-variable 'comment-column)
;;  (setq comment-column 32)
;;  (make-local-variable 'comment-start)
;;  (setq comment-start "; ")
;;  (make-local-variable 'comment-end)
;;  (setq comment-end "\n")
;;  (make-local-variable 'comment-start-skip)
;;  (setq comment-start-skip ";;* *")
  ;;
  ;; later we should move this into the xsl-mode-hook in
  ;; our local .emacs file
  ;; (abbrev-mode t)
  ;;
  ;; XSL font-lock highlighting setup
;;  (xsl-font-make-faces)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(xsl-font-lock-keywords t))
  (setq font-lock-mark-block-function 'xsl-font-lock-mark-block-function)
;;  (make-local-variable 'font-lock-defaults)
;;  (setq font-lock-defaults 
;;	'(xsl-font-lock-keywords nil t ((?- . "w")
;;					(?_ . "w")
;;					(?. . "w"))))
  ;; add an entry to compilation-error-regexp-alist for XSL
  ;; compiler errors
;;  (setq compilation-error-regexp-alist
;;	(cons '("file:/c:/projects/xslide/test.xsl:29:
;;XSL Error on line \\([0-9]*\\) in file \\(.*\\):$" 2 1)
;;	      compilation-error-regexp-alist))

  (set-syntax-table xsl-mode-syntax-table)
  ;; Maybe insert space characters when user hits "Tab" key
  (setq indent-tabs-mode xsl-indent-tabs-mode)
  (if (and
       xsl-initial-stylesheet-file
       (eq (point-min) (point-max)))
      (progn
	(insert-file-contents xsl-initial-stylesheet-file)
	(goto-char xsl-initial-stylesheet-initial-point)))
  (run-hooks 'xsl-mode-hook))


;;;; Bug reporting

(eval-and-compile
  (autoload 'reporter-submit-bug-report "reporter"))

(defun xsl-submit-bug-report ()
  "Submit via mail a bug report on XSLIDE."
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on XSL mode? ")
       (reporter-submit-bug-report
	xslide-maintainer-address
	(concat "xslide.el " xslide-version)
	(list 
	 )
	nil
	nil
     "Please change the Subject header to a concise bug description.\nRemember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.  Please remove these\ninstructions from your message.")
    (save-excursion
      (goto-char (point-min))
      (mail-position-on-field "Subject")
      (beginning-of-line)
      (delete-region (point) (progn (forward-line) (point)))
      (insert
       "Subject: XSLIDE version " xslide-version " is wonderful but...\n"))))


;;;; Last provisions
;;;(provide 'xslide)

;;; xslide.el ends here

;; $Id: nomdoc-lispsyntax.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-lispsyntax.el $

 ;; nomdoc-lispsyntax.el --- Enables lisp-like syntax of OpenMath formulae

;; Author and maintainer: Darko Pesikan <d.pesikan@jacobs-alumni.de>
;; Contacts: Michael Kohlhase <m.kohlhase@jacobs-university.de>, 
;;           Darko Pesikan <d.pesikan@jacobs-alumni.de>
;; Note: Mode derived from nXML mode
;; Keywords: nXML OMDoc (Open Mathematical Documents)
;; Created: April 2007

;; This file is NOT part of GNU Emacs

;; Copyright (C) 2007 Darko Pesikan

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'nomdoc-sanity "nomdoc-sanity")

(defvar warnings)
; hash table
(defvar symbol-to-cd)

(defun get-next-subexpr (expr)
 "Returns the next token"
  ; check whether the first character is (
  (if (= ?\( (string-to-char expr))
      (progn
	(let ((i 1) (index 1))
	  (while (and (> i 0) (string-match "\\((\\)\\|\\()\\)" expr index))
	    (if (match-string 1 expr) (setq i (+ i 1))
	      (setq i (- i 1)))
	    (setq index (match-end 0)))
	  (if (> i 0)
	      (error "Error in syntax"))
	  (string-match "[ \t\n]*" expr (match-end 0))
	  (substring expr 0 (match-end 0))))
    (if (string-match "^[^() \t\n]+[ \t\n]*" expr)
	(match-string 0 expr)
      nil)))

(defun parse-s-expression (expr)
  (let (tokens token)
    (if (string-match "^[ \t\n]*(" expr)
      (progn
	(setq expr (substring expr (match-end 0)))
	(while (and (null (string-match "^)[ \t\n]*$" expr)) 
		    (setq token (get-next-subexpr expr)))
	  (setq tokens (append tokens (list token)))
	  (setq expr (substring expr (length token))))

	(if (or (null token)
		(and (not (null (string-match "^)[ \t\n]*$" expr)))
		     (null tokens)))
	    (error "Error in syntax"))
	  
	tokens)

    (if (null (string-match "\\(^[^() \t\n]+\\)[ \t\n]*$" expr))
	(error "Error in syntax"))
    (match-string 1 expr))))

(defun lisp-to-openmath (expr is-first)
 "Takes a prefix lisp-like expression and translates it to OpenMath one line string."

 (let ((tokens (parse-s-expression expr)) pos CDs CD)
   ; is the expression is of form (a_1 a_2 ... a_n) then it's an OMA
   ; such that a_1 must be an OMS [if double or non existant, prompts the user]
   ; and a_x (1<x<=n) are OMI if integers, else they are OMS if found in the registry, 
   ; and otherwise they are simply OMVs
   (if (listp tokens)
       (let
	((result (concat "<OMA> " (lisp-to-openmath (car tokens) t)))
	 (i 1))
	(while (< i (length tokens))
	  (setq result (concat result (lisp-to-openmath (nth i tokens) nil))
		i (+ i 1)))
	(concat result "</OMA> "))

     (if (null (setq CDs 
		       (mapcar (lambda (x) 
				 (string-match "[^#]*#\\(.+\\)" x)
				 (match-string 1 x))
			       (gethash (sxhash tokens) symbol-to-cd))
		       ))
					; if symbol not found
	   (if is-first
	       (progn
		 (setq warnings t) ; we are about to make an invalid OpenMath (cd is "")
		 (concat "<OMS cd=\"\" name=\"" tokens "\"/> "))
	     (if (and (setq pos (string-match "-?[0-9]*\\(\\.[0-9]+\\)?$" tokens)) 
		      (= pos 0))
		 (if (match-string 1 tokens) ; float
		     (concat "<OMF dec=\"" tokens "\"/> ")
		   (concat "<OMI> " tokens " </OMI> ")) ; integer
	       ; not a number either, so it's must be a variable
	       (concat "<OMV name=\"" tokens "\"/> ")))
	 ; symbol FOUND!
	 (if (= (length CDs) 1)
	     (concat "<OMS cd=\"" (car CDs) "\" name=\"" tokens "\"/> ")
	   ; prompt the user to choose from more than one
	   (let ((answer (completing-read 
			 (concat "Symbol " tokens " ambiguous. Enter its CD (default "
				 (car CDs) "): ")
			 (mapcar (lambda (x) (list x nil)) CDs))))
		 (if (string= answer "")
		     (setq answer (car CDs)))
		 (concat "<OMS cd=\"" (car CDs) "\" name=\"" tokens  "\"/> ")))))))

(defvar regexp-processing-tag 
  "[ \t\n]*<\\?prefix[ \t\n]+\\([^?]*\\)\\?>"
  "Matches a math prefix S-Expression with its processing tag.")

(defun nomdoc-generate-om ()
 "Generates OpenMath chunk corresponding to a LISP 
expression in the current processing tag <?prefix ...?>."
 (interactive)
 (save-excursion
   (beginning-of-line)
   (if (null (looking-at regexp-processing-tag))
     (message "Not in <?prefix ...?> processing tag")
    ; if the chunk is outside theory, just empty hash table 
     (re-search-forward regexp-processing-tag)
     (let ((lisp-expr (match-string-no-properties 1))
	   (th-id (current-theory-begin)))
       (if th-id (setq symbol-to-cd (find-symbols-in-scope 
				     (fullify-path (nth 1 th-id) (buffer-file-name))))
	 (setq symbol-to-cd (make-hash-table)))

       (setq warnings nil)
       (condition-case nil
	   (progn
	     (insert 
	      "\n<OMOBJ xmlns=\"http://www.openmath.org/OpenMath\"> " 
	      (lisp-to-openmath lisp-expr t)
	      "</OMOBJ>")
	     (if warnings
		 (message "Some symbols nonexistent in the repository. Please fill in CDs where necessary.")))
	 (error (message "Invalid prefix expression.")))
       (beginning-of-line)
       (forward-char)
       (nomdoc-unroll-element)))))

(defun nomdoc-math-outline ()
 "Shows prefix lisp-like math only and hides the corresponding OpenMath."
 (interactive)
 (setq line-move-ignore-invisible t)
 (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "<\\?prefix[^?]*\\?>" nil t)
     (let* ((beg (progn
		   (re-search-forward "<OMOBJ " nil t)
		   (match-beginning 0)))
	    (end (progn 
		   (re-search-forward "</OMOBJ>" nil t)
		   (match-end 0))))
       (put-text-property beg end 'invisible t)))))

(defun nomdoc-math-show-all ()
 "Shows both prefix math and OpenMath."
 (interactive)
 (put-text-property (point-min) (point-max) 'invisible nil)
)
 
(provide 'nomdoc-lispsyntax) 


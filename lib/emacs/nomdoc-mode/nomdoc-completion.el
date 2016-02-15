;; $Id: nomdoc-completion.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-completion.el $

;;; nomdoc-completion.el --- Semantic completion for OpenMath

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

(defun complete-it (initial-input candidates)
 "Determines which of the candidates will be chosen 
  to complete the text in the given region and returns completion 
  or nil if no completion possible."
 (let (answer)
   (if (= (length candidates) 0) 
       (progn
	 (message "No completion possible.")
	 nil)
     (if (= (length candidates) 1)
	 (setq answer (car candidates))
       (setq answer
	     (completing-read
	      (concat "Enter some more to complete (default " (car candidates) "): ")
	      (mapcar (lambda (x) (list x nil)) candidates)
	      nil t initial-input))
       (if (string= answer "")
	   (setq answer (car candidates))))
					; deleting up-to-now entered text
     answer)))

(defun nomdoc-om-completion ()
 "Semantic completion for NAME and CD attributes of OMS elements."
 (interactive)
 (let ((theory (current-theory-begin)) path cpoint)
   ;if we are not in theory no semantic completion available
   (if (null theory)
       (message "Not inside valid theory element.")

     (setq path (fullify-path (nth 1 theory) (buffer-file-name)))
     ; completion for the literal OPEN-MATH
     (if (save-excursion
	   (beginning-of-line)
	   (and	(re-search-forward "\\=.*<" nil t)
		(progn (backward-char) t)
		(re-search-forward (concat "\\=" regexp-OMS) nil t)))
					; if we are inside OMS tag
	 (let* (  ; attributes might come in any order
		(i (if (string= (match-string 2) "name") 3 5))
		(name (match-string i))
		(cd (match-string (- 8 i)))
		(beg-name (match-beginning i))
		(mrk (match-beginning (- 8 i)))
		(symbols (find-symbols-in-scope-assoc path))
		(answer (complete-it name (all-completions name symbols))))

	   ; here we do some mark-point acrobatics, so that the user doesn't see the marked 
	   ; region. push-mark and pop-mark should be used here, but for some reason it 
	   ; doesn't work.
	   (unless (null answer)
	     (set-mark mrk)
	     (delete-region beg-name (+ (length name) beg-name))
	     (goto-char beg-name)
	     (insert answer)
	     (setq mrk (mark))
	     (set-mark nil)
	     (setq answer (complete-it cd (all-completions cd (mapcar (lambda (x) (list x nil)) 
					      (nth 1 (assoc answer symbols))))))
	     (unless (null answer)
	       (delete-region mrk (+ (length cd) mrk))
	       (goto-char mrk)
	       (insert answer))))
       
       ; completion for lisp-syntax
       (if (save-excursion 
	     (and
	      (setq cpoint (point))
	      (re-search-backward "(\\(.*\\)" nil t)
	      (= cpoint (match-end 0))))
	   
	   (let* ((name (match-string 1))
		  (beg-name (match-beginning 1))
		  (symbols (find-symbols-in-scope-assoc path))
		  (answer (complete-it name (all-completions name symbols))))
	     (unless (null answer)
	       (delete-region beg-name (+ (length name) beg-name))
	       (goto-char beg-name)
	       (insert answer)))
	       
	 (message "No valid OMS element in the current line found."))))))

(provide 'nomdoc-completion)

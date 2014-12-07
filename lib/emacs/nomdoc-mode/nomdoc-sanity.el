;; $Id: nomdoc-sanity.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-sanity.el $

;; nomdoc-sanity.el --- Performs semantic sanity check of theory-imports 

;; Author and maintainer: Darko Pesikan <dpesikan@gmail.com>
;; Contacts: Michael Kohlhase <m.kohlhase@jacobs-university.de>, 
;;           Darko Pesikan <dpesikan@gmail.com>
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

(require 'nomdoc-config "nomdoc-config")
(require 'nomdoc-util "nomdoc-util")

(defun regexp-theory-tag (&optional id)
"Matches the theory open tag"
 (concat 
  "<theory[ \t\n][^>]*xml:id[ \t\n]*=[ \t\n]*\"\\("
  (if (null id) "[^\"]+" id)
  "\\)\"[^>]*>"))

(defun regexp-symbol-tag (&optional name)
 "Matches the symbol open tag"
 (concat
  "<\\(symbol\\)[ \t\n][^>]*name[ \t\n]*=[ \t\n]*\"\\(" 
  (if (null name) "[^\"]+" name)
  "\\)\"[^>]*>"))

(defun regexp-imports-tag (&optional from) 
 "Matches the imports open tag"
 (concat
  "<\\(imports\\)[ \t\n][^>]*from[ \t\n]*=[ \t\n]*\"\\("
  (if (null from) "[^\"]+" from)
  "\\)\"[^>]*>"))

(defvar regexp-OMS
  (concat
   "<\\([^:]+:\\)?OMS[ \t\n][^>]*\\(name\\|cd\\)[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\""
                           "[^>]*\\(name\\|cd\\)[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\""
   "[^>]*/>")
 "Matches an OpenMath symbol usage. e.g. <OMS name=\"mapsto\" cd=\"sts\"/>")

(defun highlight (str)
  (propertize str 'face 'bold))

(defun displayize (theory path)
  (highlight (relativify-path theory path)))

(defun line-number (&optional pos)
 "Returns the line number where POS, or the point is."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defvar nomdoc-reg 
 (find-file-noselect (concat nomdoc-registry-dir "/ nomdoc-registry.xml"))
  "The registry buffer.")

(defvar dependency-graph 
 (let ((ht (make-hash-table)))
   (with-current-buffer nomdoc-reg
     (goto-char (point-min))
     (while (re-search-forward (regexp-theory-tag) nil t)
       (setq path (match-string 1))
					; find the end of this file's block
       (setq end 
	     (save-excursion (re-search-forward "</theory>" nil t)))
       (setq tlist nil)
       (while (re-search-forward (regexp-imports-tag) end t)
	 (add-to-list 'tlist (match-string 2)))
       (puthash (sxhash path) tlist ht)))
   ht)
 "The dependency graph for theories as a hash table. The KEY in it is a
  hashed value of string full_filepath#theory_name and the VALUE is a 
  list of strings of the same format such that the theory in KEY is directly
  dependent on the theories in VALUES. If no dependencies for a theory exist,
  it is still in the hashtable but its VALUE in nil. Created only once at 
  startup and updated when registry is updated (in nomdoc-update-registry)."
)

(defvar current-doc-buffer nil
  "The buffer of the main document under which the mode operates")

(defun expand-scope (theory warn-it)
"Takes a theory and returns a list of all the descendants of the theory 
(a theory is a descendant of itself here) reporting detected cycles if warn-it is t."
(let (visited (statuses (make-hash-table)))

  (defconst BLACK 0)
  (defconst GRAY 1)
  (defconst WHITE 2)

  (defun dfs-visit (expander) ; DFS 
    (puthash (sxhash expander) GRAY statuses)
    (let ((children (gethash (sxhash expander) dependency-graph)))
      (while (not (null children))
	(let ((status (gethash (sxhash (car children)) statuses WHITE)))
	  (if (and (= GRAY status) warn-it)
	      (insert (concat    ; into sanity-check buffer
		       (highlight "Warning:") " a cyclic path detected. One edge is "
		       (displayize expander (buffer-file-name current-doc-buffer)) " -> " 
		       (displayize (car children) (buffer-file-name current-doc-buffer)) ".\n"))
	    (if (= WHITE status)
		(dfs-visit (car children)))))
	(setq children (cdr children))))
    (puthash (sxhash expander) BLACK statuses)
    (add-to-list 'visited expander))

  (dfs-visit theory)
  visited))

(defun files-in-below-directory (directory ext)
 "Visits a directory recursively and returns a list of all files with extension ext in it."
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether the file is of appropriate type
       ;; and if so, append its name to a list.
       ((equal (concat "." ext) (substring (car (car current-directory-list))
					   (- -1 (length ext))))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        (unless
            (equal (or "." "..")
                   (substring (car (car current-directory-list)) -1))
          ;; descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)) ext)
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames 
   el-files-list))

(defun nomdoc-build-registry ()
 "Deletes the current version of the OMDoc files registry 
(if any) and creates a new, fresh version. Visits all 
OMDOC files from `nomdoc-file-paths', fetches the 
theories and symbols defined and stores them in the 
registry. Imports theories are also saved to 
enable scoping features and semantic consistency. 
See also `nomdoc-update-registry'."
 (interactive)
 (let ((paths nomdoc-file-paths))
  (set-buffer nomdoc-reg)
  (erase-buffer)
  ; add the main element
  (insert "<nomdoc-registry>\n")
  (while (not (null paths))  ; for each repository (path)
    (message (concat "Searching " (car paths) " for omdoc files..."))
    (let ((files (files-in-below-directory (car paths) "omdoc")))
     (while (not (null files)) ; for each omdoc file
       (if (file-readable-p (car files))
          (with-temp-buffer
           (insert-file-contents-literally (car files))
           ; adding symbols and the rest from the current file to registry
           (message (concat "Processing file " (car files) "..."))
           (fill-from-current-buffer (car files))
           (setq files (cdr files)))
        (message "File " (car files) " not readable!")
        (sit-for 0.5))))
    (setq paths (cdr paths)))
   (insert "</nomdoc-registry>\n")
   (save-buffer)))

(defun nomdoc-sanity-check-openmath ()
"Opens a buffer and displays all problems regarding the usage of 
 OMDoc theories as OpenMath Content Dictionaries"
  (interactive)
  (save-excursion
    (let
	(CD NAME war-hdr war-theories war-symbols
	    (war-buff (get-buffer-create " Warnings")))
      (with-current-buffer war-buff
	(setq buffer-read-only nil)
	(erase-buffer))
      (goto-char (point-min))
      (while (re-search-forward regexp-OMS nil t)

       ; setting CD and NAME of the found OMS usage
	(if (string= (match-string 2) "cd")
	    (setq CD (match-string 3) NAME (match-string 5))
	  (setq CD (match-string 5) NAME (match-string 3)))
	(setq war-hdr (highlight (concat "At line " (number-to-string (line-number)) ": ")))

	; only one warning per theory/symbol
	(unless (or (member CD war-theories) (member NAME war-symbols))
       
	  (with-current-buffer nomdoc-reg
	    (goto-char (point-min))
	    (if (re-search-forward (regexp-theory-tag (concat "[^\"]*#" CD)) nil t)
					; peek further to make sure there are no other choices
		(if (save-excursion (re-search-forward (regexp-theory-tag (concat "[^\"]*#" CD)) nil t))
		    (progn
		      (add-to-list 'war-theories CD)
		      (with-current-buffer war-buff
			(insert war-hdr "more than one theory with name " (highlight CD)
				" in the repository.\n")))
					; see whether the claimed symbol is indeed defined in theory CD
		  (save-excursion
		    (setq limit (re-search-forward "</theory>" nil t)))
		  (if (null (re-search-forward (regexp-symbol-tag NAME) limit t))
		      (with-current-buffer war-buff
			(add-to-list 'war-symbols NAME)
			(insert war-hdr "no symbol " (highlight NAME) " defined in theory " (highlight CD)
				" in the repository.\n"))))
	    
	    (with-current-buffer war-buff
	      (add-to-list 'war-theories CD)
	      (insert war-hdr "no theory in the repository for content dictionary " (highlight CD) ".\n"))))))
    
    (set-buffer war-buff)
    (if (= (line-number) 1)
	(progn
	  (message "All OMS elements have been checked and they seem fine.")
	  (kill-buffer war-buff))

      (pop-to-buffer war-buff)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (set-window-text-height nil (min 10 (count-screen-lines)))
      (pop-to-buffer current-doc-buffer)
))))

;;;;;; SEARCH ROUTINES ;;;;;;

(defun find-theories-used (beg end)
 "Generates a list of all CDs (theories) of used OMVs symbols in a given region"
 (save-excursion
   (let (CDs CD NAME (ht (make-hash-table)))
     (set-buffer current-doc-buffer)
     (goto-char beg)
     (while (re-search-forward regexp-OMS end t)
       (if (string= (match-string 2) "cd")
	   (setq CD (match-string 3) NAME (match-string 5))
	 (setq CD (match-string 5) NAME (match-string 3)))
       (with-current-buffer nomdoc-reg
	 (goto-char (point-min))
	 ; assuming there is only one theory and 
	 ; symbol NAME is indeed defined in theory CD
	 (if (re-search-forward (regexp-theory-tag (concat "[^\"]*#" CD)) nil t)
	     (progn
	       (setq CDs (cons (match-string 1) CDs))
	       (puthash (sxhash (match-string 1)) NAME ht)))))
     (list CDs ht))))

(defun find-theory-elements (beg end path)
 "Takes a region of the current (omdoc) file and returns all 
elements of concern defined there. It returns a hash table with KEY being
an element type (e.g. `symbol'), and VALUE a list of its names 
(or other attribute values of concern for types other than symbols)."

 (let ((table (make-hash-table)))
   (save-excursion
     (goto-char beg)
     (while (re-search-forward 
                (concat
                  "\\(" (regexp-symbol-tag) "\\)\\|"
                  "\\(" (regexp-imports-tag) "\\)")
            end t)
       (let*  ((type (sxhash (or (match-string-no-properties 2) 
                                 (match-string-no-properties 5)))) ; element types
	       (name (or (match-string-no-properties 3) 
                         (fullify-path (match-string-no-properties 6) path)))
	       (old (gethash type table)))
	 (puthash type (add-to-list 'old name) table))))
   table))

; used in nomdoc-lispsyntax
(defun find-symbols-in-scope (theory)
"Returns a hashtable where keys are all symbols accessible from
 THEORY and a value are all CDs where the corresponding symbol in key
 was defined. So the value is usually a list of size one."
 (nomdoc-update-registry)
 (let ((theories (expand-scope theory nil))
       (symbol-to-cd (make-hash-table)))
   (with-current-buffer nomdoc-reg
     (while (not (null theories))
       (goto-char (point-min))
       (let* ((beg (re-search-forward (regexp-theory-tag (car theories)) nil t))
 	      (end (re-search-forward "</theory>" nil t)))
	 (goto-char beg)
	 (while (re-search-forward (regexp-symbol-tag) end t)
	   (let ((old (gethash (sxhash (match-string 2)) symbol-to-cd)))
	     (add-to-list 'old (car theories))
	     (puthash (sxhash (match-string 2)) old symbol-to-cd))))
     (setq theories (cdr theories))))
   symbol-to-cd))

(defun find-symbols-in-scope-assoc (theory)
"Same as the above but returns an association list."
 (nomdoc-update-registry)
 (let ((theories (expand-scope theory nil))
        table)
   (with-current-buffer nomdoc-reg
     (while (not (null theories))
       (goto-char (point-min))
       (let* ((beg (re-search-forward (regexp-theory-tag (car theories)) nil t))
 	      (end (re-search-forward "</theory>" nil t))
	      (theory 
	       (progn
		 (string-match "[^#]*#\\(.+\\)" (car theories))
		 (match-string 1 (car theories)))))
	 (goto-char beg)
	 (while (re-search-forward (regexp-symbol-tag) end t)
	   (let ((CDs (assoc (match-string 2) table)))
	     (setq table (remove CDs table)) ; remove if it exists
	     (setq table (cons (list (match-string 2) (cons theory (nth 1 CDs))) table)))))

       (setq theories (cdr theories))))
   table))

(defvar a nil)
(defvar b nil)
(defvar da nil)
(defvar ta nil)
(defvar data nil)
(defvar more nil)

(setq table (list (list a (list da ta)) (list b (list more data))))

(setq list (assoc a table))

(setq list (cons 5 list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-from-current-buffer (path)
 "Fills the registry with the data we get from analysing the current buffer."
  (goto-char (point-min))
  (while (re-search-forward (regexp-theory-tag) nil t) ;for each module
     (let ((id (match-string 1))
	   theory-elements full-id
		;  definitions, axioms, and assertions should be added to tags too
	   (tags (list "imports" "symbol"))
	        ;  corresponding attributes we fetch for each tag respectively
	   (attrs (list "from" "name"))
	   (beg (point))
	   (end (current-theory-end)))
	(if end
	    (progn
	      (goto-char end)
	      (setq theory-elements (find-theory-elements beg end path)
		    full-id (fullify-path id path))
	      (with-current-buffer nomdoc-reg
		(insert " <theory xml:id=\"" full-id "\">\n")

		(while (not (null tags))
		  (let ((elements (gethash (sxhash (car tags)) theory-elements)))
		    ; update dependency graph too
		    (if (string= (car tags) "imports")
			(puthash (sxhash full-id) elements dependency-graph))
		    ; write elements of (car types) 
		    (while (not (null elements))
		      (insert "  <" (car tags) " " (car attrs) "=\"" (car elements) "\"/>\n")
		      (setq elements (cdr elements))))
		  (setq tags (cdr tags) attrs (cdr attrs)))

                (insert " </theory>\n")))
	  ; something got wrong, stop it
	  (goto-char (point-max)))
)))

(defun nomdoc-update-registry ()
 "Updates the registry for changes in the current 
buffer. This function is automatically executed each 
time a (possibly new) omdoc file is saved. It is also 
executed internally on a number of other occasions."
 (interactive)
 (let ((fn (buffer-file-name)))
   (with-current-buffer nomdoc-reg
     (goto-char (point-min))
     (next-line 1) ; skip the <nomdoc-registry>
     			; delete all theories from this file
                        ; all theories from one file are next to each other
     (if (re-search-forward (regexp-theory-tag (concat fn "[^\"]*")) nil t)
	 (let ((beg (match-beginning 0)))
	   (while (re-search-forward (regexp-theory-tag (concat fn "[^\"]*")) nil t))
	   (re-search-forward "</theory>" nil t)
	   (delete-region beg (point)))))
					; add new section
   (save-excursion
       (fill-from-current-buffer fn))
   (with-current-buffer nomdoc-reg
     (basic-save-buffer))))   

(defun current-theory-begin ()
  "Returns the location of the current theory's open tag and its name 
   or nil if we are not in theory. Doesn't ignore CDATA."
  (save-excursion
    (let ((i 1))
      (while (and (> i 0) (re-search-backward 
			    (concat "\\(</theory>\\)\\|\\(" (regexp-theory-tag) "\\)") nil t))
	(if (match-string 1) (setq i (+ i 1))
	  (setq i (- i 1))))
      (if (= i 0) (list (point) (match-string-no-properties 3))
	nil))))

(defun current-theory-end ()
  "Returns the location of the current theory's end tag or nil 
   if we are not in theory. Doesn't ignore CDATA."
  (save-excursion
    (let ((i 1))
      (while (and (> i 0) (re-search-forward 
			    (concat "\\(</theory>\\)\\|\\(" (regexp-theory-tag) "\\)") nil t))
	(if (match-string 1) (setq i (- i 1))
	  (setq i (+ i 1))))
      (if (= i 0) (point)
	nil))))

(defun fullify-path (relative full)
  "Given a relative path and the path to which it is relative, make the path full"
   (setq parts (split-string relative "#"))
   (cond           
    ((= 0 (length parts)) nil)
    ((= 1 (length parts))
       ; special case when the relative path is nil, 
       ;then just concatenate full path in front of filename
     (if (char-equal ?# (string-to-char relative)) (concat full relative)
       (concat full "#" relative)))
    (t ; there are two elements as: relative_path#theory_name
     (setq rel (car parts))
     (setq index 0)
     (while (string-match "\\.\\./" rel index)
       (setq index (+ 3 index)))
     
     (concat 
      (with-temp-buffer
        (insert full)
        (goto-char (point-max))
        (re-search-backward "/" nil t (+ (/ index 3) 1))
        (delete-region (+ 1 (point)) (point-max))
        (buffer-string))
      (substring rel index) "#" (nth 1 parts)))))

(defun relativify-path (full base)
 "Given a base path, shorten the full path. The inverse of function above."
  (if (string= full base) ""
    (setq index 0)
    (while (string= 
	    (progn 
	      (string-match "/[^/]*" full index)
	      (match-string 0 full))
	    (progn
	      (string-match "/[^/]*" base index)
	      (match-string 0 base)))
      (setq index (match-end 0)))

    (setq full (substring full (+ index 1)))
    (setq base (substring base (+ index 1)))

    ; file.omdoc#theory when base is file.omdoc, becomes #theory
    (if (and (setq index (string-match base full 0)) (= index 0))
	(setq full (substring full (match-end 0)))
      ; else, go down with .. is base contains more subdirectories
      (setq index 0)
      (while (string-match "[^/]*/" base index) 
	(setq index (match-end 0) 
	      full (concat "../" full))))
    full))

(defun set-difference (x y)
 "Finds difference x / y of two input sets x and y."
 (let (z)
   (while x
     (unless (member (car x) y) (setq z (cons (car x) z)))
     (setq x (cdr x)))
   z))

(defun set-intersection (x y)
 "Finds intersection x /\ y of two input sets x and y."
 (let (z)
   (while x
     (if (member (car x) y) (setq z (cons (car x) z)))
     (setq x (cdr x)))
   z))
   
(defun determine-uses (home-theories self &optional forced)
 "Given a bunch of theories (with representatives of their usage) determines 
an optimal imports section. It doesn't include theory 'self' to avoid a
situation where a theory uses itself. Optionally, it DOES include theory 'forced' 
regardless of whether it's needed of not. It returns an optimal list of theories, 
a hash table with representative symbols, and a hash table scope-ht with
descendants of theories."

 (let* ((theories (remove self (nth 0 home-theories)))
	sack
        (scope-ht (make-hash-table)))

   (unless (null forced) (add-to-list 'theories forced))
   (remhash (sxhash self) dependency-graph) ; !!!

   (while (not (null theories))
     (let* ((i 0) 
	    (head (car theories)) 
	    (descendants (expand-scope head t)))
       (puthash (sxhash head) descendants scope-ht)
       (setq theories (cdr theories))
       (while (< i (length descendants))
         (let ((del-mod (nth i descendants)))
          (setq
            theories (delete del-mod theories)
            sack (delete del-mod sack)
            i (1+ i))))
       (setq sack (cons head sack))))

   (unless (null forced) (add-to-list 'sack forced))
   (list sack (nth 1 home-theories) scope-ht)))

(defun sanity-check-fill (&optional emptyit &optional require-theory)
"Returns a sanity-check buffer filled with
 suggested changes from the current theory." 

(save-excursion
  (let* ((path (buffer-file-name))
	(temp (current-theory-begin))
  	  (beg (car temp))
     	  (theory-name (fullify-path (nth 1 temp) path))
	(end (current-theory-end))
	(ht-elements (find-theory-elements beg end path))
	(current-dependencies (gethash (sxhash "imports") ht-elements))
	(scb (get-buffer-create "*Sanity Check*")))

  ; creating a sanity-check buffer
    (setq current-doc-buffer (current-buffer))
    (pop-to-buffer scb)
    (setq buffer-read-only nil)
    (if emptyit (erase-buffer))

    (let*
      ((temp (determine-uses (find-theories-used beg end) theory-name require-theory))
          (optimal-dependencies (nth 0 temp))
          (rep-symbols (nth 1 temp))
          (scopes (nth 2 temp))
        (add (set-difference optimal-dependencies current-dependencies))
	(drop (set-difference current-dependencies optimal-dependencies))
	(theory-disp (displayize theory-name path)))

      (if (and (null add) (null drop))
	  (insert theory-disp ": imports elements seem to be optimal! Good.\n")
	; process DROPs
	(while (not (null drop))
	  (if (null (gethash (sxhash (car drop)) rep-symbols) )
					; so nothing is used
	      (let ((children (set-intersection (expand-scope (car drop) t) add)))
		(if (not (null children))
					; there are some adds that are descendants of (car drop)
					; so we do substitution
		    (let ((only-one (null (cdr children))))
		     (setq add (set-difference add children))
		     (insert theory-disp ": substitute " (displayize (car drop) path) " with ")
		     (while (not (null children))
		       (insert (if (or (cdr children) only-one) "" "and ")
			       (displayize (car children) path)
			       " (it already offers e.g. "
			       (highlight (gethash (sxhash (car children)) rep-symbols))
			       (if (cdr children) "), " ").\n"))
		       (setq children (cdr children))))
					; pure drop 
		  (insert theory-disp ": drop " (displayize (car drop) path) " (no symbols used).\n")))
	  
	    (let ((ou optimal-dependencies))
	      (while (and ou (not (member (car drop) 
					  (gethash (sxhash (car ou)) scopes))))
		(setq ou (cdr ou)))
	      (if ou (insert theory-disp ": drop "  (displayize (car drop) path) " (" 
			     (displayize (car (gethash (sxhash (car ou)) scopes)) path)
			    " is dependent on it).\n")
		(insert theory-disp ": drop "  (displayize (car drop) path) " (self-dependency).\n"))))
	  
	  (setq drop (cdr drop)))

      ; process ADDs
	(while add
	(insert theory-disp ": add " (displayize (car add) path))
	(let ((symbol (gethash (sxhash (car add)) rep-symbols)))
	  (if symbol (insert " (it offers e.g. " (highlight symbol) ").\n")
					; forced addition
	    (insert " (lower theory depends on upper by default).\n"))) 
					; NOTE: add split-theory ?
	(setq add (cdr add))))
      scb))))

(defun sanity-check-finalize (buffer)
"Takes a sanity-check buffer and activates the mode, makes the buffer read-only etc."
  (set-buffer buffer)
  (insert "Press C-h m for more information about this buffer.")
  (resize-temp-buffer-window)
  (setq buffer-read-only t)
  (goto-char (point-min))
  (command-mode))

(defun nomdoc-sanity-check-theory ()
 "Determines whether the current theory (the one in which 
the point is located) has optimal dependencies and if 
not suggests changes through `command-mode'."
 (interactive)
 (if (null (current-theory-begin))
     (error "Not in theory"))
 (nomdoc-update-registry)
 (setq buffer (sanity-check-fill t))
 (sanity-check-finalize buffer)
)

(defun nomdoc-sanity-check-buffer ()
 "Executes `nomdoc-sanity-check-theory' on each theory in the current buffer."
 (interactive)
 (save-excursion
   (goto-char (point-min))
   (re-search-forward (regexp-theory-tag) nil t)
   (nomdoc-update-registry)
   (let ((buff (sanity-check-fill t)))
     (while (re-search-forward (regexp-theory-tag) nil t)
       (setq buff (sanity-check-fill)))
     (sanity-check-finalize buff)
     (set-buffer buff)
     (command-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; COMMAND-MODE routines ;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar command-mode-map 
  (let ((s (make-keymap)))
   (define-key s "x" 'sanity-check-execute)
   (define-key s "i" 'sanity-check-ignore)
   (define-key s "a" 'sanity-check-execute-all)
   (define-key s "q" 'sanity-check-quit)
   s))

(defun add-imports (th which)
  "Remove the imports element from TH whose attribute 'from' is WHICH."
  (save-match-data
    (with-current-buffer current-doc-buffer
      (goto-char (point-min))
      (re-search-forward (regexp-theory-tag th) nil t)
					; car because it's propertized
      (search-forward "<" nil t)
      (backward-char 1)
      (insert "<imports xml:id=\"" 
	      th
	      "-imports-" 
	      (progn
		(string-match "[^#]*#\\(.*\\)" which)
		(match-string 1 which))
	      "\" from=\""
	      which 
	      "\"/>\n")
      (indent-according-to-mode))))

(defun delete-imports (th which)
  "Add the imports element to TH whose attribute 'from' is WHICH."
  (save-match-data
    (with-current-buffer current-doc-buffer
      (goto-char (point-min))
      (re-search-forward (regexp-theory-tag th) nil t)
      (re-search-forward (regexp-imports-tag which) nil t)
      (delete-region (match-beginning 0) (+ 1 (match-end 0)))
)))

(defun sanity-check-execute (&optional no-beep)
 (interactive)
 (save-excursion
   (beginning-of-line)
   (if (looking-at "[^#]*#\\([^:]*\\): [ads]")
       (let ((theory-name (match-string-no-properties 1)))
	 (goto-char (- (match-end 0) 1))
	 (cond
	  ((looking-at "add \\([^ ]*\\) ")
	   (add-imports theory-name (match-string-no-properties 1)))
	  ((looking-at "drop \\([^ ]*\\) ")
	   (delete-imports theory-name (match-string-no-properties 1)))
	  ((looking-at "substitute \\([^ ]*\\) with \\([^ ]*\\) ")
	   (delete-imports theory-name (match-string-no-properties 1))
	   (add-imports theory-name (match-string-no-properties 2))))
	 (sanity-check-ignore))
     (unless no-beep (beep)))))

(defun sanity-check-ignore ()
 (interactive)
 (save-excursion
   (beginning-of-line)
   ; if the line is not the last one, delete it
   (if (= (line-number) (line-number (point-max))) (beep)
     (let ((temp (point)))
       
       (search-forward "\n")
       (setq buffer-read-only nil)
       (delete-region temp (point))
       (if (= (count-lines (point-min) (point-max)) 1) (sanity-check-quit)
	 (setq buffer-read-only t))))))

(defun sanity-check-execute-all ()
 (interactive)
 (save-excursion
   (goto-char (point-min))
   (let ((res (sanity-check-execute t))
	 (ok t))
     (while (and (get-buffer "*Sanity Check*") ok)
       (if (null res) (setq ok (= 0 (forward-line))))
       (setq res (sanity-check-execute t))))))

(defun sanity-check-quit ()
 (interactive)
 (kill-buffer nil)
 (delete-window))

(define-derived-mode command-mode fundamental-mode 
  "nOMDoc commands" 
"This mode lists suggested changes to the lastly accessed 
OMDoc file and enables the user to apply or ignore them.
To execute a suggested change, press `x' on its line.
To ignore a suggested change, press 'i' on its line.
To apply all changes, press 'a' anywhere.
To exit (and ignore all still present suggested changes), press 'q'.")

; HERE WE FINALLY PROVIDE SANITY! :-)
(provide 'nomdoc-sanity)
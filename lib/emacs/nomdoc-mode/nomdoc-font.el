;; $Id: nomdoc-font.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-font.el $

;;; nomdoc-font.el --- Font lock lists for nomdoc-mode.el

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

;;; Commentary:
;;{{{ 

;; Defines the faces (color and type) for various OMDoc tag types,
;; as well as the regular expression patterns defining the regions
;;  in an OMDoc document to fontify in each face.

;;}}}

;;{{{ Code:

(require 'nomdoc-config "nomdoc-config")

;;; {{{ FACES

; Possible colors for use for each OMDoc type.
; (Color set for dark background needs to be set and tested)

(defface nomdoc-structure-face
  '((((class color)(background light))
     (:foreground "dark violet"))
    (((class color)(background dark))
     (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
 :group 'nomdoc
)

(defvar nomdoc-structure-face 'nomdoc-structure-face)

(defface nomdoc-math-face
  '((((class color)(background light))
     (:foreground "lime green"))
    (((class color)(background dark))
     (:foreground "sea green"))
    (t (:inverse-video t)))
 "Face for math elements in OMDoc documents."
 :group 'nomdoc
)

(defvar nomdoc-math-face 'nomdoc-math-face)

(defface nomdoc-theory-face
  '((((class color)(background light))
     (:foreground "goldenrod"))
    (((class color)(background dark))
     (:foreground "green2"))
    (t (:inverse-video t)))
 "Face for theory elements in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-theory-face 'nomdoc-theory-face)

(defface nomdoc-auxiliary-face
  '((((class color)(background light))
     (:foreground "VioletRed1"))
    (((class color)(background dark))
     (:foreground "orchid"))
    (t (:inverse-video t)))
 "Face for auxiliary elements in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-auxiliary-face 'nomdoc-auxiliary-face)

(defface nomdoc-presentation-face
  '((((class color)(background light))
     (:foreground "DodgerBlue1"))
    (((class color)(background dark))
     (:foreground "LightCyan"
      :background "DarkCyan"))
    (t (:inverse-video t)))
 "Face for presentation elements in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-presentation-face 'nomdoc-presentation-face)

(defface nomdoc-om-face
  '((((class color)(background light))
     (:foreground "slate blue"))
    (((class color)(background dark))
     (:foreground "slate blue"))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-om-face 'nomdoc-om-face)

(defface nomdoc-dc-face
  '((((class color)(background light))
     (:foreground "cadet blue"))
    (((class color)(background dark))
     (:foreground "Orange2"))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-dc-face 'nomdoc-dc-face)

(defface nomdoc-cdata-face
  '((((class color)(background light))
     (:foreground "brown"))
    (((class color)(background dark))
     (:foreground "brown"))
    (t (:inverse-video t)))
 "Face for CDATA."
 :group 'nomdoc
)
(defvar nomdoc-cdata-face 'nomdoc-cdata-face)

(defface nomdoc-error-face
  '((((class color)(background light))
     (:foreground "Red"
      :bold t
      ))
    (((class color)(background dark))
     (:foreground "Red"
      :bold t
      ))
    (t (:inverse-video t)))
 "Face for potential or real errors in OMDoc documents."
 :group 'nomdoc
)
(defvar nomdoc-error-face 'nomdoc-error-face)

(defface nomdoc-comment-face
  '( (((class color)(background light))
     (:foreground "black"
      :italic t
      ))
    (t (:inverse-video t)))
 "Face for comments"
 :group 'nomdoc
)
(defvar nomdoc-comment-face 'nomdoc-comment-face)

;;; }}}



;;{{{ patterns for font-lock

(defvar nomdoc-font-lock-keywords
  (list 
   ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
   '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
     0 nomdoc-comment-face t)
   '("<Title[^<>]*>\\(.*\\)</Title>" 1 'bold t)
   (list (concat "</?" (regexp-opt nomdoc-structure-keywords t)
		 "[^>]*>")
	 0 nomdoc-structure-face t)
   (list (concat "</?" (regexp-opt nomdoc-math-keywords t)
		 "[^>]*>") 
	 0 nomdoc-math-face t)
   (list (concat "</?" (regexp-opt nomdoc-theory-keywords t)
		 "[^>]*>")
	 0 nomdoc-theory-face t)
   (list (concat "</?" (regexp-opt nomdoc-auxiliary-keywords t)
		 "[^>]*>")
	 0 nomdoc-auxiliary-face t)
   (list (concat "</?" (regexp-opt nomdoc-om-keywords t)
		 "[^>]*>")
	 0 nomdoc-om-face t)
; note: following changed to include <recurse/>
   (list (concat "</?" (regexp-opt nomdoc-presentation-keywords t)
		 "[^>]*/?>")
	 0 nomdoc-presentation-face t)
   (list (concat "</?" (regexp-opt nomdoc-dc-keywords t)
		 "[^>]**>")
	 0 nomdoc-dc-face t)
   '("</?error[^>]*>" 0 nomdoc-error-face t)
; showing strings as values for attributes within tags
   '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
; give emphasis to ID-s
   '("id[ \t\n]*=[ \t\n]*\\(\"[^\"]+\"\\)" 1 nomdoc-structure-face t)
; OMDoc comments
   '("<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" 0 nomdoc-comment-face t)
; initial Directives, also in comment face
   '("<\\?[^>]*>" 0 nomdoc-comment-face t)
   (list (concat "<!\\[CDATA\\["
		 "[^]]*\\(\\][^]]+\\)*\\]?\\(\\]\\][^>]" 
                 "[^]]*\\(\\][^]]+\\)*\\]?\\)*\\(\\]\\]\\)?"
				; any string not containing ]]> as its substring
		 "\\]\\]>")
	 0 nomdoc-cdata-face t)
   '("\\(.\\|\n\\)*" 0 font-lock-string-face n)
  "Additional expressions to highlight in OMDoc mode."))

;;}}}

(provide 'nomdoc-font)
;;; nomdoc-font.el ends here

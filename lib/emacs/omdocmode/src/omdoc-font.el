;;; omdoc-font.el --- Font lock lists for omdoc-mode.el

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

;; Defines the faces (color and type) for various OMDoc tag types,
;; as well as the regular expression patterns defining the regions
;;  in an OMDoc document to fontify in each face.

;;}}}

;;{{{ Code:

;;{{{ Dependencies

(require 'omdoc-config "omdoc-config")

;;}}}

; Possible colors for use for each OMDoc type.
; (Color set for dark background needs to be set and tested)

(defface omdoc-structure-face
  '((((class color)(background light))
     (:foreground "RoyalBlue"
      :bold t))
    (((class color)(background dark))
     (:foreground "RoyalBlue"
      :bold t))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
)
(defvar omdoc-structure-face 'omdoc-structure-face)

(defface omdoc-cdata-face
  '((((class color)(background light))
     (:foreground "OrangeRed3"
      :background "LightBlue"))
    (((class color)(background dark))
     (:foreground "OrangeRed1"
      :background "DarkBlue"))
    (t (:inverse-video t)))
 "Face for unparsed data in OMDoc documents."
)
(defvar omdoc-cdata-face 'omdoc-cdata-face)

(defface omdoc-math-face
  '((((class color)(background light))
     (:foreground "Turquoise"
      :bold t
      ))
    (((class color)(background dark))
     (:foreground "Turquoise"
      :bold t
      ))
    (t (:inverse-video t)))
 "Face for math elements in OMDoc documents."
)
(defvar omdoc-math-face 'omdoc-math-face)

(defface omdoc-theory-face
  '((((class color)(background light))
     (:foreground "green3"
;      :bold t
      ))
    (((class color)(background dark))
     (:foreground "green2"
;      :bold t
      ))
    (t (:inverse-video t)))
 "Face for theory elements in OMDoc documents."
)
(defvar omdoc-theory-face 'omdoc-theory-face)

(defface omdoc-auxiliary-face
  '((((class color)(background light))
     (:foreground "orchid"
      :bold t
      ))
    (((class color)(background dark))
     (:foreground "orchid"
      :bold t
      ))
    (t (:inverse-video t)))
 "Face for auxiliary elements in OMDoc documents."
)
(defvar omdoc-auxiliary-face 'omdoc-auxiliary-face)

(defface omdoc-presentation-face
  '((((class color)(background light))
     (:foreground "DarkCyan"
      :background "LightCyan"
;      :bold t))
      ))
    (((class color)(background dark))
     (:foreground "LightCyan"
      :background "DarkCyan"
;      :bold t))
      ))
    (t (:inverse-video t)))
 "Face for presentation elements in OMDoc documents."
)
(defvar omdoc-presentation-face 'omdoc-presentation-face)

(defface omdoc-om-face
  '((((class color)(background light))
     (:foreground "Purple"
      :background "LightYellow"
;      :bold t
      ))
    (((class color)(background dark))
     (:foreground "Purple"
      :background "Yellow4"
;      :bold t
      ))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
)
(defvar omdoc-om-face 'omdoc-om-face)

(defface omdoc-dc-face
  '((((class color)(background light))
     (:foreground "Orange3"
      :bold t
      ))
    (((class color)(background dark))
     (:foreground "Orange2"
      :bold t
      ))
    (t (:inverse-video t)))
 "Face for structure elements in OMDoc documents."
)
(defvar omdoc-dc-face 'omdoc-dc-face)

(defface omdoc-error-face
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
)
(defvar omdoc-error-face 'omdoc-error-face)

;;{{{ patterns for font-lock

;; The following are left over from html-helper-mode but not currently used.
(defvar omdoc-bold-face 'bold
  "Face used as bold.  Typically `bold'.")
(defvar omdoc-italic-face 'italic
  "Face used as italic.  Typically `italic'.")
(defvar omdoc-underline-face 'underline
  "Face used as underline.  Typically `underline'.")

(defvar omdoc-font-lock-keywords
  (list 
   ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
   '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
     0 font-lock-comment-face t)
;   '("<metadata>\\(\\(.\\|\n\\)*\\)</metadata>" 1 omdoc-bold-face)
; above line causes stack overflow in regexp matcher!
   '("\\(<[^>]*>\\)" 1 font-lock-type-face t)
   '("<Title[^<>]*>\\(.*\\)</Title>" 1 omdoc-bold-face t)
   (list (concat "</?" omdoc-structure-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>")
	 0 omdoc-structure-face t)
   (list (concat "</?" omdoc-math-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>") 
	 0 omdoc-math-face t)
   (list (concat "</?" omdoc-theory-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>")
	 0 omdoc-theory-face t)
   (list (concat "</?" omdoc-auxiliary-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>")
	 0 omdoc-auxiliary-face t)
   (list (concat "</?" omdoc-om-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>")
	 0 omdoc-om-face t)
; note: following changed to include <recurse/>
   (list (concat "</?" omdoc-presentation-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*/?>")
	 0 omdoc-presentation-face t)
   (list (concat "</?" omdoc-dc-keywords-expr
		 "\\([ \t\n]+[^> \t\n]+\\)*>")
	 0 omdoc-dc-face t)
   '("</?error[^>]*>" 0 omdoc-error-face t)
; showing strings as values for attributes within tags
   '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
; OMDoc comments
   '("<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" 0 font-lock-comment-face t)
; initial Directives, also in comment face
   '("<\\?x[^>]*>" 0 font-lock-comment-face t)
   '("<!D[^>]*>" 0 font-lock-comment-face t)
; special font for CDATA
; for the function omdoc-next-cdata, see the main omdoc-mode.el file.
   '(omdoc-next-cdata 0 omdoc-cdata-face t))
;   '("<!\\[CDATA.*\\]\\]>" 0 omdoc-cdata-face t))
  "Additional expressions to highlight in OMDoc mode.")

;;}}}

(provide 'omdoc-font)
;;; omdoc-font.el ends here

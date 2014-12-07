;;;; xslide-font.el --- Tony's XSL font lock keywords
;; $Id: xslide-font.el 3883 2003-04-11 19:13:01Z kohlhase $

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

;; Font lock mode keywords for use when editing XSL stylesheets

;; Send bugs to xslide-bug@menteith.com


;;;; Variables

;; Define mode-specific faces
(defface xsl-xslt-main-face
  '((((background light))
     (:foreground "SlateBlue4"))
    (((background dark))
     (:foreground "Wheat")))
  "Used for local name portion of XSLT elements and attributes"
  :group 'xsl-faces)
(defvar xsl-xslt-main-face 'xsl-xslt-main-face
  "Used for local name portion of XSLT elements and attributes")

(defface xsl-xslt-alternate-face
  '((((background light))
     (:foreground "SlateBlue2"))
    (((background dark))
     (:foreground "LightGray")))
  "Used for prefix and colon portion of XSLT elements and attributes"
  :group 'xsl-faces)
(defvar xsl-xslt-alternate-face 'xsl-xslt-alternate-face
  "Used for prefix and colon portion of XSLT elements and attributes")

(defface xsl-fo-main-face
  '((((background light))
     (:foreground "darkorchid4"))
    (((background dark))
     (:foreground "PaleGreen")))
  "Used for local name portion of formatting object elements and attributes"
  :group 'xsl-faces)
(defvar xsl-fo-main-face 'xsl-fo-main-face
  "Used for local name portion of formatting object elements and attributes")

(defface xsl-fo-alternate-face
  '((((background light))
     (:foreground "darkorchid3"))
    (((background dark))
     (:foreground "Yellow")))
  "Used for prefix and colon portion of formatting object elements and attributes"
  :group 'xsl-faces)
(defvar xsl-fo-alternate-face 'xsl-fo-alternate-face
  "Used for prefix and colon portion of formatting object elements and attributes")

(defface xsl-other-element-face
  '((((background light))
     (:foreground "Blue"))
    (((background dark))
     (:foreground "Coral")))
  "Used for literal result element tags"
  :group 'xsl-faces)
(defvar xsl-other-element-face 'xsl-other-element-face
  "Used for literal result element tags")

;;;; Constants

(eval-and-compile
  (defvar xsl-font-lock-keywords
    (list
     ;;
     ;; Reserved XML Processing instruction lookalikes
     ;;
     '(
       "\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil)
       (3 font-lock-type-face nil t)
       (5 font-lock-type-face nil t)
       (7 font-lock-type-face nil t)
       (11 font-lock-keyword-face))
     ;;
     ;; Non-reserved XML Processing instruction
     ;; Any XML PI that doesn't start with "<?xml"
     ;;
     '("\\(<\\?\\)\\([^ \t?>]+\\)[ \t]*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face)
       (4 font-lock-keyword-face))
     ;;
     ;; Marked section start
     ;;
     '("\\(<!\\[\\)[^[]*\\(\\[\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;;
     ;; XSL formatting objects
     ;;
     (list
      (concat "\\(</?\\)\\(" xsl-fo-ns-prefix ":\\)\\("
	      (regexp-opt
	       (mapcar 'car xsl-fo-symbol-alist))
	      "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\|$\\)")
      '(1 xsl-fo-main-face)
      '(2 xsl-fo-alternate-face)
      '(3 xsl-fo-main-face))
     (list
      (concat "</?" xsl-fo-ns-prefix ":\\([^/>]\\|/[^>]\\)*\\(/?>\\)")
      '(2 xsl-fo-main-face))
     ;;
     ;; XSL elements
     ;;
     (list
      (concat "\\(</?\\)\\(" xsl-xslt-ns-prefix ":\\)\\("
	      (regexp-opt
	       (mapcar 'car xsl-element-symbol-alist))
	      "\\)\\(\\s-+[^= 	]+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?>\\)")
      '(1 xsl-xslt-main-face)
      '(2 xsl-xslt-alternate-face)
      '(3 xsl-xslt-main-face))
     (list
      (concat "</?" xsl-xslt-ns-prefix ":\\S-+\\(\\s-+[^=> 	]+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?>\\)")
      '(3 xsl-xslt-main-face))
     ;;
     ;; XSL attributes
     ;;
     (let* ((xsl-attributes-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-attributes-alist)
	      t))
	    (xsl-attributes-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-attributes-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-attributes-alist-regexp
	 "[ \t]*=[ \t]*\"\\)"
	 "\\([^\"<]*\\)"
	 "\\(\"\\)")
	(list 1 xsl-xslt-alternate-face)
	(list (+ 2 xsl-attributes-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-attributes-alist-regexp-depth)
	      xsl-xslt-alternate-face)))
     ;; do again with single-quote delimiters
     (let* ((xsl-attributes-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-attributes-alist)
	      t))
	    (xsl-attributes-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-attributes-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-attributes-alist-regexp
	 "[ \t]*=[ \t]*'\\)"
	 "\\([^'<]*\\)"
	 "\\('\\)")
	(list 1 xsl-xslt-alternate-face)
	(list (+ 2 xsl-attributes-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-attributes-alist-regexp-depth)
	      xsl-xslt-alternate-face)))
     ;;
     ;; XSL formatting object properties
     ;;
     (let* ((xsl-fo-attribute-symbol-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-fo-attribute-symbol-alist)
	      t))
	    (xsl-fo-attribute-symbol-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-fo-attribute-symbol-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-fo-attribute-symbol-alist-regexp
	 "[ \t]*=[ \t]*\"\\)"
	 "\\([^\"<]*\\)"
	 "\\(\"\\)")
	(list 1 xsl-fo-alternate-face 'append)
	(list (+ 2 xsl-fo-attribute-symbol-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-fo-attribute-symbol-alist-regexp-depth)
	      xsl-fo-alternate-face)))
     ;; do again with single-quote delimiters
     (let* ((xsl-fo-attribute-symbol-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-fo-attribute-symbol-alist)
	      t))
	    (xsl-fo-attribute-symbol-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-fo-attribute-symbol-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-fo-attribute-symbol-alist-regexp
	 "[ \t]*=[ \t]*'\\)"
	 "\\([^'<]*\\)"
	 "\\('\\)")
	(list 1 xsl-fo-alternate-face 'append)
	(list (+ 2 xsl-fo-attribute-symbol-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-fo-attribute-symbol-alist-regexp-depth)
	      xsl-fo-alternate-face)))
     ;;
     ;; Mark the start and end of literals, but don't do anything to their
     ;; contents
     ;;
     '("\\('\\)[^']*\\('\\)"
       (1 font-lock-string-face)
       (2 font-lock-string-face))
     '("\\(\"\\)[^\"]*\\(\"\\)"
       (1 font-lock-string-face)
       (2 font-lock-string-face))
     ;;
     ;; { } in attribute values
     ;;
;;     '("\\('\\|\"\\)\\([^{\\1]\\|{{\\)*\\({[^\\1}]*}\\)\\([^{\\1]\\|{{\\)*\\(\\1\\)"
     '("'\\([^{'<]\\|{{\\)*\\({[^'}<]*}\\)\\([^{'<]\\|{{\\)*'"
       (2 font-lock-variable-name-face t))
     '("\"\\([^{\"<]\\|{{\\)*\\({[^\"}<]*}\\)\\([^{\"<]\\|{{\\)*\""
       (2 font-lock-variable-name-face t))
     ;;
     ;; Text inside <xsl:text>
     (list
      (concat "<" xsl-xslt-ns-prefix ":text>"
	      "\\([^<]*\\)"
	      "</" xsl-xslt-ns-prefix ":text>")
      '(1 font-lock-string-face append))
     ;;
     ;; "Other" tags
     ;;
     (list
      (concat "\\(</?\\([^xf/\?!]\\|x[^s]\\|xs[^l]\\|xsl[^:]\\|f[^o]\\|fo[^:]\\)\\([^</>]\\|/[^>]\\)*/?>\\)")
      '(1 xsl-other-element-face t))
     ;;
     ;; Content of tags
     ;;
     (list
      (concat ">\\([^<]+\\)<")
      '(1 font-lock-string-face keep))
     ;;
     ;; Entity references
     ;;
     '("\\([%&][^; \t]+;\\)"
       (1 font-lock-reference-face t))
     ;;
     ;; Put comment patterns last so they mask anything
     ;; that might be inside the comment
     ;;
     '("\\(<!--[^-]*\\(-[^-]+\\)*-->\\)"
       (1 font-lock-comment-face t))
     )
    "Additional expressions to highlight in XSL mode."))

;;;; Code:
(defun xsl-font-lock-mark-block-function ()
  "Function to mark the area of text to fontify.

Used with font-lock-fontify-block.  Set font-lock-mark-block-function
to this function for this function to take effect.

This function marks the area beginning five \"<\" before point and five
\">\" at ends of lines after point.  The default without a function like
this is to fontify 16 lines before and after point, but then the region
often starts or ends partway through a comment or declaration, turning
that half white because the keywords didn't match, and it just looks so
ugly."
  (let ((current-point (point)))
    (re-search-forward ">[ \t]*$" (point-max) 'limit 5)
    (set-mark (point))
    (goto-char current-point)
    (re-search-backward "^[ \t]*<" (point-min) 'limit 5)))

(defun xsl-font-lock-region-point-min ()
  "Return the start point of the region to fontify"
  (save-excursion
    (re-search-backward "^[ \t]*<" (point-min) 'limit 5)
    (point)))

(defun xsl-font-lock-region-point-max ()
  "Return the start point of the region to fontify"
  (save-excursion
    (re-search-forward ">[ \t]*$" (point-max) 'limit 5)
    (point)))

(provide 'xslide-font)

;; end of xslide-font.el

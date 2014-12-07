;;;; xslide-abbrev.el --- Abbrev table definitions for xslide
;; $Id: xslide-abbrev.el 3883 2003-04-11 19:13:01Z kohlhase $

;; Copyright (C) 1998, 1999, 2001 Tony Graham

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

;; 

;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' to submit a bug report


;;;; Variables:

(defvar xsl-mode-abbrev-table nil
  "Abbrev table used while in XSL mode.")

;;; define xsl-mode-abbrev-table if not already defined
(if xsl-mode-abbrev-table
    ()
  ;; remember current state of abbrevs-changed so it can be restored after
  ;; defining some abbrevs
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'xsl-mode-abbrev-table ())

    ;; Generate abbrevs for XSL and Formatting Object elements from
    ;; data in xsl-all-elements-alist
    (mapcar (lambda (x)
	      (define-abbrev xsl-mode-abbrev-table
		(nth 3 x) (car x) nil))
	    xsl-all-elements-alist)

    ;; Generate abbrevs for attributes for XSL and Formatting Object
    ;; elements
    (mapcar (lambda (x)
	      (define-abbrev xsl-mode-abbrev-table
		(nth 1 x)
		(concat (car x) "=\"\"")
		'backward-char))
	    (append
	     xsl-attributes-alist
	     xsl-fo-attribute-symbol-alist))

    ;; restore abbrevs-changed
    (setq abbrevs-changed ac)))


(provide 'xslide-abbrev)

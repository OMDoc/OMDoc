;; $Id: nomdoc-mode.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-mode.el $

;;; nomdoc-mode.el --- nOMDoc mode main file

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

(require 'nomdoc-config "nomdoc-config")
(require 'nomdoc-templates "nomdoc-templates")
(require 'nomdoc-font "nomdoc-font")
(require 'nomdoc-util "nomdoc-util")
(require 'nomdoc-mathsearch "nomdoc-mathsearch")
(require 'nomdoc-sanity "nomdoc-sanity")
(require 'nomdoc-lispsyntax "nomdoc-lispsyntax")
(require 'nomdoc-completion "nomdoc-completion")
(require 'tempo)
(require 'easymenu)

;; type based keymaps and element-insertion commands are defined
;; in nomdoc-template.el

;; special mode keys
(mapcar
 (function (lambda (l) (define-key nomdoc-mode-map (car l) (nth 1 l))))
 '(
; tempo commands
   ("\M-\C-f"  tempo-forward-mark)
   ("\M-\C-b"  tempo-backward-mark)
   ("\M-\C-c"  tempo-complete-tag)

; re-fontify if things went wrong
   ("\C-c\C-f" font-lock-fontify-buffer)
; rest
   ("\C-ct" nomdoc-insert-time-and-user-at-point)
   ("\C-cn" nomdoc-insert-new-buffer-strings)

   ("\C-cr" nomdoc-roll-element)
   ("\C-cu" nomdoc-unroll-element)
   ("\C-cs" nomdoc-math-search-string)
   ("\C-cx" nomdoc-math-search-xmlq)

   ("\C-cb" nomdoc-build-registry)
   ("\C-cp" nomdoc-update-registry)
   ("\C-cm" nomdoc-sanity-check-openmath)
   ("\C-ck" nomdoc-sanity-check-theory)
   ("\C-cf" nomdoc-sanity-check-buffer)

   ("\C-cc" nomdoc-om-completion)

   ("\C-cg" nomdoc-generate-om)
   ("\C-co" nomdoc-math-outline)
   ("\C-cw" nomdoc-math-show-all)
  ))

(defun nomdoc-insert-new-buffer-strings ()
  "Insert `nomdoc-new-buffer-strings'."
  (interactive)
  (tempo-template-nomdoc-skeleton))

(defun nomdoc-current-buffer ()
  "Return the string name of file belonging to current buffer."
  (buffer-name (current-buffer)))

(defun nomdoc-current-title ()
  "Return the presumed title (first part of buffer name)."
  (let ((bname (nomdoc-current-buffer)))
    (substring bname 0 (or (string-match "\\." bname)(length bname)))))

;;}}}
;;{{{ Major mode setup for the nOMDoc mode

;; Keymap and menu
(defvar nomdoc-mode-map (make-sparse-keymap)
  "Keymap for nOMDoc")

; The main function: deriving from nxml-mode
(define-derived-mode nomdoc-mode nxml-mode  "nOMDoc"
 "Major mode for editing OMDoc documents. It derives from \\[nxml-mode]
and has a number of its own functions in addition."

  (tempo-use-tag-list 'nomdoc-tempo-tags nomdoc-completion-finder)
  
  (if nomdoc-auto-update-timestamps
      (add-hook 'local-write-file-hooks 'nomdoc-update-timestamp))

  ; update the symbol regstry upon saving
  (add-hook 'local-write-file-hooks 'nomdoc-update-registry)
  ; when a new omdoc file is created, fill it with a default skeleton
  (if (and nomdoc-build-new-buffer (zerop (buffer-size)))
      (nomdoc-insert-new-buffer-strings))

  (easy-menu-add (nomdoc-menu) nomdoc-mode-map)

  ;; font-lock setup
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(nomdoc-font-lock-keywords t t))
  (setq font-lock-maximum-size 2048000)

  (run-hooks 'nomdoc-mode-hook)
)

(provide 'nomdoc-mode)

(run-hooks 'nomdoc-load-hook)

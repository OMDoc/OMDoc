;; $Id: nomdoc-util.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-util.el $

;;; nomdoc-util.el --- Various stuff that doesn't fit elsewhere

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

;;; Code to compress and uncompress the current element in one line

(defun nomdoc-roll-element ()
  "Remove newlines and leading whitespace in the current element."
  (interactive)
  
  (let ((beg (save-excursion (nxml-backward-up-element 1) (point)))
	(end (save-excursion (nxml-backward-up-element -1) (point))))
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (while (re-search-forward "\n[ \t]*" nil t)
		    (replace-match " " nil t)))))

(defun nomdoc-unroll-element ()
  "Split the current element up to one tag per line."
  (interactive)
  
  (let ((beg (save-excursion (nxml-backward-up-element 1) (point)))
	(end (save-excursion (nxml-backward-up-element -1) (point))))
    (goto-char beg)
    (set-mark end)
    (while (re-search-forward ">[ \t]*<" (mark) 1)
      (goto-char (+ (match-beginning 0) 1))
      (insert "\n"))
    (indent-region beg (point) nil)))

;; Code to detect and write timestamps (in the Metadata sections)

;;{{{ Timestamp variables:

(defvar nomdoc-timestamp-start "<dc:date action=\"updated\">"
  "*Start delimiter for timestamps.
Everything between `nomdoc-timestamp-start' and
`nomdoc-timestamp-end' will be deleted and replaced with the output
of the functions `nomdoc-timestamp-hook' if
`nomdoc-auto-update-timestamps' is t")

(defvar nomdoc-timestamp-end "</dc:date>"
  "*End delimiter for timestamps.")

;;}}}
;;{{{ Timestamp functions

(defun nomdoc-update-timestamp ()
  "It finds the timestamp in the buffer by looking for
`nomdoc-timestamp-start', deletes all text up to
`nomdoc-timestamp-end', and runs `nomdoc-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward nomdoc-timestamp-start nil t)
      (let ((ts-start (match-end 0))
	    (ts-end (if (search-forward nomdoc-timestamp-end nil t)
			(match-beginning 0)
		      nil)))
	(if ts-end
	    (progn
	      (delete-region ts-start ts-end)
	      (goto-char ts-start)
	      (nomdoc-default-insert-timestamp)))))))

(defun trail-zero (n)
  (concat (if (< n 10) "0" "") (int-to-string n)))

(defun nomdoc-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (decode-time)))
    (insert 
	    (int-to-string (nth 5 time))
	    "-"
	    (trail-zero (nth 4 time))
	    "-"
	    (trail-zero (nth 3 time))
	    "T"
	    (trail-zero (nth 2 time))
	    ":"
	    (trail-zero (nth 1 time))
	    ":"
    	    (trail-zero (nth 0 time))
)))

(defun nomdoc-insert-time-and-user-at-point ()
  "Creates date and user metadata tags. Inserts current time
   and associates an action with the current user."
  (interactive)
  (setq indent (- (point) (line-beginning-position)))
  (setq role (if (= ?c (read-char "Are you a creator (c) or a contributor (n) of this element?"))
      "creator" "contributor"))
  (setq action (if (= ?c (read-char "Is the element created (c) or updated (u)?"))
      "created" "updated"))
  (insert "<dc:date action=\"" action "\" who=\"" (user-login-name) "\">")
  (nomdoc-default-insert-timestamp)
  (insert nomdoc-timestamp-end "\n")
  (insert-char ?\  indent)
  (insert "<dc:" role
	  " id=\"" (user-login-name) "\">" nomdoc-name-string "</dc:" role ">\n")
  (insert-char ?\  indent)
)

;;}}}

(provide 'nomdoc-util)

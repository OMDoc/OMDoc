;;; omdoc-timestamp.el --- Timestamping code for omdoc-mode.el

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

;; Code to detect and write timestamps (in the Metadata sections)
;; 

;;}}}

;;{{{ Code:

;(defconst omdoc-mode-version "0.8")

;;{{{ Dependencies

(require 'omdoc-config "omdoc-config")

;;}}}

;;{{{ Timestamp variables:

(defvar omdoc-timestamp-start "<Date action=\"updated\"> "
  "*Start delimiter for timestamps.
Everything between `omdoc-timestamp-start' and
`omdoc-timestamp-end' will be deleted and replaced with the output
of the functions `omdoc-timestamp-hook' if
`omdoc-do-write-file-hooks' is t")

(defvar omdoc-timestamp-end " </Date>"
  "*End delimiter for timestamps.
Everything between `omdoc-timestamp-start' and
`omdoc-timestamp-end' will be deleted and replaced with the output
of the function `omdoc-insert-timestamp' if
`omdoc-do-write-file-hooks' is t")

;;}}}
;;{{{ Timestamp functions

(defun omdoc-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`omdoc-timestamp-start', deletes all text up to
`omdoc-timestamp-end', and runs `omdoc-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward omdoc-timestamp-start nil t))
	(message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length omdoc-timestamp-start)))
	    (ts-end (if (search-forward omdoc-timestamp-end nil t)
			(- (point) (length omdoc-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'omdoc-timestamp-hook)))))
  nil)

; until I find out how to do this right
(defvar omdoc-month-alist '(("Jan" . "01")("Feb" . "02")("Mar" . "03")("Apr" . "04")("May" . "05")("Jun" . "06")("Jul" . "07")("Aug" . "08")("Sep" . "09")("Oct" . "10")("Nov" . "11")("Dec" . "12"))
  "Month format conversion list")

(defun omdoc-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert 
	    (substring time -4)
	    "-"
	    (cdr (assoc (substring time 4 7) omdoc-month-alist))
	    "-"
	    (substring time 8 10)
	    "T"
	    (substring time 11 19)
	    "Z"
)))

(defun omdoc-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert omdoc-timestamp-start)
  (insert omdoc-timestamp-end))

;;}}}

(provide 'omdoc-timestamp)
;;; omdoc-timestamp.el ends here

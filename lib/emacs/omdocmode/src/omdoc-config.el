;;; omdoc-config.el --- Configuration variables for omdoc-mode.el.

;; Author: Peter Jansen <pjj@cs.cmu.edu>
;; Maintainer: Peter Jansen <pjj@cs.cmu.edu>
;; Contacts: pjj@cs.cmu.edu, kohlhase+@cs.cmu.edu
;; Keywords: OMDoc major-mode (Open Mathematical Documents).
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
;;  User variables for OMDoc mode. 
;;}}}

;;{{{ Code:

(defconst omdoc-mode-version "0.8")
(defvar omdoc-version "1.2")

;;{{{ CONFIG: user variables

;; Program and params for OMDoc validation

;(defvar omdoc-validate-program "C:/Program Files/SP/bin/nsgmls"
;(defvar omdoc-validate-program "G:/rxp.exe"
(defvar omdoc-validate-program "rxp"
  "*Validation program to be run on the current buffer.")

(defvar omdoc-validate-params "-VNxs"
  "*Parameters for the validation program.")

;; Variables related to the OMDoc specification.

(defvar omdochome "../.."
  "Location of omdoc files and stylesheets.")

(defvar omdoc-dtd-public (concat "-//OMDoc//DTD OMDoc V" omdoc-version "//EN")
  "*Public identifier of the OMDoc DTD*")

(defvar omdoc-dtd-system
  (cond ((equal omdoc-version "1.1") "http://omdoc.org/dtd/omdoc1.1.dtd")
	((equal omdoc-version "1.2") "http://omdoc.org/dtd/omdoc.dtd"))
  "*URL to the OMDoc DTD*")
        
(defvar omdoc-omdocdtd-version 
    (concat "<!DOCTYPE omdoc PUBLIC \"" omdoc-dtd-public "\"\n"
	    "                       \"" omdoc-dtd-system "\" []>\n")
  "*Document type declaration for the OMDoc DTD you're using.")

;; Params to identify the user in metadata components

(defvar omdoc-address-string user-mail-address
  "*The default author e-mail address.")

(defvar omdoc-name-string 
     (cond ((> (length user-full-name) 2) user-full-name)
	   (t user-mail-address))
  "*The default author full name.")

;; Features copied from html-helper mode.

(defvar omdoc-use-expert-menu t
  "*If not nil, then use the full OMDoc menu.")

(defvar omdoc-do-write-file-hooks t
  "*If not nil, then modify `local-write-file-hooks' to do timestamps.")

(defvar omdoc-build-new-buffer t
  "*If not nil, then insert `omdoc-new-buffer-strings' for new buffers.")

(defvar omdoc-user-menu nil
  "*Extra items to put in the OMDoc expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar omdoc-never-indent nil
  "*If not nil, the indentation code for OMDoc is turned off.")

;; hooks (see also tempo.el)

(defvar omdoc-mode-hook nil
  "*Hook run when omdoc-mode is started.")

(defvar omdoc-load-hook nil
  "*Hook run when omdoc-mode is loaded.")

(defvar omdoc-timestamp-hook 'omdoc-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

;; Keymap and menu
(defvar omdoc-mode-map (make-sparse-keymap)
  "Keymap for OMDoc")
(defvar omdoc-mode-menu nil
  "Menu for OMDoc. Clobbered and rebuilt by `omdoc-install-menu'")

;; Template for a new OMdoc file.
;  Users may want to change the dtd and namespace location.

(defvar omdoc-new-buffer-template
  '("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    omdoc-omdocdtd-version
    "\n<omdoc id=\"" p (omdoc-current-buffer) "\"\n"
    "  xmlns=\"http://www.mathweb.org/omdoc\"\n"
    "  xmlns:cc=\"http://creativecommons.org/ns\"\n"
    "  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n" 
;    "  xmlns:om=\"http://www.openmath.org/OpenMath\"\n"
    "  xmlns:m=\"http://www.w3.org/1998/Math/MathML\"\n"
    "  version=\"" omdoc-version "\">\n"
    "<metadata>\n"
    "  <dc:date action=\"created\">" (omdoc-default-insert-timestamp) "</dc:date>\n"
    "  <dc:title xml:lang=\"en\">" p (omdoc-current-title) "</dc:title>\n"
    "  <dc:creator role=\"aut\">" p omdoc-name-string "</dc:creator>\n"
    "  <dc:date action=\"updated\">" (omdoc-default-insert-timestamp) "</dc:date>\n"
    "  <dc:type>" p "Text</dc:type>\n"
    "  <dc:format>" p "application/omdoc+xml</dc:format>\n"
    "  <dc:rights>Copyright (c) " (substring (current-time-string) -4) " " omdoc-name-string "</dc:rights>\n"
    "  <cc:license>\n"
    "    <cc:permissions reproduction=\"permitted\" distribution=\"permitted\" derivative_works=\"permitted\"/>\n"
    "    <cc:prohibitions commercial_use=\"permitted\"/>\n"
    "    <cc:requirements notice=\"required\" copyleft=\"required\" attribution=\"required\"/>\n"
    "  </cc:license>\n"
    "</metadata>\n"
    p
    "\n</omdoc>\n")
  "*Template for new buffers.
Inserted by `omdoc-insert-new-buffer-strings' if
`omdoc-build-new-buffer' is set to t")


(defvar omdoc-structure-keywords-expr
  (regexp-opt '("omdoc" "catalogue" "loc" "omtext" "CMP" "with" "omgroup" "ref") t)
  "regular expression specifying OMDoc structure tags")

(defvar omdoc-math-keywords-expr
  (regexp-opt '("symbol" "type" "FMP" "assumption" "conclusion" "axiom" "definition" "requation" "pattern" "value" "measure" "ordering" "adt" "sortdef" "insort" "constructor" "recognizer" "argument" "selector" "assertion" "alternative" "proof" "proofobject" "derive" "hypothesis" "method" "premise" "example") t)
  "regular expression specifying OMDoc math tags")

(defvar omdoc-theory-keywords-expr
  (regexp-opt '("theory" "imports" "morphism" "inclusion" "theory-inclusion" "decomposition" "axiom-inclusion" "path-just" "obligation") t)
  "regular expression specifying OMDoc theory tags")

(defvar omdoc-auxiliary-keywords-expr
  (regexp-opt '("exercise" "hint" "solution" "mc" "choice" "answer" "omlet" "private" "code" "input" "output" "effect" "data" "ignore") t)
  "regular expression specifying OMDoc auxiliary tags")

(defvar omdoc-presentation-keywords-expr
  (regexp-opt '("presentation" "use" "omstyle" "xslt" "style" "element" "attribute" "value-of" "text" "recurse") t)
  "regular expression specifying OMDoc presentation tags")

(defvar omdoc-om-keywords-expr
  (regexp-opt '("OMS" "OMV" "OMI" "OMB" "OMBVAR" "OMSTR" "OMF" "OMA" "OMBIND" "OMATTR" "OMOBJ" "OME" "OMATP") t)
  "regular expression specifying OMDoc open math tags")

(defvar omdoc-dc-keywords-expr
  (regexp-opt '("metadata" "dc:contributor" "dc:creator" "dc:project" "dc:title" "dc:subject" "dc:description" "dc:publisher" "dc:type" "dc:format" "dc:source" "dc:language" "dc:relation" "dc:rights" "dc:date" "dc:identifier") t)
  "regular expression specifying OMDoc Dublin Core tags")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar omdoc-types-to-install
  '(structure math theory auxiliary presentation om dc)
  "*List of tag types to install when omdoc-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;; indentation

(defvar omdoc-indent-default 2
  "*Default indentation for tags not specified in omdoc-indent-alist.")

(defvar omdoc-indent-alist '(("omdoc" . 0) ("omgroup" . 3))
  "*Indentation associated with specific tags, when different from default.")

(defvar omdoc-print-indent-info t
  "If t, indent will print out information as a message.")

;; debugging

(defvar omdoc-debug nil
  "If not nil, message information and pause.")

(defvar omdoc-debug-pause 1
  "Time to pause at debug message.")


; until I find out how to do this right: conversion from Months to integer
; and vice versa.
(defvar omdoc-month-alist '(("Jan" . "01")("Feb" . "02")("Mar" . "03")("Apr" . "04")("May" . "05")("Jun" . "06")("Jul" . "07")("Aug" . "08")("Sep" . "09")("Oct" . "10")("Nov" . "11")("Dec" . "12"))
  "Month format conversion list")

; the concept of 'top level tag' for the purpose of indentation or completion
(defvar omdoc-main-tags-expr
  (regexp-opt '("omdoc" "omgroup" "catalogue" "omtext" "symbol" "axiom" "definition" "assertion" "alternative" "proofobject" "derive" "hypothesis" "example" "imports" "inclusion" "theory-inclusion" "axiom-inclusion" "exercise" "private" "code" "presentation" "omstyle") t)
  "OMDoc 'top-level' tags for the purpose of indentation etc.")

(defvar omdoc-top-level nil
  "whether default indent/blink uses all tags or just main tags.")

;;}}} end of user variables


;;{{{ completion finder for tempo (see omdoc-templates.el)

;(defvar omdoc-completion-finder
;    "\\(\\(<\\|&\\).*\\)\\="
;  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags OMDoc might complete.

;;}}}

(provide 'omdoc-config)
;;; omdoc-config.el ends here

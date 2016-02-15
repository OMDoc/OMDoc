;; $Id: nomdoc-config.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-config.el $

;;; nomdoc-config.el  --- Customizable variables for nomdoc-mode.el.

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

;;{{{ Code:

(defgroup nomdoc nil
  "nOMDoc mode features."
  :group 'nxml)

(defcustom nomdoc-file-paths nil
 "List of paths where OMDoc files are stored. Used for building a registry
with symbols defined in them."
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-registry-dir "~"
 "Path to directory where registry of theories and 
symbols is saved. It is always saved in ' nomdoc-registry.xml'
(note the space in front). If you change the value of this
variable, you should run M-x nomdoc-build-registry to build a new
registry there."
  :group 'nomdoc
  :type 'string)

(defcustom nomdoc-xsl-dir ""
  "Path to directory where XSL files for CMathML <-> OpenMath conversions reside."
  :group 'nomdoc
  :type 'string)

(defcustom nomdoc-browser "firefox"
  "Command for starting your favorite browser"
  :group 'nomdoc
  :type 'string)

(defcustom nomdoc-results-per-page 5
  "The number of results per each `page`"
  :group 'nomdoc
  :type 'number)

(defcustom nomdoc-auto-update-timestamps t
  "*If not nil, then the main metadata timestamp is automatically updated upon saving a OMDoc document"
  :group 'nomdoc
  :type 'boolean)

(defcustom nomdoc-build-new-buffer t
  "*If not nil, then insert an OMDoc skeleton in newly created buffers."
  :group 'nomdoc
  :type 'boolean)

(defcustom nomdoc-use-expert-menu t
  "*If not nil, then use the full nomdoc menu."
  :group 'nomdoc
  :type 'boolean)

(defcustom nomdoc-user-menu nil
  "*Extra items to put in the OMDoc expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus)."
  :group 'nomdoc
  :type 'symbol)

;; hooks (see also tempo.el)

(defcustom nomdoc-mode-hook nil
  "*Hook run when omdoc-mode is started."
  :group 'nomdoc
  :type 'function)

(defcustom nomdoc-load-hook nil
  "*Hook run when omdoc-mode is loaded."
  :group 'nomdoc
  :type 'function)

;; {{{ KEYWORDS

(defcustom nomdoc-structure-keywords
  '("omdoc" "omtext" "CMP" "with" "omgroup" "ref")
  "OMDoc structure tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-math-keywords
  '("symbol" "type" "FMP" "assumption" "conclusion" "phrase" "term" "axiom" "definition" "requation" "pattern" "value" "measure" "ordering" "adt" "sortdef" "insort" "constructor" "recognizer" "argument" "selector" "assertion" "alternative" "proof" "proofobject" "derive" "hypothesis" "method" "premise" "example" "theory" "imports" "tgroup")
  "OMDoc math tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-theory-keywords
  '("morphism" "inclusion" "theory-inclusion" "decomposition" "axiom-inclusion" "path-just" "obligation")
  "OMDoc theory tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-auxiliary-keywords
  '("exercise" "hint" "solution" "mc" "choice" "answer" "omlet" "private" "code" "input" "output" "effect" "data" "ignore" "param" "legacy")
  "OMDoc auxiliary tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-presentation-keywords
  '("presentation" "use" "omstyle" "xslt" "style" "element" "attribute" "value-of" "text" "recurse" "map" "separator")
  "OMDoc presentation tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-om-keywords
  '("OMS" "OMV" "OMI" "OMB" "OMBVAR" "OMSTR" "OMF" "OMA" "OMBIND" "OMATTR" "OMOBJ" "OME" "OMATP" "OMR" "OMFOREIGN")
  "OMDoc open math tags"
  :group 'nomdoc
  :type '(repeat string))

(defcustom nomdoc-dc-keywords
  '("metadata" "dc:contributor" "dc:creator" "dc:project" "dc:title" "dc:subject" "dc:description" "dc:publisher" "dc:type" "dc:format" "dc:source" "dc:language" "dc:relation" "dc:rights" "dc:date" "dc:identifier" "cc:licence" "cc:permissions" "cc:prohibitions" "cc:requirements")
  "OMDoc Dublin Core and Content Commons tags"
  :group 'nomdoc
  :type '(repeat string))

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defcustom nomdoc-types-to-install
  '(structure om math-ml math-text statements adts proofs complex presentation auxiliary exercises dc cc)
  "*List of tag types to install when omdoc-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order."
  :group 'nomdoc
  :type '(repeat symbol))

;;; }}} KEYWORDS

(provide 'nomdoc-config)

;; $Id: nomdoc-templates.el 8011 2008-09-07 19:43:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-templates.el $

;;; nomdoc-templates.el --- Template and Menu code for nomdoc-mode.el.

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

;; This file contains code to 
;;  -- provide template support (interacting with tempo.el)
;;     to generate templates for constructs specified in the 
;;     OMDoc 1.2 DTD.
;;  -- provide menu support (for the same)
;;  -- automatically install key bindings for the various insertion
;;     functions
;;}}}

;;{{{ Code:

;;{{{ Dependencies

(require 'nomdoc-config "nomdoc-config")
(require 'tempo)
(require 'easymenu)

;;}}}

;;{{{ Template functions

;; nomdoc-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu
;; (i.e. belong to one MODULE in OMDoc specification).
;; Types can be added to the system after omdoc has been loaded,
;; briefly by doing omdoc-add-type-to-alist, then
;; nomdoc-install-type, then omdoc-add-tag (for each tag)
;; then omdoc-rebuild-menu. See the mode documentation for more detail.

(defconst nomdoc-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `nomdoc-add-type-to-alist'.")

;;{{{ accessor functions for nomdoc-type-alist

(defun nomdoc-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type nomdoc-type-alist))))

(defun nomdoc-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type nomdoc-type-alist))))

(defun nomdoc-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type nomdoc-type-alist))))

(defun nomdoc-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type nomdoc-type-alist))))

(defun nomdoc-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (nomdoc-menu-string-for type)
	(eval (nomdoc-menu-for type))))

;;}}}

(defun nomdoc-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq nomdoc-type-alist (cons type nomdoc-type-alist)))

;; Here are the categories for nearly all OMDoc 1.2 modules
(mapcar 'nomdoc-add-type-to-alist
  '((structure    . (nomdoc-structure-map "\C-c\C-s" nomdoc-structure-menu "Insert Document Structure Elements"))
    (dc    . (nomdoc-dc-map "\C-c\C-d" nomdoc-dc-menu "Insert Dublin Core Elements"))
    (cc    . (nomdoc-cc-map "\C-c\C-r" nomdoc-cc-menu "Insert Creative Commons Elements"))
    (om    . (nomdoc-om-map "\C-c\C-o" nomdoc-om-menu "Insert Open Math Elements"))
    (math-ml . (nomdoc-math-ml-map "\C-c\C-l" nomdoc-math-ml-menu "Insert Content MathML Elements"))
    (math-text . (nomdoc-math-text-map "\C-c\C-x" nomdoc-math-text-menu "Insert Math Text Elements"))
    (statements . (nomdoc-statements-map "\C-c\C-t" nomdoc-statements-menu "Insert Math Statement Elements"))
    (adts . (nomdoc-adts-map "\C-c\C-a" nomdoc-adts-menu "Insert Abstract Data Type Elements"))
    (proofs . (nomdoc-proofs-map "\C-c\C-p" nomdoc-proofs-menu "Insert Proof Elements"))
    (complex  . (nomdoc-complex-map "\C-c\C-c" nomdoc-complex-menu "Insert Complex Theory Elements"))
    (presentation    . (nomdoc-presentation-map "\C-c\C-n" nomdoc-presentation-menu
						"Insert Presentation & Notation Elements"))   
    (auxiliary  . (nomdoc-auxiliary-map "\C-c\C-u" nomdoc-auxiliary-menu "Insert Auxiliary Elements"))
    (exercises  . (nomdoc-exercises-map "\C-c\C-e" nomdoc-exercises-menu "Insert Exercise Elements"))))

;; Once nomdoc-mode is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst nomdoc-installed-types nil
  "The types that have been installed (used when building menus).")

;; Keymap and menu
(defvar nomdoc-mode-map (make-sparse-keymap)
  "Keymap for nOMDoc")

(defun nomdoc-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with nomdoc-add-type-to-alist."
  (setq nomdoc-installed-types (cons type nomdoc-installed-types))
  (let ((keymap (nomdoc-keymap-for type))
	(key (nomdoc-key-for type))
	(menu (nomdoc-menu-for type))
	(menu-string (nomdoc-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	   (define-key nomdoc-mode-map key keymap)))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'nomdoc-install-type nomdoc-types-to-install)

;;{{{ nomdoc-add-tag function for building basic tags

(defvar nomdoc-tempo-tags nil
  "List of tags used in completion.")

(defun nomdoc-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "nomdoc-" (downcase s))))

(defun nomdoc-add-tag (l)
  "Add a new tag to nomdoc-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(nomdoc-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (nomdoc-keymap-for type))
	 (menu (nomdoc-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template (nomdoc-string-to-symbol name)
					 tag completer doc
					 'nomdoc-tempo-tags)))

    (if (null (memq type nomdoc-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key nomdoc-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  ; good, add it at the back [keep the order!]
	  (set menu (append (eval menu) (list (vector name command t)))))
      )))

;;}}}

;;{{{ The nOMDoc tags

;; Based on OMDoc 1.2 modules.

(mapcar
 'nomdoc-add-tag
 '(
					; module DOC: document infrastructure elements
   (structure	"o"	"<omdoc"	"omdoc"	("<omdoc xml:id=\"" p "\">" n r> n "</omdoc>"))
   (structure	"g"	"<omgroup>"	"omgroup"	("<omgroup>" n> r n "</omgroup>" >))
   (structure	"m"	"<metadata>"	"metadata"	("<metadata>" n> r n "</metadata>" >))
   (structure	"r"	"<ref"	"ref"	("<ref xref=\"" r "\"/>"))
   (structure	"i"	"<ignore>"	"ignore"	("<ignore>" n> r n "</ignore>" >))

					; Dublin Core & Creative Commons
   (dc	"c"	"<dc:creator>"	"creator"	(& "<dc:creator role=\"aut" p "\"> " nomdoc-name-string p " </dc:creator>" > %))
   (dc	"3"	"<dc:contributor>"	"contributor"	(& "<dc:contributor> " nomdoc-address-string p " </dc:contributor>" > %))
   (dc	"t"	"<dc:title>"	"title"	("<dc:title> " r " </dc:title>" >))
   (dc	"s"	"<dc:subject>"	"subject"	("<dc:subject> " r " </dc:subject>" >))
   (dc	"x"	"<dc:description>"	"description"	("<dc:description>" n> r n "</dc:description>" >))
   (dc	"p"	"<dc:publisher>"	"publisher"	("<dc:publisher> " r " </dc:publisher>" >))
   (dc	"d"	"<dc:date>"	"date"	(& "<dc:date action=\"created" p "\"> " (nomdoc-default-insert-timestamp) p " </dc:date>" > %))
   (dc	"y"	"<dc:type>"	"dc:type"	("<dc:type>" n> r n "</dc:type>" >))
   (dc	"f"	"<dc:format>"	"format"	("<dc:format> " r " </dc:format>" >))
   (dc	"1"	"<dc:identifier>"	"identifier"	("<dc:identifier> " r " </dc:identifier>" >))
   (dc	"b"	"<dc:source>"	"source"	("<dc:source> " r " </dc:source>" >))
   (dc	"l"	"<dc:language>"	"language"	("<dc:language> " r " </dc:language>" >))
   (dc	"r"	"<dc:relation>"	"relation"	("<dc:relation> " r " </dc:relation>" >))
   (dc	"i"	"<dc:rights>"	"rights"	("<dc:rights> " r " </dc:rights>" >))

					; Creative Commons
   (cc	"l"	"<cc:licence>"	        "licence"	("<cc:licence> " r " </cc:licence>" >))
   (cc	"p"	"<cc:permissions>"	"permissions"	("<cc:permissions " r ">"))
   (cc	"h"	"<cc:prohibitions>"	"prohibitions"	("<cc:prohibitions " r ">"))
   (cc	"r"	"<cc:requirements>"	"requirements"	("<cc:requirements " r ">"))

					; Open Math
   (om	"o"	"<OMOBJ>"	"OMOBJ"	("<OMOBJ>" n> r n "</OMOBJ>" >))
   (om	"s"	"<OMS"	"OMS"	("<OMS name=\"" p "\" cd=\"" p "\"/>"))
   (om	"v"	"<OMV"	"OMV"	("<OMV name=\"" p "\"/>"))
   (om	"a"	"<OMA>"	"OMA"	("<OMA>" n> r n "</OMA>" >))
   (om	"d"	"<OMBIND>"	"OMBIND"	("<OMBIND>" n> r n "</OMBIND>" >))
   (om	"l"	"<OMBVAR>"	"OMBVAR"	("<OMBVAR>" p "</OMBVAR>" >))
   (om	"n"	"<OMFOREIGN>"	"OMFOREIGN"	("<OMFOREIGN>" n> r n "</OMFOREIGN>" >))
   (om	"t"	"<OMATTR>"	"OMATTR"	("<OMATTR>" n> r n "</OMATTR>" >))
   (om	"1"	"<OMATP>"	"OMATP"	("<OMATP>" n> r n "</OMATP>" >))
   (om	"i"	"<OMI>"	"OMI"	("<OMI>" n> r n "</OMI>" >))
   (om	"b"	"<OMB>"	"OMB"	("<OMB>" n> r n "</OMB>" >))
   (om	"f"	"<OMF"	"OMF"	("<OMF>" n> r n "</OMF>" >))
   (om	"e"	"<OME>"	"OME"	("<OME>" n> r n "</OME>" >))
   (om	"r"	"<OMR"	"OMR"	("<OMR href=\"" p "\"/>"))
   (om	"g"	"<OMSTR>"	"OMSTR"	("<OMSTR>" n> r n "</OMSTR>" >))

					; Content MathML
   (math-ml     "m"	"<m:math>"	"math"	("<m:math>" n> r n "</m:math>" >))
   (math-ml     "p"	"<m:apply>"	"apply"	("<m:apply>" n> r n "</m:apply>" >))
   (math-ml     "y"	"<m:csymbol"	"csymbol"	("<m:csymbol definitionURL=\"" p "\"/>"))
   (math-ml     "i"	"<m:ci>"	"ci"	("<m:ci>" r "</m:ci>" >))
   (math-ml     "n"	"<m:cn>"	"cn"	("<m:cn>" r "</m:cn>" >))
   (math-ml     "b"	"<m:bvar>"	"bvar"	("<m:bvar>" r "</m:bvar>" >))
   (math-ml     "s"	"<m:semantics>"	"semantics"	("<m:semantics>" n> r n "</m:semantics>" >))
   (math-ml     "a"	"<m:annotation>"	"annotation"	("<m:annotation>" n> r n "</m:annotation>" >))
   (math-ml     "x"	"<m:annotation-xml>"	"annotation-xml"	("<m:annotation-xml>" n> r n "</m:annotation-xml>" >))
					; module MTXT: mathematical text
   (math-text	"c"	"<CMP>"	"CMP"	("<CMP>" n> r n "</CMP>" >))
   (math-text	"f"	"<FMP>"	"FMP"	("<FMP>" n> r n "</FMP>" >))
   (math-text	"a"	"<assumption"	"assumption"	("<assumption xml:id=\"" p "\">" n> r n "</assumption>" >))
   (math-text	"n"	"<conclusion"	"conclusion"	("<conclusion xml:id=\"" p "\">" n> r n "</conclusion>" >))
   (math-text	"p"	"<phrase"	"phrase"	("<phrase xml:id=\"" p "\">" n> r n "</phrase>" >))
   (math-text	"t"	"<term" 	"term"	        ("<term name=\"" p "\" cd=\"" p "\"" n> r n "</term>" >))
   (math-text	"o"	"<omtext"	"omtext"	("<omtext xml:id=\"" p "\">" n> r n "</omtext>" >))

					; module ST: mathematical statements
   (statements	"s"	"<symbol>"	"symbol"	("<symbol>" n> r n "</symbol>" >))
   (statements	"y"	"<type"	"type"	("<type system=\"" p "\">" n> r n "</type>" >))
   (statements	"x"	"<axiom"	"axiom"	("<axiom xml:id=\"" p "\">" n> r n "</axiom>" >))
   (statements	"d"	"<definition"	"definition"	("<definition xml:id=\"" p "\" for=\"" p "\">" n> r n "</definition>" >))
   (statements	"r"	"<requation>"	"requation"	("<requation>" n> r n "</requation>" >))
   (statements	"m"	"<measure>"	"measure"	("<measure>" n> r n "</measure>" >))
   (statements	"o"	"<ordering>"	"ordering"	("<ordering>" n> r n "</ordering>" >))
   (statements	"a"	"<assertion"	"assertion"	("<assertion xml:id=\"" p "\">" n> r n "</assertion>" >))
   (statements	"e"	"<example"	"example"	("<example xml:id=\"" p "\" for=\"" p "\">" n> r n "</example>" >))
   (statements	"l"	"<alternative"	"alternative"	("<alternative theory=\"" p "\" entailed-by=\"" p "\" entails=\"" p "\" entailed-by-thm=\"" p "\" entails-thm=\"" p "\" xml:id=\"" p "\" for=\"" p "\">" n> r n "</alternative>" >))
   (statements	"t"	"<theory"	"theory"	("<theory xml:id=\"" p "\">" n> r n "</theory>" >))
   (statements	"i"	"<imports"	"imports"	("<imports xml:id=\"" p "\" from=\"" p "\">" n> r n "</imports>" >))
   (statements  "g"     "<tgroup>"      "tgroup"        ("<tgroup>" n> r n "</tgroup>" > ))

					; module ADT: Abstract Data Types
   (adts	"a"	"<adt"	"adt"	("<adt xml:id=\"" p "\">" n> r n "</adt>" >))
   (adts	"s"	"<sortdef"	"sortdef"	("<sortdef name=\"" p "\">" n> r n "</sortdef>" >))
   (adts	"c"	"<constructor"	"constructor"	("<constructor name=\"" p "\">" n> r n "</constructor>" >))
   (adts	"r"	"<argument"	"argument"	("<argument>" n> r n "</argument>" >))
   (adts	"i"	"<insort"	"insort"	("<insort for=\"" p "\"/>"))
   (adts	"l"	"<selector"	"selector"	("<selector name=\"" p "\">" n> r n "</selector>" >))
   (adts	"z"	"<recognizer"	"recognizer"	("<recognizer name=\"" p "\">" n> r n "</recognizer>" >))

					; module PF: Proofs
   (proofs	"p"	"<proof"	"proof"	("<proof xml:id=\"" p "\" for=\"" p "\">" n> r n "</proof>" >))
   (proofs	"o"	"<proofobject"	"proofobject"	("<proofobject xml:id=\"" p "\" for=\"" p "\">" n> r n "</proofobject>" >))
   (proofs	"h"	"<hypothesis"	"hypothesis"	("<hypothesis xml:id=\"" p "\">" n> r n "</hypothesis>" >))
   (proofs	"d"	"<derive"	"derive"	("<derive xml:id=\"" p "\">" n> r n "</derive>" >))
   (proofs	"m"	"<method"	"method"	("<method xref=\"" p "\">" n> r n "</method>" >))
   (proofs	"r"	"<premise"	"premise"	("<premise xref=\"" p "\"/>"))

					; module CTH and DG: Complex Theories
   (complex	"m"	"<morphism>"	"morphism"	("<morphism>" n> r n "</morphism>" >))
   (complex	"i"	"<inclusion"	"inclusion"	("<inclusion via=\"" p "\"/>"))
   (complex	"t"	"<theory-inclusion"	"theory-inclusion"	("<theory-inclusion xml:id=\"" p "\" from=\"" p "\" to=\"" p "\">" n> r n "</theory-inclusion>" >))
   (complex	"a"	"<axiom-inclusion"	"axiom-inclusion"	("<axiom-inclusion xml:id=\"" p "\" from=\"" p "\" to=\"" p "\">" n> r n "</axiom-inclusion>" >))
   (complex	"d"	"<decomposition"	"decomposition"	("<decomposition links=\"" p "\" xml:id=\"" p "\" for=\"" p "\"/>"))
   (complex	"p"	"<path-just"	"path-just"	("<path-just local=\"" p "\" globals=\"" p "\"/>"))
   (complex	"o"	"<obligation"	"obligation"	("<obligation induced-by=\"" p "\" assertion=\"" p "\"/>"))

					; module PRES: Presentation and notation elements
   (presentation	"o"	"<omstyle"	"omstyle"	("<omstyle element=\"" p "\">" n> r n "</omstyle>" >))
   (presentation	"p"	"<presentation"	"presentation"	("<presentation for=\"" p "\">" n> r n "</presentation>" >))
   (presentation	"x"	"<xslt"	"xslt"	("<xslt format=\"" p "\">" n> r n "</xslt>" >))
   (presentation	"u"	"<use"	"use"	("<use format=\"" p "\">" n> r n "</use>" >))
   (presentation	"s"	"<style"	"style"	("<style format=\"" p "\">" n> r n "</style>" >))
   (presentation	"e"	"<element"	"element"	("<element name=\"" p "\">" n> r n "</element>" >))
   (presentation	"a"	"<attribute"	"attribute"	("<attribute name=\"" p "\">" n> r n "</attribute>" >))
   (presentation	"t"	"<text>"	"text"	("<text>" n> r n "</text>" >))
   (presentation	"v"	"<value-of"	"value-of"	("<value-of select=\"" p "\"/>"))
   (presentation	"r"	"<recurse"	"recurse"	("<recurse/>"))
   (presentation	"m"	"<map>"	         "map"	        ("<map>" p "</map>"))
   (presentation	"-"	"<separator>"	"separator"	("<separator>" p "</separator>"))

					; module EXT: Auxiliary elements
   (auxiliary	"p"	"<private"	"private"	("<private xml:id=\"" p "\">" n> r n "</private>" >))
   (auxiliary	"c"	"<code"	"code"	("<code xml:id=\"" p "\">" n> r n "</code>" >))
   (auxiliary	"i"	"<input>"	"input"	("<input>" n> r n "</input>" >))
   (auxiliary	"o"	"<output>"	"output"	("<output>" n> r n "</output>" >))
   (auxiliary	"e"	"<effect>"	"effect"	("<effect>" n> r n "</effect>" >))
   (auxiliary	"d"	"<data>"	"data"	("<data>" n> r n "</data>" >))
   (auxiliary	"l"	"<omlet"	"omlet"	("<omlet data=\"" p "\">" n> r n "</omlet>" >))
   (auxiliary	"m"	"<param"	"param"	("<param name=\"" p "\">" n> r n "</param>" >))
   ; here we insert legacy, because elements from Math-Objects module have been put into OM and MathML
   ; groups respectively, and legacy is left alone so we must deal with it [don't want group with only one member]
   (auxiliary   "g"     "<legacy"       "legacy" ("<legacy format=\"" p "\">" n> r n "</legacy>" >))

					; module QUIZ: Exercises
   (exercises	"e"	"<exercise"	"exercise"	("<exercise xml:id=\"" p "\" for=\"" p "\">" n> r n "</exercise>" >))
   (exercises	"h"	"<hint>"	"hint"	("<hint>" n> r n "</hint>" >))
   (exercises	"s"	"<solution>"	"solution"	("<solution>" n> r n "</solution>" >))
   (exercises	"m"	"<mc>"	"mc"	("<mc>" n> r n "</mc>" >))
   (exercises	"x"	"<choice>"	"choice"	("<choice>" n> r n "</choice>" >))
   (exercises	"a"	"<answer"	"answer"	("<answer verdict=\"" p "\">" n> r n "</answer>" >))

  ))

(defvar nomdoc-omdocdtd-version 
    (concat "<!DOCTYPE omdoc PUBLIC \"-//OMDoc//DTD OMDoc V1.2//EN\"\n"
	    "                       \"http://www.mathweb.org/src/mathweb/omdoc/dtd/omdoc.dtd\" []>\n")
  "*Document type declaration for the OMDoc DTD you're using.")

;; Params to identify the user in metadata components

(defvar nomdoc-address-string user-mail-address
  "*The default author e-mail address.")

(defvar nomdoc-name-string 
     (cond ((> (length user-full-name) 2) user-full-name)
	   (t user-mail-address))
  "*The default author full name.")

;; Template for a new OMdoc file.
;  Users may want to change the dtd and namespace location.

(defvar nomdoc-new-buffer-template
  '("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    nomdoc-omdocdtd-version
    "\n<omdoc id=\"" p (nomdoc-current-buffer) "\"\n"
    "  xmlns=\"http://www.mathweb.org/omdoc\"\n"
    "  xmlns:cc=\"http://creativecommons.org/ns\"\n"
    "  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n" 
    "  xmlns:om=\"http://www.openmath.org/OpenMath\"\n"
    "  xmlns:m=\"http://www.w3.org/1998/Math/MathML\"\n"
    "  version=\"1.2\">\n"
    "<metadata>\n"
    "  <dc:date action=\"created\">" (nomdoc-default-insert-timestamp) "</dc:date>\n"
    "  <dc:title xml:lang=\"en\">" p (nomdoc-current-title) "</dc:title>\n"
    "  <dc:creator role=\"aut\">" p nomdoc-name-string "</dc:creator>\n"
    "  <dc:date action=\"updated\">" (nomdoc-default-insert-timestamp) "</dc:date>\n"
    "  <dc:type>" p "Text</dc:type>\n"
    "  <dc:format>" p "application/omdoc+xml</dc:format>\n"
    "  <dc:rights>Copyright (c) " (substring (current-time-string) -4) " " nomdoc-name-string "</dc:rights>\n"
    "  <cc:license>\n"
    "    <cc:permissions reproduction=\"permitted\" distribution=\"permitted\" derivative_works=\"permitted\"/>\n"
    "    <cc:prohibitions commercial_use=\"permitted\"/>\n"
    "    <cc:requirements notice=\"required\" copyleft=\"required\" attribution=\"required\"/>\n"
    "  </cc:license>\n"
    "</metadata>\n"
    p
    "\n</omdoc>\n")
  "*Template for new buffers.
Inserted by `nomdoc-insert-new-buffer-strings' if
`nomdoc-build-new-buffer' is set to t")


(tempo-define-template "nomdoc-skeleton" nomdoc-new-buffer-template
		       nil
		       "Insert a skeleton for an OMDoc document")


;;}}}
;;{{{ Menu support

;; menus are built for easymenu. nomdoc-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar nomdoc-novice-menu
  '("OMDoc"
    ["Insert omgroup" tempo-template-nomdoc-omgroup t]
    ["Insert CMP" tempo-template-nomdoc-cmp t]
    ["Insert omtext" tempo-template-nomdoc-omtext t]
    ["Insert FMP" tempo-template-nomdoc-fmp t]
    ["Insert theory" tempo-template-nomdoc-theory t]
    ["Insert example" tempo-template-nomdoc-example t]
    ["Insert omlet" tempo-template-nomdoc-omlet t]
    ["Insert presentation" tempo-template-nomdoc-presentation t]
    ["Insert OMOBJ" tempo-template-nomdoc-omobj t]
    ["Insert OMS" tempo-template-nomdoc-oms t]
    ["-" nil t]
    ["Turn on Expert Menu" nomdoc-toggle-expert-menu t]
)
  "Menu for novices, only installed if `nomdoc-use-expert-menu is nil'")

(defun nomdoc-menu nil
  "Return the proper menu. Looks at `nomdoc-use-expert-menu'"
  (if nomdoc-use-expert-menu
      (nomdoc-expert-menu)
    nomdoc-novice-menu))

(defun nomdoc-rebuild-menu nil
  "Rebuild and install the nOMDoc menu (using `easy-menu-define').
If `nomdoc-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (nomdoc-menu)))
    (easy-menu-remove menu)
    (easy-menu-define nomdoc-mode-menu-symbol
		      nomdoc-mode-map "nOMDoc menus" menu)
    (easy-menu-add menu nomdoc-mode-map)))

(defun nomdoc-toggle-expert-menu (&optional arg)
  "Toggle full nOMDoc menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq nomdoc-use-expert-menu
	(if (null arg) (not nomdoc-use-expert-menu)
	  (> (prefix-numeric-value arg) 0)))
  (nomdoc-rebuild-menu))

;; Expert menus: consed up out of nomdoc-installed-types
(defun nomdoc-expert-menu ()
  "This menu is based on the current value of `nomdoc-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq nomdoc-mode-menu nil)
  
  ;; Cons in the toggle of the menu
  (setq nomdoc-mode-menu
	(cons '["Turn on Novice Menu" nomdoc-toggle-expert-menu t]
	      nomdoc-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq nomdoc-mode-menu
	(append nomdoc-user-menu nomdoc-mode-menu))

  (defun nomdoc-customize ()
    (interactive)
    (customize-group "nomdoc"))

  ;; cons in the timestamp delimiters
  (setq nomdoc-mode-menu
	(append
	 (list
	  '["-" nil t]
	  '["OpenMath Completion" nomdoc-om-completion t]
	  '["-" nil t]
	  '["Sanity Check Current Theory" nomdoc-sanity-check-theory t]
	  '["Sanity Check OpenMath in Buffer" nomdoc-sanity-check-openmath t]
	  '["Sanity Check All Theories in Buffer" nomdoc-sanity-check-buffer t]
	  '["Build Registry from Scratch" nomdoc-build-registry t]
	  '["Update Registry from Buffer" nomdoc-update-registry t]
	  '["-" nil t]
	  '["Convert S-Expression into OpenMath" nomdoc-generate-om t]
	  '["-" nil t]
	  '["Roll Element in One Line" nomdoc-roll-element t]
	  '["Unroll Current Element" nomdoc-unroll-element t]
	  '["Insert Time and User Tags" nomdoc-insert-time-and-user-at-point t]
	  '["Insert OMDoc Default Skeleton" nomdoc-insert-new-buffer-strings t]
	  '["-" nil t]
	  '["Math Search by String" nomdoc-math-search-string t]
	  '["Math Search by XMLQ" nomdoc-math-search-xmlq t]
	  '["-" nil t]
	  '["Customize nOMDoc" nomdoc-customize t]
)
	      nomdoc-mode-menu))
  
  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
	       (setq nomdoc-mode-menu
		     (cons (nomdoc-normalized-menu-for type)
			   nomdoc-mode-menu))))
	  nomdoc-installed-types)

  (setq nomdoc-mode-menu
	(append
	 (list
	  '["Show Math Outline Only" nomdoc-math-outline t]
	  '["Show All Math" nomdoc-math-show-all t]
	  '["-" nil t]
)
	      nomdoc-mode-menu))
  

  ;; now tack on our name
  (setq nomdoc-mode-menu (cons "OMDoc" nomdoc-mode-menu))
  nomdoc-mode-menu
)

(nomdoc-rebuild-menu)

;;}}}

;;{{{ completion finder for tempo

(defvar nomdoc-completion-finder
    "\\(\\(<\\|&\\).*\\)\\="
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;;}}}

(provide 'nomdoc-templates)
;;; nomdoc-templates.el ends here

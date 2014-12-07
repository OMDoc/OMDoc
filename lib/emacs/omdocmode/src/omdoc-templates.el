;;; omdoc-templates.el --- Template and Menu code for omdoc-mode.el.

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

;; This file contains code to 
;;  -- provide template support (interacting with tempo.el)
;;     to generate templates for constructs specified in the 
;;     OMDoc 1.1 DTD.
;;  -- provide menu support (for the same)
;;  -- automatically install key bindings for the various insertion
;;     functions
;;}}}

;;{{{ Code:

;;{{{ Dependencies

(require 'omdoc-config "omdoc-config")
(require 'tempo)
(require 'easymenu)

;;}}}

;;{{{ Template functions

;; omdoc-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after omdoc has been loaded,
;; briefly by doing omdoc-add-type-to-alist, then
;; omdoc-install-type, then omdoc-add-tag (for each tag)
;; then omdoc-rebuild-menu. See the mode documentation for more detail.

(defconst omdoc-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `omdoc-add-type-to-alist'.")

;;{{{ accessor functions for omdoc-type-alist

(defun omdoc-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type omdoc-type-alist))))

(defun omdoc-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type omdoc-type-alist))))

(defun omdoc-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type omdoc-type-alist))))

(defun omdoc-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type omdoc-type-alist))))

(defun omdoc-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (omdoc-menu-string-for type)
	(eval (omdoc-menu-for type))))

;;}}}

(defun omdoc-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq omdoc-type-alist (cons type omdoc-type-alist)))

;; Here are the types provided by omdoc-mode.
;;  '(structure math theory auxiliary presentation OM DC)
(mapcar 'omdoc-add-type-to-alist
  '((structure    . (omdoc-structure-map "\C-c\C-s" omdoc-structure-menu "Insert Document Structure Elements"))
    (math    . (omdoc-math-map "\C-c\C-m" omdoc-math-menu "Insert Math Elements"))
    (theory  . (omdoc-theory-map "\C-c\C-t" omdoc-theory-menu "Insert Theory Element"))
    (auxiliary  . (omdoc-auxiliary-map "\C-c\C-a" omdoc-auxiliary-menu "Insert Auxiliary Elements"))
    (presentation    . (omdoc-presentation-map "\C-c\C-p" omdoc-presentation-menu "Insert Presentation Elements"))
    (om    . (omdoc-om-map "\C-c\C-o" omdoc-om-menu "Insert OM Elements"))
    (dc    . (omdoc-dc-map "\C-c\C-d" omdoc-dc-menu "Insert Dublin Core Elements"))))

;; Once omdoc-mode is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst omdoc-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun omdoc-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with omdoc-add-type-to-alist."
  (setq omdoc-installed-types (cons type omdoc-installed-types))
  (let ((keymap (omdoc-keymap-for type))
	(key (omdoc-key-for type))
	(menu (omdoc-menu-for type))
	(menu-string (omdoc-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	   (define-key omdoc-mode-map key keymap)))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'omdoc-install-type omdoc-types-to-install)

;;{{{ omdoc-add-tag function for building basic tags

(defvar omdoc-tempo-tags nil
  "List of tags used in completion.")

(defun omdoc-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "omdoc-" (downcase s))))

(defun omdoc-add-tag (l)
  "Add a new tag to omdoc-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(omdoc-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (omdoc-keymap-for type))
	 (menu (omdoc-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template (omdoc-string-to-symbol name)
					 tag completer doc
					 'omdoc-tempo-tags)))

    (if (null (memq type omdoc-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key omdoc-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;;}}}

;;{{{ The OMDoc tags

;; Based on the 1.1 OMdoc DTD.
;;  Generated semi-automatically

(mapcar
 'omdoc-add-tag
;;  '(structure math theory auxiliary presentation OM DC)
 '(
					; structure elements
   (structure	"o"	"<omdoc"	"omdoc"	("<omdoc xml:id=\"" p "\">" n r> n "</omdoc>"))
   (structure	"u"	"<catalogue>"	"catalogue"	("<catalogue>" n> r n "</catalogue>" >))
   (structure	"l"	"<loc"	"loc"	("<loc theory=\"" p "\"/>"))
   (structure	"t"	"<omtext"	"omtext"	("<omtext xml:id=\"" p "\">" n> r n "</omtext>" >))
   (structure	"c"	"<CMP>"	"CMP"	("<CMP>" n> r n "</CMP>" >))
   (structure	"w"	"<with>"	"with"	("<with>" n> r n "</with>" >))
   (structure	"g"	"<omgroup>"	"omgroup"	("<omgroup>" n> r n "</omgroup>" >))
   (structure	"r"	"<ref"	"ref"	("<ref xref=\"" r "\"/>"))

					; math elements -- too many of those for comfortable key assignments, but...
   (math	"y"	"<symbol>"	"symbol"	("<symbol>" n> r n "</symbol>" >))
   (math	"n"	"<commonname>"	"commonname"	("<commonname>" n> r n "</commonname>" >))
   (math	"t"	"<type"	"type"	("<type system=\"" p "\">" n> r n "</type>" >))
   (math	"f"	"<FMP>"	"FMP"	("<FMP>" n> r n "</FMP>" >))
   (math	"u"	"<assumption"	"assumption"	("<assumption xml:id=\"" p "\">" n> r n "</assumption>" >))
   (math	"c"	"<conclusion"	"conclusion"	("<conclusion xml:id=\"" p "\">" n> r n "</conclusion>" >))
   (math	"x"	"<axiom"	"axiom"	("<axiom xml:id=\"" p "\">" n> r n "</axiom>" >))
   (math	"d"	"<definition"	"definition"	("<definition xml:id=\"" p "\" for=\"" p "\">" n> r n "</definition>" >))
   (math	"="	"<requation>"	"requation"	("<requation>" n> r n "</requation>" >))
   (math	"%"	"<measure>"	"measure"	("<measure>" n> r n "</measure>" >))
   (math	"o"	"<ordering>"	"ordering"	("<ordering>" n> r n "</ordering>" >))
   (math	"a"	"<adt"	"adt"	("<adt xml:id=\"" p "\">" n> r n "</adt>" >))
   (math	"s"	"<sortdef"	"sortdef"	("<sortdef xml:id=\"" p "\">" n> r n "</sortdef>" >))
   (math	"i"	"<insort"	"insort"	("<insort for=\"" p "\"/>"))
   (math	"k"	"<constructor"	"constructor"	("<constructor No=\"" p "\">" n> r n "</constructor>" >))
   (math	"r"	"<recognizer"	"recognizer"	("<recognizer No=\"" p "\">" n> r n "</recognizer>" >))
   (math	"$"	"<argument"	"argument"	("<argument sort=\"" p "\">" n> r n "</argument>" >))
   (math	"*"	"<selector"	"selector"	("<selector No=\"" p "\">" n> r n "</selector>" >))
   (math	"!"	"<assertion"	"assertion"	("<assertion xml:id=\"" p "\">" n> r n "</assertion>" >))
   (math	"?"	"<alternative"	"alternative"	("<alternative theory=\"" p "\" entailed-by=\"" p "\" entails=\"" p "\" entailed-by-thm=\"" p "\" entails-thm=\"" p "\" xml:id=\"" p "\" for=\"" p "\">" n> r n "</alternative>" >))
   (math	"p"	"<proof"	"proof"	("<proof xml:id=\"" p "\" for=\"" p "\">" n> r n "</proof>" >))
   (math	"0"	"<proofobject"	"proofobject"	("<proofobject xml:id=\"" p "\" for=\"" p "\">" n> r n "</proofobject>" >))
   (math	"m"	"<metacomment>"	"metacomment"	("<metacomment>" n> r n "</metacomment>" >))
   (math	"-"	"<derive"	"derive"	("<derive xml:id=\"" p "\">" n> r n "</derive>" >))
   (math	"q"	"<conclude>"	"conclude"	("<conclude>" n> r n "</conclude>" >))
   (math	"h"	"<hypothesis"	"hypothesis"	("<hypothesis xml:id=\"" p "\">" n> r n "</hypothesis>" >))
   (math	"~"	"<method"	"method"	("<method xref=\"" p "\">" n> r n "</method>" >))
   (math	"1"	"<premise"	"premise"	("<premise xref=\"" p "\"/>"))
   (math	"e"	"<example"	"example"	("<example xml:id=\"" p "\" for=\"" p "\">" n> r n "</example>" >))

					; theories elements
   (theory	"t"	"<theory"	"theory"	("<theory xml:id=\"" p "\">" n> r n "</theory>" >))
   (theory	"i"	"<imports"	"imports"	("<imports xml:id=\"" p "\" from=\"" p "\">" n> r n "</imports>" >))
   (theory	"m"	"<morphism>"	"morphism"	("<morphism>" n> r n "</morphism>" >))
   (theory	"q"	"<inclusion"	"inclusion"	("<inclusion via=\"" p "\"/>"))
   (theory	"r"	"<theory-inclusion"	"theory-inclusion"	("<theory-inclusion xml:id=\"" p "\" from=\"" p "\" to=\"" p "\">" n> r n "</theory-inclusion>" >))
   (theory	"d"	"<decomposition"	"decomposition"	("<decomposition links=\"" p "\" xml:id=\"" p "\" for=\"" p "\"/>"))
   (theory	"a"	"<axiom-inclusion"	"axiom-inclusion"	("<axiom-inclusion xml:id=\"" p "\" from=\"" p "\" to=\"" p "\">" n> r n "</axiom-inclusion>" >))
   (theory	"p"	"<path-just"	"path-just"	("<path-just local=\"" p "\" globals=\"" p "\"/>"))
   (theory	"o"	"<obligation"	"obligation"	("<obligation induced-by=\"" p "\" assertion=\"" p "\"/>"))

					; auxiliary elements
   (auxiliary	"e"	"<exercise"	"exercise"	("<exercise xml:id=\"" p "\" for=\"" p "\">" n> r n "</exercise>" >))
   (auxiliary	"h"	"<hint>"	"hint"	("<hint>" n> r n "</hint>" >))
   (auxiliary	"s"	"<solution>"	"solution"	("<solution>" n> r n "</solution>" >))
   (auxiliary	"m"	"<mc>"	"mc"	("<mc>" n> r n "</mc>" >))
   (auxiliary	"x"	"<choice>"	"choice"	("<choice>" n> r n "</choice>" >))
   (auxiliary	"a"	"<answer"	"answer"	("<answer verdict=\"" p "\">" n> r n "</answer>" >))
   (auxiliary	"l"	"<omlet"	"omlet"	("<omlet>" n> r n "</omlet>" >))
   (auxiliary	"p"	"<private"	"private"	("<private xml:id=\"" p "\">" n> r n "</private>" >))
   (auxiliary	"c"	"<code"	"code"	("<code xml:id=\"" p "\">" n> r n "</code>" >))
   (auxiliary	"i"	"<input>"	"input"	("<input>" n> r n "</input>" >))
   (auxiliary	"o"	"<output>"	"output"	("<output>" n> r n "</output>" >))
   (auxiliary	"f"	"<effect>"	"effect"	("<effect>" n> r n "</effect>" >))
   (auxiliary	"d"	"<data>"	"data"	("<data>" n> r n "</data>" >))
   (auxiliary	"g"	"<ignore>"	"ignore"	("<ignore>" n> r n "</ignore>" >))

					; presentation elements
   (presentation	"p"	"<presentation"	"presentation"	("<presentation for=\"" p "\">" n> r n "</presentation>" >))
   (presentation	"u"	"<use"	"use"	("<use format=\"" p "\">" n> r n "</use>" >))
   (presentation	"o"	"<omstyle"	"omstyle"	("<omstyle element=\"" p "\">" n> r n "</omstyle>" >))
   (presentation	"x"	"<xslt"	"xslt"	("<xslt format=\"" p "\">" n> r n "</xslt>" >))
   (presentation	"s"	"<style"	"style"	("<style format=\"" p "\">" n> r n "</style>" >))
   (presentation	"e"	"<element"	"element"	("<element name=\"" p "\">" n> r n "</element>" >))
   (presentation	"a"	"<attribute"	"attribute"	("<attribute name=\"" p "\">" n> r n "</attribute>" >))
   (presentation	"v"	"<value-of"	"value-of"	("<value-of select=\"" p "\"/>"))
   (presentation	"t"	"<text>"	"text"	("<text>" n> r n "</text>" >))
   (presentation	"r"	"<recurse"	"recurse"	("<recurse/>"))

					; open math variant
   (om	"s"	"<OMS"	"OMS"	("<OMS name=\"" p "\" cd=\"" p "\"/>"))
   (om	"v"	"<OMV"	"OMV"	("<OMV name=\"" p "\"/>"))
   (om	"r"	"<OMR"	"OMR"	("<OMR xlink:href=\"" p "\"/>"))
   (om	"i"	"<OMI>"	"OMI"	("<OMI>" n> r n "</OMI>" >))
   (om	"b"	"<OMB>"	"OMB"	("<OMB>" n> r n "</OMB>" >))
   (om	"l"	"<OMBVAR>"	"OMBVAR"	("<OMBVAR>" n> r n "</OMBVAR>" >))
   (om	"g"	"<OMSTR>"	"OMSTR"	("<OMSTR>" n> r n "</OMSTR>" >))
   (om	"f"	"<OMF"	"OMF"	("<OMF/>"))
   (om	"a"	"<OMA>"	"OMA"	("<OMA>" n> r n "</OMA>" >))
   (om	"d"	"<OMBIND>"	"OMBIND"	("<OMBIND>" n> r n "</OMBIND>" >))
   (om	"t"	"<OMATTR>"	"OMATTR"	("<OMATTR>" n> r n "</OMATTR>" >))
   (om	"o"	"<OMOBJ>"	"OMOBJ"	("<OMOBJ>" n> r n "</OMOBJ>" >))
   (om	"e"	"<OME>"	"OME"	("<OME>" n> r n "</OME>" >))
   (om	"1"	"<OMATP>"	"OMATP"	("<OMATP>" n> r n "</OMATP>" >))

					; dublin core
   (dc	"m"	"<metadata>"	"metadata"	("<metadata>" n> r n "</metadata>" >))
   (dc	"3"	"<dc:contributor>"	"contributor"	(& "<dc:contributor> " omdoc-address-string p " </dc:contributor>" > %))
;   (dc	"c"	"<Creator>"	"Creator"	(& "<Creator role=\"aut" p "\"> " omdoc-address-string p " </Creator>" > %))
   (dc	"c"	"<dc:creator>"	"creator"	(& "<dc:creator role=\"aut" p "\"> " omdoc-name-string p " </dc:creator>" > %))
   (dc	"j"	"<Project>"	"Project"	("<Project> " r " </Project>" >))
   (dc	"t"	"<dc:title>"	"title"	("<dc:title> " r " </dc:itle>" >))
   (dc	"s"	"<dc:ubject>"	"subject"	("<dc:subject> " r " </dc:subject>" >))
   (dc	"x"	"<dc:escription>"	"description"	("<dc:description>" n> r n "</dc:description>" >))
   (dc	"p"	"<dc:publisher>"	"publisher"	("<dc:publisher> " r " </dc:ublisher>" >))
   (dc	"y"	"<dc:type>"	"type"	("<dc:type>" n> r n "</dc:type>" >))
   (dc	"f"	"<dc:format>"	"format"	("<dc:format> " r " </dc:ormat>" >))
   (dc	"b"	"<dc:source>"	"source"	("<dc:source> " r " </dc:source>" >))
   (dc	"l"	"<dc:language>"	"sanguage"	("<dc:language> " r " </dc:anguage>" >))
   (dc	"r"	"<dc:relation>"	"relation"	("<dc:relation> " r " </dc:relation>" >))
   (dc	"i"	"<dc:rights>"	"rights"	("<dc:rights> " r " </dc:rights>" >))
   (dc	"d"	"<dc:date>"	"date"	(& "<dc:date action=\"created" p "\"> " (omdoc-default-insert-timestamp) p " </dc:ate>" > %))
   (dc	"1"	"<dc:dentifier>"	"identifier"	("<dc:identifier> " r " </dc:dentifier>" >))
   ))

(tempo-define-template "omdoc-skeleton" omdoc-new-buffer-template
		       nil
		       "Insert a skeleton for an OMDoc document")


;;}}}
;;{{{ Menu support

;; menus are built for easymenu. omdoc-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar omdoc-novice-menu
  '("OMDoc"
    ["Insert omgroup" tempo-template-omdoc-omgroup t]
    ["Insert CMP" tempo-template-omdoc-cmp t]
    ["Insert omtext" tempo-template-omdoc-omtext t]
    ["Insert FMP" tempo-template-omdoc-fmp t]
    ["Insert theory" tempo-template-omdoc-theory t]
    ["Insert example" tempo-template-omdoc-example t]
    ["Insert omlet" tempo-template-omdoc-omlet t]
    ["Insert presentation" tempo-template-omdoc-presentation t]
    ["Insert OMOBJ" tempo-template-omdoc-omobj t]
    ["Insert OMS" tempo-template-omdoc-oms t]
    ["Turn on Expert Menu" omdoc-toggle-expert-menu t])
  "Menu for novices, only installed if `omdoc-use-expert-menu is nil'")

(defun omdoc-menu nil
  "Return the proper menu. Looks at `omdoc-use-expert-menu'"
  (if omdoc-use-expert-menu
      (omdoc-expert-menu)
    omdoc-novice-menu))

(defun omdoc-rebuild-menu nil
  "Rebuild and install the OMDoc menu (using `easy-menu-define').
If `omdoc-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (omdoc-menu)))
    (easy-menu-remove menu)
    (easy-menu-define omdoc-mode-menu-symbol
		      omdoc-mode-map "OMDoc menus" menu)
    (easy-menu-add menu omdoc-mode-map)))

(defun omdoc-toggle-expert-menu (&optional arg)
  "Toggle full OMDoc menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq omdoc-use-expert-menu
	(if (null arg) (not omdoc-use-expert-menu)
	  (> (prefix-numeric-value arg) 0)))
  (omdoc-rebuild-menu))


;; Expert menus: consed up out of omdoc-installed-types
(defun omdoc-expert-menu ()
  "This menu is based on the current value of `omdoc-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq omdoc-mode-menu nil)
  
  ;; Cons in the toggle of the menu
  (setq omdoc-mode-menu
	(cons '["Turn on Novice Menu"
		omdoc-toggle-expert-menu t]
	      omdoc-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq omdoc-mode-menu
	(append omdoc-user-menu omdoc-mode-menu))

  ;; cons in the timestamp delimiters
  (setq omdoc-mode-menu
	(cons '["Insert Timestamp Delimiter"
		omdoc-insert-timestamp-delimiter-at-point t]
	      omdoc-mode-menu))
  
  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
	       (setq omdoc-mode-menu
		     (cons (omdoc-normalized-menu-for type)
			   omdoc-mode-menu))))
	  omdoc-installed-types)

  ;; now tack on our name
  (setq omdoc-mode-menu (cons "OMDoc" omdoc-mode-menu))
  omdoc-mode-menu)

(omdoc-rebuild-menu)

;;}}}

;;{{{ completion finder for tempo

(defvar omdoc-completion-finder
    "\\(\\(<\\|&\\).*\\)\\="
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags OMDoc might complete.

;;}}}

;;}}}

(provide 'omdoc-templates)
;;; omdoc-templates.el ends here

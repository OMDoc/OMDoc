;;;; xslide-data.el --- XSL IDE element and attribute data
;; $Id: xslide-data.el 3883 2003-04-11 19:13:01Z kohlhase $

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

;; Data about elements and attributes in XSL stylesheets collected
;; in one place

;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' for bug reports


;;;; Variables

(defvar xsl-xslt-ns-prefix "xsl"
  "*Prefix for the XSL namespace")

(defvar xsl-fo-ns-prefix "fo"
  "*Prefix for the Formatting Object namespace")

(defvar xsl-element-symbol-alist
  (list
   '("apply-imports"
     "empty"
     ()
     "xai")
   '("apply-templates"
     "block"
     ("select" "mode")
     "xat")
   '("attribute"
     "block"
     ("name" "namespace")
     "xa")
   '("attribute-set"
     "block"
     ("name" "use-attribute-sets")
     "xas")
   '("call-template"
     "block"
     ("name")
     "xct")
   '("choose"
     "block"
     ()
     "xc")
   '("comment"
     "block"
     ()
     "xcm")
   '("copy"
     "block"
     ("use-attribute-sets")
     "xcp")
   '("copy-of"
     "block"
     ("select")
     "xco")
   '("decimal-format"
     "block"
     ("name" "decimal-separator" "grouping-separator" "infinity"
      "minus-sign" "NaN" "percent" "per-mille" "zero-digit"
      "digit" "pattern-separator")
     "xdf")
   '("element"
     "block"
     ("name" "namespace" "use-attribute-sets")
     "xe")
   '("fallback"
     "block"
     ()
     "xfb")
   '("for-each"
     "block"
     ("select")
     "xfe")
   '("if"
     "block"
     ("test")
     "xif")
   '("import"
     "empty"
     ("href")
     "xim")
   '("include"
     "empty"
     ("href")
     "xinc")
   '("key"
     "block"
     ("name" "match" "use")
     "xk")
   '("message"
     "block"
     ("terminate")
     "xme")
   '("namespace-alias"
     "block"
     ("stylesheet-prefix" "result-prefix")
     "xna")
   '("number"
     "empty"
     ("level" "count" "from" "value" "format" "lang" "letter-value"
      "grouping-separator" "grouping-size")
     "xn")
   '("otherwise"
     "block"
     ()
     "xo")
   '("output"
     "empty"
     ("method" "version" "encoding" "omit-xml-declaration"
      "standalone" "doctype-public" "doctype-system"
      "cdata-section-elements" "indent" "media-type")
     "xout")
   '("param"
     "block"
     ("name" "select")
     "xpa")
   '("preserve-space"
     "empty"
     ("elements")
     "xps")
   '("processing-instruction"
     "block"
     ("name")
     "xpi")
   '("sort"
     "empty"
     ("select" "lang" "data-type" "order" "case-order")
     "xso")
   '("strip-space"
     "empty"
     ("elements")
     "xss")
   (list "stylesheet"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xs")
   '("template"
     "block"
     ("match" "mode" "priority" "name")
     "xt")
   '("text"
     "inline"
     ("disable-output-escaping")
     "xtxt")
   (list "transform"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xtran")
   '("value-of"
     "empty"
     ("select" "disable-output-escaping")
     "xvo")
   '("variable"
     "block"
     ("name" "select")
     "xva")
   '("when"
     "block"
     ("test")
     "xw")
   '("with-param"
     "block"
     ("name" "select")
     "xwp")))

(defvar xsl-attributes-alist
  (list
   '("NaN" "nan" ())
   '("cdata-section-elements" "cds" ())
   '("count" "cnt" ())
   '("data-type" "dt" ())
   '("decimal-separator" "ds" ())
   '("digit" "dig" ())
   '("disable-output-escaping" "doe" ())
   '("doctype-public" "dtp" ())
   '("doctype-system" "dts" ())
   '("elements" "ele" ())
   '("encoding" "enc" ())
   '("exclude-result-prefixes" "erp" ())
   '("extension-element-prefixes" "eep" ())
   '("format" "fmt" ())
   '("from" "fr" ())
   '("grouping-separator" "gsep" ())
   '("grouping-size" "gsiz" ())
   '("href" "href" ())
   '("id" "id" ())
   '("indent" "ind" ())
   '("infinity" "inf" ())
   '("lang" "l" ())
   '("letter-value" "lv" ())
   '("level" "lvl" ())
   '("match" "m" ())
   '("media-type" "mt" ())
   '("method" "meth" ())
   '("minus-sign" "ms" ())
   '("mode" "mo" ())
   '("n-digits-per-group" "ndpg" ())
   '("name" "n" ())
   '("namespace" "ns" ())
   '("omit-xml-declaration" "oxml" ())
   '("order" "o" ())
   '("pattern-separator" "ps" ())
   '("per-mille" "pm" ())
   '("percent" "perc" ())
   '("priority" "p" ())
   '("result-prefix" "rp" ())
   '("select" "s" ())
   '("standalone" "stand" ())
   '("stylesheet-prefix" "spr" ())
   '("terminate" "ter" ())
   '("test" "t" ())
   '("use" "use" ())
   '("use-attribute-sets" "ua" ())
   '("value" "v" ())
   '("version" "ver" ())
   '("xmlns" "xn" ())
   '("xmlns:fo" "xnf" ())
   '("xmlns:xsl" "xnx" ("http://www.w3.org/1999/XSL/Transform"))
   '("zero-digit" "zd" ())))

(defvar xsl-fo-symbol-alist
  (list
   '("basic-link" "inline" () "fbl")
   '("bidi-override" "inline" () "fbo")
   '("block" "block" () "fb")
   '("block-container" "block" () "fbc")
   '("character" "empty" () "fc")
   '("color-profile" "empty" () "fcp")
   '("conditional-page-master-reference" "empty" () "fcpmr")
   '("declarations" "block" () "fd")
   '("external-graphic" "empty" () "feg")
   '("float" "block" () "ff")
   '("flow" "block" () "ff")
   '("footnote" "block" () "ff")
   '("footnote-body" "block" () "ffb")
   '("initial-property-set" "empty" () "fips")
   '("inline" "inline" () "fi")
   '("inline-container" "inline" () "fic")
   '("instream-foreign-object" "block" () "fifo")
   '("layout-master-set" "block" () "flms")
   '("leader" "inline" () "fl")
   '("list-block" "block" () "flb")
   '("list-item" "block" () "fli")
   '("list-item-body" "block" () "flib")
   '("list-item-label" "block" () "flil")
   '("marker" "inline" () "fm")
   '("multi-case" "inline" () "fmc")
   '("multi-properties" "block" () "fmp")
   '("multi-property-set" "empty" () "fmps")
   '("multi-switch" "block" () "fms")
   '("multi-toggle" "inline" () "fmt")
   '("page-number" "empty" () "fpn")
   '("page-number-citation" "empty" () "fpnc")
   '("page-sequence" "block" () "fps")
   '("page-sequence-master" "block" () "fpsm")
   '("region-after" "empty" () "fra")
   '("region-before" "empty" () "frb")
   '("region-body" "empty" () "frb")
   '("region-end" "empty" () "fre")
   '("region-start" "empty" () "frs")
   '("repeatable-page-master-alternatives" "block" () "frpma")
   '("repeatable-page-master-reference" "empty" () "frpmr")
   '("retrieve-marker" "empty" () "frm")
   '("root" "block" () "fr")
   '("simple-page-master" "block" () "fspm")
   '("single-page-master-reference" "empty" () "fspm")
   '("static-content" "block" () "fsc")
   '("table" "block" () "ft")
   '("table-and-caption" "block" () "ftac")
   '("table-body" "block" () "ftb")
   '("table-caption" "block" () "ftc")
   '("table-cell" "block" () "ftc")
   '("table-column" "empty" () "ftc")
   '("table-footer" "block" () "ftf")
   '("table-header" "block" () "fth")
   '("table-row" "block" () "ftr")
   '("title" "inline" () "ft")
   '("wrapper" "inline" () "fw")))

(defvar xsl-fo-attribute-symbol-alist
  (list
   '("absolute-position" "ap")
   '("active-state" "as")
   '("alignment-adjust" "aa")
   '("alignment-baseline" "ab")
   '("auto-restore" "ar")
   '("azimuth" "a")
   '("background" "b")
   '("background-attachment" "ba")
   '("background-color" "bc")
   '("background-image" "bi")
   '("background-position" "bp")
   '("background-position-horizontal" "bph")
   '("background-position-vertical" "bpv")
   '("background-repeat" "br")
   '("baseline-shift" "bs")
   '("blank-or-not-blank" "bon")
   '("block-progression-dimension" "bpd")
   '("block-progression-dimension.maximum" "bpdmax")
   '("block-progression-dimension.minimum" "bpdmin")
   '("block-progression-dimension.optimum" "bpdopt")
   '("border" "b")
   '("border-after-color" "bac")
   '("border-after-precedence" "bap")
   '("border-after-style" "bas")
   '("border-after-width" "baw")
   '("border-after-width.conditionality" "bawc")
   '("border-after-width.length" "bawl")
   '("border-before-color" "bbc")
   '("border-before-precedence" "bbp")
   '("border-before-style" "bbs")
   '("border-before-width" "bbw")
   '("border-before-width.conditionality" "bbwc")
   '("border-before-width.length" "bbwc")
   '("border-bottom" "bb")
   '("border-bottom-color" "bbc")
   '("border-bottom-style" "bbs")
   '("border-bottom-width" "bbw")
   '("border-bottom-width.conditionality" "bbwc")
   '("border-bottom-width.length" "bbwl")
   '("border-collapse" "bc")
   '("border-color" "bc")
   '("border-end-color" "bec")
   '("border-end-precedence" "bep")
   '("border-end-style" "bes")
   '("border-end-width" "bew")
   '("border-end-width.conditionality" "bewc")
   '("border-end-width.length" "bewl")
   '("border-left" "bl")
   '("border-left-color" "blc")
   '("border-left-style" "bls")
   '("border-left-width" "blw")
   '("border-left-width.conditionality" "blwc")
   '("border-left-width.length" "blwl")
   '("border-right" "br")
   '("border-right-color" "brc")
   '("border-right-style" "brs")
   '("border-right-width" "brw")
   '("border-right-width.conditionality" "brwc")
   '("border-right-width.length" "brwl")
   '("border-separation.block-progression-direction" "bsbpd")
   '("border-separation.inline-progression-direction" "bsipd")
   '("border-spacing" "bs")
   '("border-start-color" "bsc")
   '("border-start-precedence" "bsp")
   '("border-start-style" "bss")
   '("border-start-width" "bsw")
   '("border-start-width.conditionality" "bswc")
   '("border-start-width.length" "bswl")
   '("border-style" "bs")
   '("border-top" "bt")
   '("border-top-color" "btc")
   '("border-top-style" "bts")
   '("border-top-width" "btw")
   '("border-top-width.conditionality" "btwc")
   '("border-top-width.length" "btwl")
   '("border-width" "bw")
   '("bottom" "b")
   '("break-after" "ba")
   '("break-before" "bb")
   '("caption-side" "cs")
   '("case-name" "cn")
   '("case-title" "ct")
   '("character" "ch")
   '("clear" "cl")
   '("clip" "cli")
   '("color" "c")
   '("color-profile-name" "cpn")
   '("column-count" "cc")
   '("column-gap" "cg")
   '("column-number" "cn")
   '("column-width" "cw")
   '("content-height" "ch")
   '("content-type" "ct")
   '("content-width" "cw")
   '("country" "c")
   '("cue" "c")
   '("cue-after" "ca")
   '("cue-before" "cb")
   '("destination-placement-offset" "dpo")
   '("direction" "d")
   '("display-align" "da")
   '("dominant-baseline" "db")
   '("elevation" "e")
   '("empty-cells" "ec")
   '("end-indent" "ei")
   '("ends-row" "er")
   '("extent" "e")
   '("external-destination" "ed")
   '("float" "f")
   '("flow-name" "fn")
   '("font" "f")
   '("font-family" "ff")
   '("font-selection-strategy" "fss")
   '("font-size" "fs")
   '("font-size-adjust" "fsa")
   '("font-stretch" "fs")
   '("font-style" "fs")
   '("font-variant" "fv")
   '("font-weight" "fw")
   '("force-page-count" "fpc")
   '("format" "f")
   '("glyph-orientation-horizontal" "goh")
   '("glyph-orientation-vertical" "gov")
   '("grouping-separator" "gs")
   '("grouping-size" "gs")
   '("height" "h")
   '("hyphenate" "hy")
   '("hyphenation-character" "hc")
   '("hyphenation-keep" "hk")
   '("hyphenation-ladder-count" "hlc")
   '("hyphenation-push-character-count" "hpc")
   '("hyphenation-remain-character-count" "hrcc")
   '("id" "i")
   '("indicate-destination" "id")
   '("initial-page-number" "ipn")
   '("inline-progression-dimension" "ipd")
   '("inline-progression-dimension.maximum" "ipdmax")
   '("inline-progression-dimension.minimum" "ipdmin")
   '("inline-progression-dimension.optimum" "ipdopt")
   '("internal-destination" "id")
   '("intrusion-displace" "id")
   '("keep-together.within-column" "ktc")
   '("keep-together.within-line" "ktl")
   '("keep-together.within-page" "ktp")
   '("keep-with-next.within-column" "kwnc")
   '("keep-with-next.within-line" "kwnl")
   '("keep-with-next.within-page" "kwnp")
   '("keep-with-previous.within-column" "kwpc")
   '("keep-with-previous.within-line" "kwpl")
   '("keep-with-previous.within-page" "kwpp")
   '("language" "la")
   '("last-line-end-indent" "lle")
   '("leader-alignment" "la")
   '("leader-length" "ll")
   '("leader-length.maximum" "llmax")
   '("leader-length.minimum" "llmin")
   '("leader-length.optimum" "llopt")
   '("leader-pattern" "lp")
   '("leader-pattern-width" "lpw")
   '("left" "le")
   '("letter-spacing" "ls")
   '("letter-value" "lv")
   '("line-height" "lh")
   '("line-height-shift-adjustment" "lhs")
   '("line-stacking-strategy" "lss")
   '("linefeed-treatment" "lt")
   '("margin" "m")
   '("margin-bottom" "mb")
   '("margin-left" "ml")
   '("margin-right" "mr")
   '("margin-top" "mt")
   '("marker-class-name" "mcn")
   '("master-name" "mn")
   '("master-reference" "mr")
   '("max-height" "mh")
   '("max-width" "mw")
   '("maximum-repeats" "mr")
   '("media-usage" "mu")
   '("min-height" "mh")
   '("min-width" "mw")
   '("number-columns-repeated" "ncr")
   '("number-columns-spanned" "ncs")
   '("number-rows-spanned" "nrs")
   '("odd-or-even" "ooe")
   '("orphans" "or")
   '("overflow" "ov")
   '("padding" "pd")
   '("padding-after" "pa")
   '("padding-before" "pb")
   '("padding-bottom" "pb")
   '("padding-end" "pe")
   '("padding-left" "pl")
   '("padding-right" "pr")
   '("padding-start" "ps")
   '("padding-top" "pt")
   '("page-break-after" "pba")
   '("page-break-before" "pbb")
   '("page-break-inside" "pbi")
   '("page-height" "ph")
   '("page-position" "pp")
   '("page-width" "pw")
   '("pause" "p")
   '("pause-after" "pa")
   '("pause-before" "pb")
   '("pitch" "p")
   '("pitch-range" "pr")
   '("play-during" "pd")
   '("position" "p")
   '("precedence" "p")
   '("provisional-distance-between-starts" "pdbs")
   '("provisional-label-separation" "pls")
   '("ref-id" "rid")
   '("reference-orientation" "ro")
   '("region-name" "rn")
   '("relative-align" "ra")
   '("relative-position" "rp")
   '("rendering-intent" "ri")
   '("retrieve-boundary" "rb")
   '("retrieve-class-name" "rcn")
   '("retrieve-position" "rp")
   '("richness" "rich")
   '("right" "rig")
   '("role" "ro")
   '("rule-style" "rs")
   '("rule-thickness" "rt")
   '("scaling" "sc")
   '("scaling-method" "sm")
   '("score-spaces" "ss")
   '("script" "scr")
   '("show-destination" "sde")
   '("size" "si")
   '("source-document" "sdo")
   '("space-after" "sa")
   '("space-before" "sb")
   '("space-end" "se")
   '("space-start" "ss")
   '("span" "spn")
   '("speak" "spe")
   '("speak-header" "sh")
   '("speak-numeral" "sn")
   '("speak-punctuation" "sp")
   '("speech-rate" "sr")
   '("src" "src")
   '("start-indent" "si")
   '("starting-state" "ss")
   '("starts-row" "sr")
   '("stress" "str")
   '("suppress-at-line-break" "salb")
   '("switch-to" "st")
   '("table-layout" "tl")
   '("table-omit-footer-at-break" "tofb")
   '("table-omit-header-at-break" "tohb")
   '("target-presentation-context" "tpc")
   '("target-processing-context" "tpc")
   '("target-stylesheet" "ts")
   '("text-align" "ta")
   '("text-align-last" "tal")
   '("text-altitude" "ta")
   '("text-decoration" "td")
   '("text-depth" "td")
   '("text-indent" "ti")
   '("text-shadow" "ts")
   '("text-transform" "tt")
   '("top" "top")
   '("treat-as-word-space" "taw")
   '("unicode-bidi" "ub")
   '("vertical-align" "va")
   '("visibility" "vi")
   '("voice-family" "vf")
   '("volume" "vo")
   '("white-space" "ws")
   '("white-space-collapse" "wsc")
   '("white-space-treatment" "wst")
   '("widows" "wdw")
   '("width" "w")
   '("word-spacing" "ws")
   '("wrap-option" "wo")
   '("writing-mode" "wm")
   '("xml:lang" "xl")
   '("z-index" "zi")))

(setq xsl-all-attribute-alist
      (sort
       (append
	xsl-attributes-alist
	xsl-fo-attribute-symbol-alist)
       (lambda (a b) (string< (car a) (car b)))))

(setq xsl-all-elements-alist
      (sort
       (append
	(mapcar (lambda (x)
		  (cons (if xsl-xslt-ns-prefix
			    (concat xsl-xslt-ns-prefix ":" (car x))
			  (car x))
			(cdr x)))
		xsl-element-symbol-alist)
	(mapcar (lambda (x)
		  (if xsl-fo-ns-prefix
		      (cons
		       (concat xsl-fo-ns-prefix ":" (car x))
		       (cdr x))
		    x))
		xsl-fo-symbol-alist))
       (lambda (a b) (string< (car a) (car b)))))

(provide 'xslide-data)

;; end of xslide-data.el

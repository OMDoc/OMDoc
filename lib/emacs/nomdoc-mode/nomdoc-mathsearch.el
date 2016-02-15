;; $Id: nomdoc-mathsearch.el 6403 2007-05-29 04:46:48Z kohlhase $ $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.3/lib/emacs/nomdoc-mode/nomdoc-mathsearch.el $

;; nomdoc-mathsearch.el --- nOMDoc math search mode & related routines

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

(defface clickable-text-emacs-face
  `( (((class color) (background light))
      (:foreground "maroon" :bold t))
     (t (:foreground "maroon" :bold t)))
  "Basic emacs-link face for clickable text.")

(defface clickable-text-internet-face
  `( (((class color) (background light))
      (:foreground "blue" :underline t))
     (t (:foreground "blue" :underline t)))
  "Basic internet-like face for clickable text.")

(defun linkify (from to event &optional internet-like)
  "Puts text properties of a given region to make it look like a link and associates an OnClick event for it. An optional argument internet-like makes the function produce internet-like links (underlined and blue), while its ommision makes it produce emacs-like links (maroon and bold)."
  (put-text-property from to 'mouse-face 'highlight)
  (if internet-like
      (put-text-property from to 'face 'clickable-text-internet-face)
      (put-text-property from to 'face 'clickable-text-emacs-face))
   ; associating the given function to it:
   ; we define a map and assign it to KEYMAP property of the text itself
   (let ((map (make-sparse-keymap)))
       (define-key map [mouse-1] event)
       (put-text-property from to 'keymap map)))

; possibly already exists, but couldn't find it
(defun readln (buffer)
  "Reads a line of a given buffer (at point) and returns it."
  (with-current-buffer buffer
    (if (re-search-forward "\\(.*\\)\r\n" nil t)
	(match-string 1)
      nil)))
  
(defvar display-offset 0
  "The offset rom which we start displaying search results.")

(defvar search-id ""
  "Search session ID, to be used when sending CONT command.")

(defun linify (query)
  (if (string-match "\n" query)
      (linify (replace-match " " nil nil query))
    query))

(defun send-query (command &optional arg)
  "Call the MWS server for a given command and return the output in a buffer"
  ; creating a new fresh output buffer
  (with-current-buffer
      (setq output-buffer (get-buffer-create " output-buffer"))
    (erase-buffer))

  (message "Searching...")
					; the IP is 212.201.49.3
  (setq mws-con (open-network-stream "mws" output-buffer
				     "raspberry.eecs.iu-bremen.de" 19843))
  ; set a dummy sentinel, because I don't want
  ; the abnormal exit error message [a bug in MWS?]
  (set-process-sentinel mws-con (lambda (a b)))
  (process-send-string mws-con 
	(concat
	   command "\r\n"
	   (cond
	    ((string-equal "CONT" command) 
	       (concat
		(int-to-string nomdoc-results-per-page) "\r\n"
		(int-to-string display-offset) "\r\n" 
		search-id "\r\n"))
	    ((string-equal "STRING" command)
	       (concat
		(int-to-string nomdoc-results-per-page) "\r\n"
		"0\r\n"
		"#" arg "\r\n")) ; arg = query
	    ((string-equal "XMLQ" command)
	       (concat
		(int-to-string nomdoc-results-per-page) "\r\n"
		"0\r\n"
		(linify arg) "\r\n")) ; arg = query

	    ((string-equal "SRC" command)
	       (concat
		search-id "\r\n"
		(int-to-string arg) "\r\n"))))); arg = the result's number in the list

  ; wait in steps of 0.2s until it's closed or we got bored 
  (setq wait-time 0)
  (while (and (< wait-time 10) (equal (process-status mws-con) 'open))
    (sit-for 0.2)
    (setq wait-time (+ wait-time 0.2)))
  (if (>= wait-time 10) ; we get bored in 10 seconds
	(error "Time out!"))
  ; search finished, delete the message "Searching"
  (message "")
  output-buffer
)

(defvar nomdoc-document-buffer nil
  "The buffer of the document the user is working on")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN SEARCHING ROUTINES ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; queries such as "#int(bvarset(bvar(@t)),@domain,power(@fun(@t),nr(2)))")
(defun nomdoc-math-search-string (query)
  "Searches for math formulae online given a search query in internal string representation format."
 (interactive "sEnter internal representation query: ")
 (setq nomdoc-document-buffer (current-buffer))
 (nomdoc-init-math-search "STRING" query)
)

(defun nomdoc-init-math-search (command query)
 (setq display-offset 0)
 (setq display-buffer (get-buffer-create "Search Results"))
 (set-buffer display-buffer)
;  fill in the buffer with links, a command line and a help info line
 (populate-buffer (send-query command query))
 (math-search-mode)
)

(setq query-buffer nil)

; Here we mess up with popping windows, I am not sure the arrangment of windows
; after the second C-c x is what we want exactly
(defun nomdoc-math-search-xmlq ()
  "Searches for math formulae online given a search query in XMLQ format. If a region
is active, copies it into the query buffer. Otherwise, the buffer is initially empty. After the XML Query is made in that buffer, press C-c x again to execute it."
  (interactive)
  ; if we are in the XML Buffer, execute it
  (if (string= (buffer-name) " XMLQ Buffer")
      (if (= 0 (buffer-size))
	  (message "Please enter a query.")
	(nomdoc-init-math-search "XMLQ" (buffer-string)))
    ; otherwise, create it
    (setq nomdoc-document-buffer (current-buffer))
    (setq query-buffer (get-buffer-create " XMLQ Buffer"))
    (if mark-active
	  (copy-to-buffer query-buffer (region-beginning) (region-end)))

    (set-buffer query-buffer)
    ; disable automatic insertion of the default skeleton in XMLQ buffer
    (setq temp nomdoc-build-new-buffer)
    (setq nomdoc-build-new-buffer nil)
    (nomdoc-mode)
    (setq nomdoc-build-new-buffer temp)
    ; if there is any initial text, indent it 
    (indent-region (point-min) (point-max) nil)
    (pop-to-buffer query-buffer)
    (message "Press C-c x again when finished to execute the query.")
))
  
(defvar math-search-mode-map
  (let ((s (make-keymap)))
      ; both click-link and click-insert-xml
   (define-key s [(return)] 'math-search-return-action)
   (define-key s "n" 'math-search-click-next)
   (define-key s "p" 'math-search-click-previous)
   (define-key s "a" 'math-search-again)
   (define-key s "q" 'math-search-quit)
 s))

(define-derived-mode math-search-mode fundamental-mode
  "MathSearch mode"
  "This mode outputs search results of a math search query.

Click on a link or press RETURN on it while point is located on the link to follow it. 
Click NEXT or press `n` to see the next \\[nomdoc-results-per-page] results if any. Similarly click PREV or press `p` to see previous results. 
Click INSERT-XML or press RETURN on it to output an XML representation (Content MathML or OpenMath) of the matched formula to your document at point. 
Press `a` to clear the results and perform another search. 
Press `q` to quit."
  (setq browse-url-generic-program nomdoc-browser)
)

(defvar xml-source-links nil
 "List of source links for currently displayed results.")

(defvar click-links nil
 "List of links to go to for currently displayed results.")

; the number of found results
(defvar found-results 0 
 "The number of found results")

(defun populate-buffer (output)
  ; if display buffer already exists, erase its content
  (setq buffer-read-only nil)
  (erase-buffer)

  ; reading initialization
  (with-current-buffer output
    (goto-char (point-min)))

  (if (string= (readln output) "ERROR")
     (error (concat "Error: " (readln output) ". Please check your syntax.")))
  ; otherwise the string is OK

  (setq search-id (readln output))
  (readln output) ; time needed for the search, discarded
  (setq found-results (string-to-int (readln output)))

  ; cnt stores the current line number
  (setq cnt display-offset)
  
  (setq click-links nil)
  (setq xml-source-links nil)
  
  (while (setq title (readln output))
     ; insert an enumerated title
     (setq cnt (+ cnt 1))
     (insert-string cnt ".")
     (setq beg (point))
     (insert title " ")
     (linkify beg (+ beg (length title)) 'math-search-click-link t)
     (setq beg (point))
     (insert "INSERT-XML" "\n")
     (linkify beg (+ beg (length "INSERT-XML")) 'math-search-click-insert-xml)

     ;   format of the link line: 
     ;   TYPE-LETTER CLICK-LINK 0x01 XML-SOURCE-LINK
     ; split it across 0x01
     (setq link-line (split-string (readln output) (char-to-string 1)))

     ; cut TYPE-LETTER, for now disregard it
     (setq click-links (append click-links (list (substring (car link-line) 1))))
     ; if source link is not given, put a "" in its place in xml-source-links
     (setq xml-source-links (append xml-source-links 
 	   (if (= 2 (length link-line)) (cdr link-line) (list ""))))

     (setq description (readln output))
     ; cut it if it's too long
     (if (> (length description) 160)
 	(setq description (concat (substring description 0 157) "...")))

     ; do not insert empty or nearly empty descriptions
     (if (> (length description) 2)
	 (insert description "\n"))
    ) ; from while
  
   (setq info-string 
 	(concat
 	 "Results " 
 	 (int-to-string (min found-results (+ 1 display-offset)))
 	 " - "
 	 (int-to-string (min found-results (+ display-offset nomdoc-results-per-page)))
 	 " of "
 	 (int-to-string found-results)))
  
  (insert    "\nPREV                 "
 	     info-string
	     "                NEXT")
  
   (setq beg (line-beginning-position))
   (linkify (+ beg 0) (+ beg 4) 'math-search-click-previous) ; PREV button
   (linkify (+ beg 37 (length info-string))
 	   (+ beg 41 (length info-string)) 'math-search-click-next) ; NEXT button

   ; shrink or grow to fit
   (pop-to-buffer display-buffer)
   (set-window-text-height nil (count-screen-lines))

   ; after feeding it, disable changing
   (setq buffer-read-only t)
   (goto-char (point-min))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; math-search-mode commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-search-return-action ()
  "Executes links and buttons thru keyboard"
  (interactive)
 (save-excursion
   (setq beg (point))
   (beginning-of-line)
   (if (looking-at "\\([0-9]+\\)\\.")
     (progn
       (goto-char beg)
       (if (re-search-forward ".+ " (line-end-position) t) ; we are in link
	   (math-search-click-link)
	 (math-search-click-insert-xml))))))

(defun math-search-click-link ()
 "A function executed when a result link is clicked. Shows the browser."
  (interactive)
 (save-excursion
   (beginning-of-line)
   (if (looking-at "\\([0-9]+\\)\\.")
       ; call the browser
       (browse-url-generic 
	(nth (- (- (string-to-int (match-string 1)) 1) display-offset) click-links))
)))

(defun math-search-click-next ()
 "A function executed when the link NEXT is clicked."
 (interactive)
 (setq display-offset (+ display-offset nomdoc-results-per-page))
 (if (>= display-offset found-results)
     (progn
       (message "End reached")
       (setq display-offset (- display-offset nomdoc-results-per-page)))
   (populate-buffer (send-query "CONT" ))))

(defun math-search-click-previous ()
 "A function executed when the link PREVIOUS is clicked."
 (interactive)
 (setq display-offset (- display-offset nomdoc-results-per-page))
 (if (< display-offset 0)
     (progn
       (message "Beginning reached")
       (setq display-offset (+ display-offset nomdoc-results-per-page)))
   (populate-buffer (send-query "CONT"))))

(defun xmlify-buffer (buf)
 "Replaces all &gt; &lt; &quot; ... with appropriate characters"
 (with-current-buffer buf
   (goto-char (point-min))
   ; delete OK and XML version lines
   (delete-region (point) (line-beginning-position 3))
   ; to return a flag that tell us if the XML huck is OpenMath or CMathML
   (setq isMathML (looking-at ".*MathML"))
   (while (re-search-forward 
	    (concat "\\(&lt;\\)\\|"
	            "\\(&gt;\\)\\|"
		    "\\(&apos;\\)\\|"
		    "\\(&quot;\\)\\|"
		    "\\(&amp;\\)\\|"
		    "\\(\r\n\\)\\|"
		    "\\(ns_m\\)") nil t)
     (cond
      ((match-string 1) (replace-match "<"))
      ((match-string 2) (replace-match ">"))
      ((match-string 3) (replace-match "`"))
      ((match-string 4) (replace-match "\""))
      ((match-string 5) (replace-match "&"))
      ((match-string 6) (replace-match "\n"))
      ((match-string 7) (replace-match "m"))
      ))
 (list buf isMathML)))

(defun math-search-click-insert-xml ()
 "Inserts the source Math XML into the active document."
 (interactive)
     (setq ch (read-char "Would you like Content MathML (c) or OpenMath ML (o)?"))
     (if (and (not (char-equal ch ?o)) (not (char-equal ch ?c)))
	 (error "Wrong input"))
 (save-excursion
   (beginning-of-line)
   (looking-at "\\([0-9]+\\)\\.")
					; car of output is the result of the query,
					; and the second element is a MathML/OpenMathML flag
					; counting on the server starts from zero so -1 :)
   (setq output
	 (xmlify-buffer (send-query "SRC" 
				    (- (string-to-int (match-string 1)) 1))))
   
   (setq xml-string 
	 (with-current-buffer (car output) 
	   (buffer-string)))
     
   (with-current-buffer nomdoc-document-buffer
     (setq beg (point))
       
			; run XSLT stylesheets here if needed to perform conversions
			; form one mathematical format to another
       
			; conversion from CMathML to OpenMath is needed or
     (if (or (and (nth 1 output) (char-equal ch ?o))
					; conversion from OpenMath to CMathMl is needed
	     (and (not (nth 1 output)) (char-equal ch ?c)))
	 (progn
					; create a temporary file as input
	   (with-current-buffer (car output)
	     (write-file (concat nomdoc-xsl-dir "/temp.xml")))
					; run stylesheets
	   (call-process "xsltproc" nil nomdoc-document-buffer nil 
			 (concat nomdoc-xsl-dir 
				 (if (char-equal ch ?o) "/cmmltoom.xsl" "/omtocmml.xsl"))
			 (concat nomdoc-xsl-dir "/temp.xml"))
					; delete the temporary file
	   (delete-file (concat nomdoc-xsl-dir "/temp.xml"))
	   )
       ; else, it's the right format, so just insert it
       (insert xml-string))
     
			; fit in the insertion in the context of the whole document
     (indent-region beg (point) nil)
     (goto-char beg)
     (delete-region (point) (line-beginning-position 2)))))

(defun math-search-again (query)
  "A function that resets the search to enable the user to make another search. It clears the search result's buffer and waits for a new query."
  (interactive "sEnter internal representation query: ")
  (setq display-offset 0)
  (populate-buffer (send-query "STRING" query))
)

(defun math-search-quit ()
  (interactive)
 "Exits the search results mode."
 (kill-buffer nil)
 (delete-window))

(provide 'nomdoc-mathsearch)

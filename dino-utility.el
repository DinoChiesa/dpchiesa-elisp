;;; dino-utility.el --- utility functions for dino
;;
;; Author: Dino Chiesa
;; Created: Wed, 17 Jul 2013  12:06
;; Package-Requires: ()
;; URL:
;; X-URL:
;; Version: 2013.07.17
;; Keywords: utility
;; License: New BSD

;;; Commentary:

;; -none-

;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2013, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;

;;; Code:

(require 'cl)

;; when copying binary files into a clipboard buffer
(fset 'dinoch-b64-copy
      [escape ?  escape ?> escape ?x ?b ?a ?s ?e ?6 ?4 ?- ?e ?n ?c tab return ?\C-w ?\C-y])

;; when pasting the base64 stuff from binary files
(fset 'dinoch-b64-paste
      [escape ?x ?r backspace ?e ?r ?a ?s ?e ?- ?b ?u tab return ?\C-y escape ?x ?b ?a ?s ?e ?6 ?4 ?- ?d ?e ?c ?o tab return ?\C-x ?\C-s])

(defun dino-fixup-linefeeds ()
  "Dino's function to replace the CR-LF of a DOS ASCII file to a LF for Unix."
  (interactive)
  (save-excursion
    (while (search-forward "\xd" nil t)
      (replace-match "" nil t))))


(defun dino-add-path-if-not-present (pathlist)
  "Add each directory in the PATHLIST to the system path and to `exec-path'.

This is done intelligently: path directories are added only if the path exists,
and is not already present on the path."
  (let ((path-elts (split-string (getenv "PATH") ":")))
    (dolist (path pathlist)
      (and (file-directory-p path)
           (progn
             (and (not (member path path-elts))
                  (setenv "PATH" (concat (getenv "PATH") ":" path)))
             (add-to-list 'exec-path path))))))





(defun dino-toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
This works only when the frame is split into exactly two windows."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can toggle only if the frame is split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame


(defun dino-indent-buffer ()
  "Dino's function to re-indent an entire buffer; helpful in progmodes
like XML mode or csharp mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun dino-toggle-buffer-modified ()
  "Toggle the buffer-modified-p value for the current buffer."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun dino-do-markdown ()
  "Remove the `delete-trailing-whitespace' fn from the `before-save-hook'.
This is important when editing markdown files which use trailing whitespace
to indicate a newline."
  (interactive)
  ;; see http://stackoverflow.com/questions/1931784
  (remove-hook 'write-contents-functions 'dino-delete-trailing-whitespace))


(defun dino-delete-trailing-whitespace ()
  (save-excursion
    (delete-trailing-whitespace)))


 (defvar dino-no-untabify-modes '(makefile-mode BSDmakefile)
  "Normally my setup untabifies buffers before save. This list
provides a set of modes for which no untabify is desired.")

(setq-default indent-tabs-mode nil) ;Use spaces not tabs!

;;(setq dino-no-untabify-modes '(makefile-mode BSDmakefile))


(defun dino-untabify-maybe ()
  "Untabify the current buffer, if the major-mode of the buffer is not
in the list `dino-no-untabify-modes'
"
  (interactive)
  (when (or (not dino-no-untabify-modes)
            (every '(lambda (m) (not (derived-mode-p m)))
                   dino-no-untabify-modes))

    (untabify 0 (point-max))))

(defun dino-untabify-unconditionally ()
  "Untabify the current buffer completely and unconditionally."
  (untabify (point-min) (point-max)))

(add-hook 'before-save-hook 'dino-untabify-maybe)


;; put an href around the url at point.
(fset 'dino-href-url
      [?< ?  backspace ?a ?  ?h ?r ?e ?f ?= ?\" ?\C-s ?  ?\C-b ?\" ?> ?\C-r ?/ ?\C-f escape ?  ?\C-s ?\" ?\C-b ?\C-w ?\C-y ?\C-f ?\C-f ?\C-y ?< ?/ ?a ?> ?< ?b ?r ?> return])


(defun revert-buffer-unconditionally ()
  "revert the current buffer unconditionally.  See also, the auto-revert minor mode."
  (interactive)
  (revert-buffer t t))

(defun dino-resize-big ()
  "quick resize to 128x72"
  (interactive)
  (set-frame-height (selected-frame) 68)
  (set-frame-width (selected-frame) 128))


(defun dino-toggle-truncation ()
  "Joe's function to toggle the state of the truncate-lines variable"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display))

;; (defun dino-remove-trailing-whitespace ()
;; "For each line in region, Convert multiple spaces at end of line to just one."
;;   (interactive)
;;   (let ((beg (point-min))
;;         (end (point-max)))
;;   (save-excursion
;;     (goto-char beg)
;;     (while (and (< (point) end)
;;                 (re-search-forward " +$" end t))
;;       (just-one-space)
;;       (backward-delete-char-untabify 1)
;;       )
;; )))


;; (defun dino-insert-timestamp ()
;;   "function to insert timestamp at point. format: DayOfWeek, Date Month Year   24hrTime"
;;   (interactive)
;;   (let* ((localstring (current-time-string))
;;         (mytime (concat "....dinoch...."
;;                          (substring localstring 0 3)  ;day-of-week
;;                          ", "
;;                          (substring localstring 8 10) ;day number
;;                          " "
;;                          (substring localstring 4 7)  ;month
;;                          " "
;;                          (substring localstring 20 24 ) ;4-digit year
;;                          "...."
;;                          (substring localstring 11 16 ) ;24-hr time
;;                          "....\n"
;;                          )))
;;     (insert mytime))
;; )

(defvar dino-timeofday--last-inserted-string nil
  "holder of the last inserted string with `dino-insert-timeofday'. Used
to determine if we need to rotate through various formats.")

(defvar dino-timeofday--last-inserted-marker nil
  "marker companion to the above.")


(defun dino-insert-timeofday ()
  "Insert a string representing the time of day at point. The
format varies depending on the mode, or if the minibuffer is
active or not.  If the minibuffer is active, or if the mode is
`wdired-mode', then the format used is like this:

  20130820-0848

This allows insertion of the time of day into filenames.
Otherwise, the format is like this:

  Tuesday, 20 August 2013, 08:48

"
  (interactive)
  (let ((tf1 "%Y%m%d-%H%M")
        (tf2 "%A, %e %B %Y, %H:%M")
        tf)

    (setq tf
         (if (or (window-minibuffer-p) (equal major-mode 'wdired-mode))
             tf1 tf2))

    ;; If the user has invoked this cmd twice in succession, then swap
    ;; formats. Only if not in the minibuffer!  Using colons in the
    ;; minibuffer causes emacs to go haywire for me.
    (if (and (boundp 'dino-timeofday--last-inserted-string)
             (stringp dino-timeofday--last-inserted-string)
             (markerp dino-timeofday--last-inserted-marker)
             (marker-position dino-timeofday--last-inserted-marker)
             (or (eq last-command 'this-command)
                 (= (point) dino-timeofday--last-inserted-marker)))

        (progn
          ;; remove prior insertion
          (backward-delete-char-untabify (length dino-timeofday--last-inserted-string))
          ;; use "the other" format
          (setq tf tf1)))

    ;; examples:
    ;; 19960617-1252
    ;; Monday, 17 June 1996, 12:52
    (setq dino-timeofday--last-inserted-string (format-time-string tf))
    (insert dino-timeofday--last-inserted-string)
    (setq dino-timeofday--last-inserted-marker (point-marker))))


(defun dino-insert-current-time-millis ()
  "function to insert the value like java's currentTimeMillis."
  (interactive)
  (let ((thing (shell-command-to-string
                "perl -MTime::HiRes -e 'printf(\"%.0f\n\",Time::HiRes::time()*1000)'")))
    (insert (substring thing 0 -1)))) ;; remove newline


(defvar dino-uuidgen-prog
  (if (eq system-type 'windows-nt)
      "c:/users/Dino/bin/uuidgen.exe"
    "/usr/bin/uuidgen")
  "Program to generate one uuid and emit it to stdout.")

(defvar dino-base64-prog
  (if (eq system-type 'windows-nt)
      "c:/dev/dotnet/base64.exe"
    "openssl base64 < ")
  "command to generate base64 encoding for a given file, emit to stdout.")
;; see also `base64-encode-region'

(defun dino-uuid-gen ()
  "function to generate a new UUID and return it."
  (let ((uuid (shell-command-to-string dino-uuidgen-prog)))
    (substring uuid 0 -1))) ;; remove newline

(defun dino-insert-uuid ()
  "function to insert a new UUID at point."
  (interactive)
  (save-excursion
    (let ((beg (point))
          (uuid (dino-uuid-gen)))
      ;; If previous cmd was a kill, this separates the
      ;; kill items:
      (forward-char 1)
      (forward-char -1)
      ;; insert the text
      (insert uuid)
      ;; put the uuid in the kill-ring?:
      (kill-region beg (point))
      (yank)
      (exchange-point-and-mark))))


(defun dino-base64-encode-file (filename)
  "function to get base64 encoding of a given file, and return it."
  (let ((command (concat dino-base64-prog " " filename)))
      (shell-command-to-string command)))


;; c:/sw/VS2010ImageLibrary/Actions/png_format/Office and VS/Animate.png
(defun dino-base64-insert-file (filename)
  "Function to insert the base64 encoding of a given file at point.
Handy for editing .resx files within emacs.
"
  (interactive "*fFile? ")
  (save-excursion
    (let* ((beg (point))
          (fname-quoted (concat "\"" filename "\""))
          (b64
           (replace-regexp-in-string (char-to-string 13) ""
                                     (dino-base64-encode-file fname-quoted))))
      ;; If previous cmd was a kill, this separates the
      ;; kill items:
      (forward-char 1)
      (forward-char -1)
      ;; insert the text
      (insert b64)
      ;; put the uuid in the kill-ring?:
      (kill-region beg (point))
      (yank)
      (exchange-point-and-mark))))

;; http://bit.ly/dino-hangout

(defun dino-csharp-snippet ()
  "convert a file into a snippet, just by narrowing. "
  (interactive)
  (save-excursion
    (let ((beg (point))
          top-of-fn
          bot-of-fn)

      (re-search-backward "{")
      (forward-char 1)
      (set-mark (point))
      (re-search-forward "}")
      (forward-char -1)
      (narrow-to-region (point) (mark)))))


(defun dino-file-contents-as-string (filename)
  "Get the contents of a file as a string. Be careful!"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun dino-time ()
  "returns the time of day as a string.  Used in the `dino-log' function."
  ;;(substring (current-time-string) 11 19) ;24-hr time
  (format-time-string "%H:%M:%S"))


(defun dino-log (label text &rest args)
  "Log a message, using `message'.
LABEL is printed as a prefix.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (let* ((msg (apply 'format text args)))
        (message "%s %s %s" label (dino-time) msg)))


(defun dino-set-alist-entry (alist key value-cdr)
  "like `add-to-list' but works whether the key exists or not.
"
  (let ((target-entry (assoc key alist)))
    (if target-entry
        (setcdr target-entry value-cdr)
      (add-to-list alist (cons key value-cdr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty print xml in region
;; http://stackoverflow.com/a/5198243/48082
(defun dino-xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
    http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
    this. The function inserts linebreaks to separate tags that have
    nothing but whitespace between them. It then indents the markup
    by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))


(defun dino-xml-pretty-print-buffer ()
  "pretty print the XML in a buffer."
  (interactive)
  (dino-xml-pretty-print-region (point-min) (point-max)))

(defvar dino-html-escape-pairs '(("&" "&amp;")
                       ("<" "&lt;")
                       (">" "&gt;"))
  "a list of pairs of strings to swap when escaping and unescaping html")

(defun dino-replace-s-non-interactively (from-string to-string)
  "Non-interactive fn to replace one string with another.
Like `replace-string' but for non-interactive use. "
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun dino-escape-html-in-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapcar (lambda (elt)
                (goto-char (point-min))
                (dino-replace-s-non-interactively (car elt) (cadr elt)))
                dino-html-escape-pairs))))

(defun dino-unescape-html-in-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapcar (lambda (elt)
                (goto-char (point-min))
                (dino-replace-s-non-interactively (cadr elt) (car elt)))
                dino-html-escape-pairs))))

(defun dino-encode-uri-component-in-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((str (buffer-substring-no-properties (point-min) (point-max)))
            (len (- (point-max) (point-min))))
        (goto-char (point-min))
        (delete-char len)
        (insert (shell-command-to-string
                 (concat
                  "/usr/local/bin/node -e \"console.log(encodeURIComponent('"
                  str "'))\"" )))))))


(defun dino-unencode-uri-component-in-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((str (buffer-substring-no-properties (point-min) (point-max)))
            (len (- (point-max) (point-min))))
        (goto-char (point-min))
        (delete-char len)
        (insert (shell-command-to-string
                 (concat
                       "/usr/local/bin/node -e \"console.log(unescape('"
                       str "'))\"" )))))))


(defun dino-sum-column (start end)
  "Adds a column of numbers. Displays the sum and inserts it into
the kill-ring. To use this fn interactively, mark the rectangle, and
invoke the function. No commas or $ in the numbers, please.

Overwrites register 9. "
  (interactive "r")
  (copy-rectangle-to-register 9 start end)
  (set-buffer (get-buffer-create "*calc-sum*"))
  (erase-buffer)
  (insert-register 9)
  (let ((sum 0))
    (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
      (setq sum (+ sum (string-to-number (match-string 0)))))
    (if (fboundp 'paste-to-osx)
          (paste-to-osx (format "%f" sum)))
    (message "Sum: %f" sum)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dino-xml-comment-region (beg end &optional arg)
  (interactive "*r\nP")
  (if (> beg end)
      (let (tmp) (setq tmp beg beg end end tmp)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (cond
       ;; is there a C-u prefix?
       ((and (listp arg) (> (length arg) 0))
        (and (re-search-forward "<!-- *[\n\r]" nil t)
             (goto-char (- (point-max) 1))
             (re-search-backward " *-->" nil t)
             (goto-char (point-min))
             (progn
               (re-search-forward "<!-- *[\n\r]" nil t)
               (replace-match "")
               (goto-char (- (point-max) 1))
               (re-search-backward "[\n\r] *-->" nil t)
               (replace-match ""))))

       (t
        (insert "<!--\n")
        (goto-char (- (point-max) 1))
        (unless (= 10 (following-char))
          (forward-char))
        (insert "\n-->"))))))


(defun dino-is-directory (dir-name)
  "Tests to see whether a name refers to a directory.
Why don't i just use `file-directory-p' ?"
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))


(defvar dino-move-timer-list nil
  "list of timers for `dino-move-whenever'")


(defun dino-get-unique-filename (fname dir)
  "returns a unique filename for FNAME in diretory DIR,
by appending -N, N=1,2,3... to FNAME when FNAME exists
in DIR.
"
  (let ((testname
         (concat (file-name-as-directory dir) fname)))
    (if (file-exists-p testname)
      (let ((parts (split-string fname "\\."))
            (continue t))
        (let ((basename
               (mapconcat 'identity (reverse (cdr (reverse parts))) "."))
              (extension
               (if (> (length parts) 1)
                   (car (reverse parts))
               ""))
              (ix 1))
        (while continue
          (setq testname
                (concat (file-name-as-directory dir) basename
                        "-" (format "%d" ix) "." extension))
          (setq continue (file-exists-p testname)
                ix (1+ ix))))))
    testname))



(defun dino-check-files-and-move (src targ)
  "Checks SRC directory and moves any files there to
TARG directory. Fixes up names for uniqueness. Returns
the list of files moved, nil if none.

Eg,
  (dino-check-files-and-move \"~/dev/axp/to-Apigee\" \"~/Desktop\")

"
  (if (not (dino-is-directory src))
      (error (format "%s is not a directory" src)))
  (if (not (dino-is-directory targ))
      (error (format "%s is not a directory" targ)))
  (let ((flist (directory-files-and-attributes src))
        moved)
    (while flist
      (let ((one-file (car flist)))
        (if (not (car (cdr one-file))) ;; regular file
            (let ((unique-name
                   (dino-get-unique-filename (car one-file) targ)))
              (rename-file
               (concat (file-name-as-directory src)
                       (car one-file))
               unique-name)
              (add-to-list 'moved unique-name))))
      (setq flist (cdr flist)))
    moved))



(defun dino-move-whenever (src targ)
  "Watches SRC directory and when files are present, moves them to
TARG directory.

Eg,

 (dino-move-whenever \"~/dev/axp/to-Apigee\"
                     \"/Users/dino/Google Drive/Pre-Sales/Accounts/AEXP\")

"
  (if (not (dino-is-directory src))
      (error (format "%s is not a directory" src)))
  (if (not (dino-is-directory targ))
      (error (format "%s is not a directory" targ)))
  (let ((x (run-with-timer 0 75 'dino-check-files-and-move src targ)))
    (add-to-list 'dino-move-timer-list (list src targ x))))




(provide 'dino-utility)

;;; dino-utility.el ends here

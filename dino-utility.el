;;; dino-utility.el --- utility functions for dino
;;
;; Author: Dino Chiesa
;; Created: Wed, 17 Jul 2013  12:06
;; Package-Requires: (package)
;; URL:
;; X-URL:
;; Version: 2016.04.28
;; Keywords: utility
;; License: New BSD

;;; Commentary:

;; -none-

;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2013-2020, Dino Chiesa
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

;;(require 'cl) ; for incf
(require 's)
(require 'package)

(defconst dino--short-filename-regex "[A-Za-z]\\S+\\.[-A-Za-z0-9_]+"
  "regexp for a filename")

(defconst dino--short-filename-no-extension-regex "[A-Za-z][-A-Za-z0-9_-]+"
  "regexp for a filename without extension")

(defconst dino--filename-edge-regex "[ \\!\?\"\.'#$%&*+/;<=>@^`|~]"
  "regexp for filename start")

;; ;; when copying binary files into a clipboard buffer
;; (fset 'dinoch-b64-copy
;;       [escape ?  escape ?> escape ?x ?b ?a ?s ?e ?6 ?4 ?- ?e ?n ?c tab return ?\C-w ?\C-y])
;;
;; ;; when pasting the base64 stuff from binary files
;; (fset 'dinoch-b64-paste
;;       [escape ?x ?r backspace ?e ?r ?a ?s ?e ?- ?b ?u tab return ?\C-y escape ?x ?b ?a ?s ?e ?6 ?4 ?- ?d ?e ?c ?o tab return ?\C-x ?\C-s])

(defun dino-fixup-linefeeds ()
  "Dino's function to replace the CR-LF of a DOS ASCII file to a LF for Unix."
  (interactive)
  (save-excursion
    (while (search-forward "\xd" nil t)
      (replace-match "" nil t))))

(defun dino-sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


(defun dino-ensure-package-installed (&rest packages)
  "For each package in the list of PACKAGES, check if it is installed. If not,
ask for installation. Return the list of packages."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages))


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


(defun dino-insert-filename (&optional arg)
  "inserts the name of the file behind the buffer, at point.
When invoked with a prefix, doesn't insert the file, but sets the
filename including the full directory path into the kill ring."
  (interactive "P")
  (let ((fname
         (if arg
             (buffer-file-name)
           (file-name-nondirectory (buffer-file-name)))))
    (kill-new fname) ;; insert into kill-ring
    (if (and (not arg)
             (not buffer-read-only))
        (insert fname)
      (message fname)
      )))

(defun dino--maybe-delete-file-name-looking-forward (no-extension)
  "if point is on a filename, delete it."
  (let ((found
         (if no-extension
             (looking-at dino--short-filename-no-extension-regex)
             (looking-at dino--short-filename-regex) )))
    (if found
        (progn
          (setq found (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
          (delete-region (match-beginning 0) (match-end 0))))
    found))


(defun dino-replace-filename (&optional arg)
  "inserts the name of the file behind the buffer, at point, replacing
any other filename at point. Optional prefix says to replace base filename
(no extension). See also `dino-replace-filename-no-extension'"
  (interactive "P")
  (let ((no-extension arg))
    (let* ((full-fname
            (file-name-nondirectory (buffer-file-name)))
           (fname
            (if no-extension
                (file-name-sans-extension full-fname)
              full-fname)))
      (kill-new fname) ;; insert into kill-ring

      ;; maybe delete an existing filename
      (and (re-search-backward dino--filename-edge-regex (line-beginning-position) t)
           (progn (forward-char)
                  (dino--maybe-delete-file-name-looking-forward no-extension)))
      ;; definitely insert the new filename
      (insert fname))))

(defun dino-replace-filename-no-extension ()
  "inserts the base name of the file behind the buffer, at point,
replacing any other base filename at point."
  (interactive)
  (dino-replace-filename t))

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

(defvar dino-no-untabify-modes '(makefile-mode BSDmakefile salted-file-mode)
  "Normally my setup untabifies buffers before save. This list
provides a set of modes for which no untabify is desired.")

(setq-default indent-tabs-mode nil) ;Use spaces not tabs!

;;(setq dino-no-untabify-modes '(makefile-mode BSDmakefile))


(defun dino-untabify-maybe ()
  "Untabify the current buffer, if the major-mode of the buffer is not
in the list `dino-no-untabify-modes'
"
  (interactive)
  (when (and (not indent-tabs-mode)
             (or (not dino-no-untabify-modes)
                 (every '(lambda (m) (not (derived-mode-p m)))
                        dino-no-untabify-modes)))

    (untabify 0 (point-max))))

(defun dino-untabify-unconditionally ()
  "Untabify the current buffer completely and unconditionally."
  (interactive)
  (untabify (point-min) (point-max)))


;; ;; put an href around the url at point.
;; (fset 'dino-href-url
;;       [?< ?  backspace ?a ?  ?h ?r ?e ?f ?= ?\" ?\C-s ?  ?\C-b ?\" ?> ?\C-r ?/ ?\C-f escape ?  ?\C-s ?\" ?\C-b ?\C-w ?\C-y ?\C-f ?\C-f ?\C-y ?< ?/ ?a ?> ?< ?b ?r ?> return])


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
  "toggle the state of the truncate-lines variable"
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

(defvar dino-timeofday--just-deleted-string nil
  "holder of the string most recently deleted by `dino-maybe-delete-time-string-looking-forward'. Used
to determine if we need to rotate through various formats.")

;; (defvar dino-timeofday--last-inserted-marker nil
;;   "marker companion to the above.")
;;
;; (defvar dino-timeofday--last-inserted-index -1
;;   "index of the time format last inserted. Used for rotation.")
;;
;; (defvar dino-timeofday--prior-insert-time nil
;;   "the time of the most recent insert")
;;

(defconst dino-time-punctuation-regex "[\\!\?\"\.'#$%&*+/;<=>@^`|~]"
  "regexp for punctuation")

(defconst dino-monthnames-and-numbers '(("jan" . 1) ("feb" . 2) ("mar" . 3)
                                        ("apr" . 4) ("may" . 5) ("jun" . 6)
                                        ("jul" . 7) ("aug" . 8) ("sep" . 9)
                                        ("oct" . 10) ("nov" . 11) ("dec" . 12)
                                        ("january" . 1) ("february" . 2)
                                        ("march" . 3) ("april" . 4) ("june" . 6)
                                        ("july" . 7) ("august" . 8)
                                        ("september" . 9) ("october" . 10)
                                        ("november" . 11) ("december" . 12)))
(defconst dino-time-formats '(
                             ("%Y%m%d-%H%M"         . dino-parse-YYYYMMDDHHMM-time)
                             ("%A, %e %B %Y, %H:%M" . dino-parse-rfc822-time)
                             ("%Y %B %e"            . dino-parse-YBe-time)
                             ("%H:%M:%S"            . dino-parse-HMS-time)
                             ("%Y-%m-%dT%H:%M:%S"   . dino-parse-YmdHMS-time)
                             )
  "A list of time formats with corresponding parse functions to use in `dino-insert-timeofday' and `dino-maybe-delete-time-string-looking-forward' ")

;; (setq dino-time-formats '(
;;                              ("%Y%m%d-%H%M" . dino-parse-YYYYMMDDHHMM-time)
;;                              ("%A, %e %B %Y, %H:%M" . dino-parse-rfc822-time)
;;                              ("%Y %B %e" . dino-parse-ymd-time)
;;                              ("%H:%M:%S" . dino-parse-hms-time)
;;                              ))

(defun dino-parse-YYYYMMDDHHMM-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
in format YYYYMMDD-HHMM. Example: \"20130820-0848\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)
        (let ((year (string-to-number (match-string 1 arg)))
              (month (string-to-number (match-string 3 arg)))
              (day (string-to-number (match-string 4 arg)))
              (hour (string-to-number (match-string 5 arg)))
              (minute (string-to-number (match-string 6 arg)))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dino-monthname-to-number (monthname)
  "Maps a monthname to a number, starting with 1 for January.
For invalid monthnames, returns nil."
  (cdr (assoc-string (downcase monthname) dino-monthnames-and-numbers)))

(defun dino-parse-rfc822-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"Tuesday, 21 November 2017, 12:42\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex
         "\\(Sunday\\|Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\), +\\([0-9]\\{1,2\\}\\) \\([A-Za-z]\\{3,14\\}\\) \\(\\(19\\|20\\)[0-9]\\{2\\}\\), \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((day (string-to-number (match-string 2 arg)))
              (month (dino-monthname-to-number (match-string 3 arg)))
              (year (string-to-number (match-string 4 arg)))
              (hour (string-to-number (match-string 6 arg)))
              (minute (string-to-number (match-string 7 arg)))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dino-parse-YBe-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2017 November 21\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\) \\([A-Za-z]\\{3,14\\}\\) +\\([0-9]\\{1,2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((year (string-to-number (match-string 1 arg)))
              (month (dino-monthname-to-number (match-string 3 arg)))
              (day (string-to-number (match-string 4 arg)))
              (hour (nth 2 dt))
              (minute (nth 1 dt))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dino-parse-HMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"14:32:33\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" )
        (dt (decode-time (current-time))))

    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((hour (string-to-number (match-string 1 arg)))
              (minute (string-to-number (match-string 2 arg)))
              (seconds (string-to-number (match-string 3 arg)))
              (year (nth 5 dt))
              (month (nth 4 dt))
              (day (nth 3 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dino-parse-YmdHMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2019-02-12T14:32:33\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" )
        (dt (decode-time (current-time))))

    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)
        (let ((year (string-to-number (match-string 1 arg)))
              (month (string-to-number (match-string 2 arg)))
              (day (string-to-number (match-string 3 arg)))
              (hour (string-to-number (match-string 4 arg)))
              (minute (string-to-number (match-string 5 arg)))
              (seconds (string-to-number (match-string 6 arg)))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))



(defun dino-maybe-delete-time-string-looking-forward ()
  "if point is looking forward at a time string, delete it.
Return a number (index of the time-format string found) if an
appropriate string has been found and deleted. Else return nil."
  (interactive)
  (let ((ix 0)
        found)
    (setq dino-timeofday--just-deleted-string nil)
    (while (and (< ix (length dino-time-formats))
                (not found))
      (let ((tf (nth ix dino-time-formats)))
        (if (looking-at (funcall (cdr tf) t))
            (progn
              (setq found ix
                    dino-timeofday--just-deleted-string (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
              (delete-region (match-beginning 0) (match-end 0)))
          (setq ix (1+ ix)))))
    found))

(defun dino-maybe-delete-time-string-under-point ()
  "if point is on a time string, delete it.
Return a number (index of the time-format string found) if an
appropriate string has been found and deleted. Else return nil."
  (interactive)
  (save-excursion
    (save-match-data
      (or
       ;; 1. try current position.
       (dino-maybe-delete-time-string-looking-forward)
       ;; 2. try searching back to punctuation.
       (and (re-search-backward dino-time-punctuation-regex (line-beginning-position) t)
            (progn (forward-char)
                   (dino-maybe-delete-time-string-looking-forward)))
       ;; 3. try searching back to whitespace
       (and (re-search-backward "[[:space:]]" (line-beginning-position) t)
            (progn (forward-char)
                   (dino-maybe-delete-time-string-looking-forward)))
       ;; 4. move back to BOL, try there.
       (progn
         (beginning-of-line)
         (dino-maybe-delete-time-string-looking-forward))))))

(defun dino-time-is-within-seconds (the-time delta-seconds)
  "Returns t if THE-TIME is less than DELTA-SECONDS ago."
  (< (float-time (time-subtract (current-time) the-time)) delta-seconds))



(defun dino-insert-timeofday (&optional arg)
  "Inserts a string representing the time of day at point.
If the mode is txt-mode, then the format used is like this:
  Tuesday, 20 August 2013, 08:48

Otherwise, the format used is like this:
  20130820-0848

If you invoke the command while point is on a timestamp string, it
will insert an updated stamp using the same format.

If you invoke this command repeatedly, it cycles through additional formats:

   Tuesday, 20 August 2013, 08:48
   2013 August 20
   08:48:03
   2019-02-12T08:48:03Z

Point is placed at the beginning of the newly inserted timestamp.
"
  (interactive "P")

  (cond
   ((or (window-minibuffer-p) (equal major-mode 'wdired-mode) arg)
    (let ((time-format (nth 0 dino-time-formats)))
      (save-excursion
        (insert (format-time-string (car time-format))))))
   (t
    (let ((ix (dino-maybe-delete-time-string-under-point)))
      (if (numberp ix)
          (let ((previous-time (funcall (cdr (nth ix dino-time-formats)) dino-timeofday--just-deleted-string)))
            (if (and previous-time
                     (not (dino-time-is-within-seconds previous-time 10))) ;; not recent
                (setq ix (1- ix))))) ;; keep same format

      (setq ix (if (numberp ix) (1+ ix)  ;; increment
                 (if (equal major-mode 'text-mode) 1 0))) ;; start at reasonable defaults

      (if (>= ix (length dino-time-formats)) ;; roll-over
          (setq ix 0))

      (let ((orig-point (point)))
        (insert (format-time-string (car (nth ix dino-time-formats))))
        (push-mark)
        (goto-char orig-point))))))


(defun dino-insert-current-time-millis ()
  "function to insert the value like java's currentTimeMillis."
  (interactive)
  (let ((thing (shell-command-to-string
                "perl -MTime::HiRes -e 'printf(\"%.0f\n\",Time::HiRes::time()*1000)'")))
    (insert (substring thing 0 -1)))) ;; remove newline


(defvar dino-uuidgen-prog
  (if (eq system-type 'windows-nt)
      "c:/users/dpchi/bin/uuidgen.exe"
    "/usr/bin/uuidgen")
  "Program to generate one uuid and emit it to stdout.")

(defvar dino-base64-prog
  (if (eq system-type 'windows-nt)
      "c:/dev/dotnet/base64.exe"
    "openssl base64 < ")
  "command to generate base64 encoding for a given file, emit to stdout.")
;; see also `base64-encode-region'

(defun dino-get-apigee-edge-cached-token (cache-file username &optional mgmt-endpt login-endpt)
  "parse the json CACHE-FILE and extract the token"
  (let ((cache-file (expand-file-name cache-file)))
    (and
     (file-exists-p cache-file)
     (let* ((mgmt-endpt (or mgmt-endpt "https://api.enterprise.apigee.com"))
            (login-endpt (or login-endpt "https://login.apigee.com"))
            (jsonkey (concat username "##" mgmt-endpt "##" login-endpt )))
       (let ((json (json-read-file cache-file)))
         (cdr
          (assoc 'access_token
                 (cdr (assoc (intern jsonkey) json))))
         ))
     )))

(defun dino-fetch-latest (path &optional match)
  "return the latest file in directory PATH, optionally the latest
file with filename matching regex MATCH. The return value is a concatenated
filename."
  (let ((entries (directory-files-and-attributes path nil match t))
        (dc-mtime (lambda (entry) (nth 5 (cdr entry)))))
    (concat
     (file-name-as-directory path)
     (car
      (car (sort entries (lambda (a b)
                           (not (time-less-p (funcall dc-mtime a)
                                             (funcall dc-mtime b))))))))))

(defun dino-googleapis-project-id (json-keyfile)
  "parse the json keyfile and extract the project id"
  (and
     (file-exists-p json-keyfile)
     (cdr (assoc 'project_id (json-read-file json-keyfile)))))

(defun dino-gcloud-auth-print-access-token ()
  "return output of $(gcloud auth print-access-token)"
  (let* ((gcloud-pgm "~/Downloads/google-cloud-sdk/bin/gcloud")
         (command-string (concat gcloud-pgm " auth print-access-token"))
         (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
         (lines (split-string output "\n")))
       (car (last lines))))


(defun dino-googleapis-token-for-sa (json-keyfile)
  "generate and return a new OAuth token for googleapis.com for a service account"
  ;; There is some problem with emacs and TLS v1.3
  ;; So we try turning it off.
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (let ((project-id (dino-googleapis-project-id json-keyfile)))
  (and
   project-id
     (let* ((command-string
             (format "node ~/dev/apigee-edge-js-examples/getToken.js -J %s -o %s -v" json-keyfile project-id))
            (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
            (lines (split-string output "\n")))
       (car (last lines))))))

(defun dino-uuid-gen ()
  "function to generate a new UUID and return it."
  (let ((uuid (shell-command-to-string dino-uuidgen-prog)))
    (substring uuid 0 -1))) ;; remove newline

(defun dino-insert-uuid ()
  "function to insert a new UUID at point."
  (interactive)
  (let ((uuid (dino-uuid-gen)))
    (kill-new uuid) ;; forcibly insert into kill-ring
    (insert uuid)))

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
      (backward-char 2) (insert "\n") (setq end (+ end 1)))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (setq end (+ end 1)))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n")
      (setq end (+ end 1)))
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

(defvar dino-xml-escape-pairs '(("&" "&amp;")
                            ("<" "&lt;")
                             (">" "&gt;")
                             ("'" "&apos;")
                             ("\"" "&quot;"))
    "a list of pairs of strings to swap when escaping and unescaping xml")

(defun dino-unescape-xml-in-region (start end)
  "replaces XML entities with their referents."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapcar (lambda (elt)
                (goto-char (point-min))
                (dino-replace-s-non-interactively (cadr elt) (car elt)))
                dino-xml-escape-pairs))))


(defun dino-urlencode-region (start end)
  "calls the Javascript function encodeURIComponent() (via nodejs) on the string in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((str (buffer-substring-no-properties (point-min) (point-max)))
            (len (- (point-max) (point-min))))
        (goto-char (point-min))
        (delete-char len)
        (insert
         (replace-regexp-in-string
          "~~" "%27"
          (shell-command-to-string
           (concat
            "node -e \"console.log(encodeURIComponent('"
            (replace-regexp-in-string
             "'" "~~"
             (replace-regexp-in-string "\n" "" str))
            "'))\"" ))))))))



(defun dino-urldecode-region (start end)
  "calls the Javascript function unescape() (via nodejs) on the string in region."
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

       ((and (listp arg) (> (length arg) 0)        ;; is there a C-u prefix?
             (re-search-forward (concat "<!-- *"   ;; and a multiline commented-block?
                                        "\\(.*\n\\)+"
                                        " *-->") nil t))
        (progn
          (goto-char (point-min))
          (re-search-forward "<!-- *" nil t)
          (replace-match "")
          (goto-char (- (point-max) 1))
          (re-search-backward " *-->" nil t)
          (replace-match "")
          ;; naively un-replace existing comment brackets
          (goto-char (point-min))
          (while (re-search-forward "< ! - - " nil t)
            (replace-match "<!-- "))
          (goto-char (point-min))
          (while (re-search-forward " - - >" nil t)
            (replace-match " -->"))
          ))

       (t
        ;; naively replace existing comment brackets
        (goto-char (point-min))
        (while (re-search-forward "<!-- " nil t)
          (replace-match "< ! - - "))
        (goto-char (point-min))
        (while (re-search-forward " -->" nil t)
          (replace-match " - - >"))
        (goto-char (point-min))
        (insert "<!--\n")
        (goto-char (- (point-max) 1))
        (unless (= 10 (following-char))
          (forward-char))
        (insert "\n-->"))))))

(defun dino-maybe-xml-comment-region (old-function &rest arguments)
  "invoke `dino-xml-comment-region' when in an xml mode."
  (apply
   (if (or (eq major-mode 'nxml-mode))
      'dino-xml-comment-region
     old-function) arguments))

(advice-add #'comment-region :around #'dino-maybe-xml-comment-region)
;;(advice-remove #'gh-api-authenticated-request  #'dino-add-user-agent)



(defun dino-filter-list (condp lst)
  "A filter. Emacs Lisp doesn't come with a filter function to keep
elements that satisfy a conditional and excise the elements that
do not satisfy it. One can use mapcar to iterate over a list with
a conditional, and then use delq to remove the nil
values.

Usage:

 (let ((num-list '(1 'a 2 \"nil\" 3 nil 4)))
    (dino-filter-list 'numberp num-list))   ;; ==> (1 2 3 4)
"
     (delq nil
           (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun dino-add-load-path-for-package (pkg)
  "sets the load-path to load the named package.
When downloading something from ELPA, the directory that contains
the .el files is something like \"~/.emacs.d/elpa/org-20140414/\".

But when that package gets updated the directory changes. This
function finds the latest directory for the named package.

FYI: this is unnecessary if there is only one version
of each package in the ~/.emacs.d/elpa directory. In that case, you can
just do

  (let ((default-directory \"~/.emacs.d/elpa\"))
    (normal-top-level-add-subdirs-to-load-path))


Also FYI: `string-prefix-p' is  built-in, but is not present in the
version of emacs that is installed by default on MacOS.

"
  (let ((pkg-dir "~/.emacs.d/elpa/")
        ;; in lieu of flet
        (pkg-match (lambda (dir) (string-prefix-p (concat pkg "-") dir t))))
    (let ((dirlist (dino-filter-list pkg-match (directory-files pkg-dir))))
      (setq dirlist (nreverse (sort dirlist 'string<)))
      (add-to-list 'load-path
                   (concat pkg-dir (car dirlist))) ;; "~/.emacs.d/elpa/org-20140414/"
      )))



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


;; (defun dino-gtm-url (mtgid)
;;   "Produce and put in killring a URL for a GoToMeeting"
;;   (interactive "smtg id: ")
;;   (if mtgid ;; eg 979-984-805 or 979984805
;;       (let ((mtgid (replace-regexp-in-string "-" "" mtgid)))
;;         (if (eq (length mtgid) 9)  ;; eg, 979984805
;;             (let ((link (concat "https://global.gotomeeting.com/join/" mtgid)))
;;               (kill-new link)
;;               (message link))
;;           (message "invalid meeting ID")))
;;     (message "empty meeting ID")))


  (defun dino-copy-value-from-key-into-killring (key)
    "Extract a value from the secure keystore into the killring, using the given KEY to do the lookup."
    ;; eg, (dino-copy-value-from-key-into-killring "box-personal")
    (interactive "skey: ")
  (let ((buf (get-buffer "pwds.txt.gpg"))
        (regexp (concat "^[ \t]*\\b" key "\\b.+ \\([^ \r\n]+\\)$")))
    (if buf
        (let ((result
               (with-current-buffer buf
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (if (re-search-forward regexp nil t)
                         (or (match-string 1) 1) ;; 1 == regex error
                       2 ;; 2 == not found
                       ))))))
          (cond
           ((numberp result)
            (message "not found")
            (kill-new "--"))
           (t
            (kill-new result)
            (message "found"))))
      (kill-new "xx")
      (message "no file"))))

(defun dino-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(eval-after-load "nxml-mode"
  '(progn
     (defun nxml-where ()
       "Display the hierarchy of XML elements the point is on, as a path."
       (interactive)
       (let ((path nil))
         (save-excursion
           (save-restriction
             (widen)
             (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                         (condition-case nil
                             (progn
                               (nxml-backward-up-element) ; always returns nil
                               t)
                           (error nil)))
               (setq path (cons (xmltok-start-tag-local-name) path)))
             (let ((xpath (concat "/" (mapconcat 'identity path "/"))))
               (if (called-interactively-p t)
                   (progn
                     (kill-new xpath)
                     (message "%s" xpath))
                 xpath))))))))

;; to fake self-insert
(defun insert-as-self (CHAR N)
  (let ((last-command-event CHAR)
        (repeat (if N N 1)))
    (self-insert-command repeat)))


(defun dino-find-file-with-line-number (original-fn &rest args)
  "Advice for `find-file'. When applied, a filename like file.js:14:10 results in opening file.js
    and moving to line 14, col 10."
  (save-match-data
    (let* ((path (nth 0 args))
           (match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path))
           (line-no (and match
                         (match-string 2 path)
                         (string-to-number (match-string 2 path))))
           (col-no (and match
                        (match-string 3 path)
                        (string-to-number (match-string 3 path))))
           (new-path (and match (match-string 1 path))))
      (if line-no
          (progn
            (pop args)
            (push new-path args)))

      (let ((result (apply original-fn args)))

        (when line-no
          ;; goto-line is for interactive use
          (goto-char (point-min))
          (forward-line (1- line-no))
          (when (> col-no 0)
            (forward-char (1- col-no))))

        result))))


(advice-add 'find-file :around #'dino-find-file-with-line-number)

(defun dino-insert-paired-quotes (arg)
  "Skip the char if it is a typeover for the ending quote, otherwise
insert a pair, and backup one character."
  (interactive "*p")
  (let ((char last-command-event))
    (if (and (or (eq char ?\") (eq char ?\'))
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg))
      (self-insert-command (prefix-numeric-value arg))
      (forward-char -1)
      )))
                                        ;    (?} . ?{)
(defvar dino-skeleton-pair-alist
  '((?\) . ?\()
    (?\> . ?\<)
    (?\] . ?\[)
    (?\" . ?\")))

(defun dino-skeleton-pair-end (arg)
  "Skip the char if it is an ending, otherwise insert it."
  (interactive "*p")
  (let ((char last-command-event))
    (if (and (assq char dino-skeleton-pair-alist)
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg)))))


(eval-and-compile

;; [4] NameStartChar
;; See the definition of word syntax in `xml-syntax-table'.
(defconst xml-name-start-char-re "[[:word:]:_]")

;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
;;                 | [#x0300-#x036F] | [#x203F-#x2040]
(defconst xml-name-char-re "[[:word:]:_.0-9\u00B7\u0300-\u036F\u203F\u2040-]")

;; [5] Name     ::= NameStartChar (NameChar)*
(defconst xml-name-re (concat xml-name-start-char-re xml-name-char-re "*"))
)

;;; Thursday, 12 October 2017, 13:18
;;; replace xml-parse-tag-1 to not skip comments.
;;; This modified version of the fn returns a cons of (nil . "<!-- string -->")
;;; for a comment.
(defun xml-parse-tag-1 (&optional parse-dtd parse-ns)
  "Like `xml-parse-tag', but possibly modify the buffer while working."
  (let* ((xml-validating-parser (or parse-dtd xml-validating-parser))
         (xml-ns
          (cond ((eq parse-ns 'symbol-qnames)
                 (cons 'symbol-qnames xml-default-ns))
                ((or (consp (car-safe parse-ns))
                     (and (eq (car-safe parse-ns) 'symbol-qnames)
                          (listp (cdr parse-ns))))
                 parse-ns)
                (parse-ns
                 xml-default-ns))))
    (cond
     ;; Processing instructions, like <?xml version="1.0"?>.
     ((looking-at-p "<\\?")
      (search-forward "?>")
      (skip-syntax-forward " ")
      (xml-parse-tag-1 parse-dtd xml-ns))
     ;; Character data (CDATA) sections, in which no tag should be interpreted
     ((looking-at "<!\\[CDATA\\[")
      (let ((pos (match-end 0)))
        (unless (search-forward "]]>" nil t)
          (error "XML: (Not Well Formed) CDATA section does not end anywhere in the document"))
        (concat
         (buffer-substring-no-properties pos (match-beginning 0))
         (xml-parse-string))))
     ;; DTD for the document
     ((looking-at-p "<!DOCTYPE[ \t\n\r]")
      (let ((dtd (xml-parse-dtd parse-ns)))
        (skip-syntax-forward " ")
        (if xml-validating-parser
            (cons dtd (xml-parse-tag-1 nil xml-ns))
          (xml-parse-tag-1 nil xml-ns))))
     ;; comment
     ((looking-at-p "<!--")
      (let ((start (point))
            comment)
        (search-forward "-->")
        (setq comment (buffer-substring-no-properties start (point)))
      ;; FIXME: This loses the skipped-over spaces.
        (skip-syntax-forward " ")
        (cons nil comment)))

     ;; end tag
     ((looking-at-p "</")
      '())
     ;; opening tag
     ((looking-at (eval-when-compile (concat "<\\(" xml-name-re "\\)")))
      (goto-char (match-end 1))
      ;; Parse this node
      (let* ((node-name (match-string-no-properties 1))
             ;; Parse the attribute list.
             (attrs (xml-parse-attlist xml-ns))
             children)
        ;; add the xmlns:* attrs to our cache
        (when (consp xml-ns)
          (dolist (attr attrs)
            (when (and (consp (car attr))
                       (equal "http://www.w3.org/2000/xmlns/"
                              (caar attr)))
              (push (cons (cdar attr) (cdr attr))
                    (if (symbolp (car xml-ns))
                        (cdr xml-ns)
                      xml-ns)))))
        (setq children (list attrs (xml-maybe-do-ns node-name "" xml-ns)))
        (cond
         ;; is this an empty element ?
         ((looking-at-p "/>")
          (forward-char 2)
          (nreverse children))
         ;; is this a valid start tag ?
         ((eq (char-after) ?>)
          (forward-char 1)
          ;; Now check that we have the right end-tag.
          (let ((end (concat "</" node-name "\\s-*>")))
            (while (not (looking-at end))
              (cond
               ((eobp)
                (error "XML: (Not Well-Formed) End of document while reading element `%s'"
                       node-name))
               ((looking-at-p "</")
                (forward-char 2)
                (error "XML: (Not Well-Formed) Invalid end tag `%s' (expecting `%s')"
                       (let ((pos (point)))
                         (buffer-substring pos (if (re-search-forward "\\s-*>" nil t)
                                                   (match-beginning 0)
                                                 (point-max))))
                       node-name))
               ;; Read a sub-element and push it onto CHILDREN.
               ((= (char-after) ?<)
                (let ((tag (xml-parse-tag-1 nil xml-ns)))
                  (when tag
                    (push tag children))))
               ;; Read some character data.
               (t
                (let ((expansion (xml-parse-string)))
                  (push (if (stringp (car children))
                            ;; If two strings were separated by a
                            ;; comment, concat them.
                            (concat (pop children) expansion)
                          expansion)
                        children)))))
            ;; Move point past the end-tag.
            (goto-char (match-end 0))
            (nreverse children)))
         ;; Otherwise this was an invalid start tag (expected ">" not found.)
         (t
          (error "XML: (Well-Formed) Couldn't parse tag: %s"
                 (buffer-substring-no-properties (- (point) 10) (+ (point) 1)))))))

     ;; (Not one of PI, CDATA, Comment, End tag, or Start tag)
     (t
      (unless xml-sub-parser   ; Usually, we error out.
        (error "XML: (Well-Formed) Invalid character"))
      ;; However, if we're parsing incrementally, then we need to deal
      ;; with stray CDATA.
      (let ((s (xml-parse-string)))
        (when (zerop (length s))
          ;; We haven't consumed any input! We must throw an error in
          ;; order to prevent looping forever.
          (error "XML: (Not Well-Formed) Could not parse: %s"
                 (buffer-substring-no-properties
                  (point) (min (+ (point) 10) (point-max)))))
        s)))))


;;; Thursday, 12 October 2017, 13:18
;;; this allows the xml-print to print out comments correctly
;;;
(defun xml-debug-print-internal (xml indent-string)
  "Outputs the XML tree in the current buffer.
The first line is indented with INDENT-STRING."
  (let ((tree xml)
        attlist)
    (insert indent-string ?< (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\  (symbol-name (caar attlist)) "=\""
              (xml-escape-string (cdar attlist)) ?\")
      (setq attlist (cdr attlist)))

    (setq tree (xml-node-children tree))

    (if (null tree)
        (insert ?/ ?>)
      (insert ?>)

      ;;  output the children
      (dolist (node tree)
        (cond
         ((listp node)
          (cond
           ((not (car node))
            (insert (cdr node)))
           (t
          (insert ?\n)
          (xml-debug-print-internal node (concat indent-string "  ")))))
         ((stringp node)
          (insert (xml-escape-string node)))
         (t
          (error "Invalid XML tree"))))

      (when (not (and (null (cdr tree))
                      (stringp (car tree))))
        (insert ?\n indent-string))
      (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>))))


(defun jshintrc ()
    "create a .jshintrc file in the current working directory if one does not yet exist."
  (interactive)
  (or (file-exists-p ".jshintrc")
      (progn
        (with-temp-file ".jshintrc"
          (insert (concat
"{\n"
"  \"node\": true,\n"
"  \"browser\": true,\n"
"  \"esversion\": 6, \n"
"  \"esnext\": true,\n"
"  \"bitwise\": true,\n"
"  \"camelcase\": true,\n"
"  \"curly\": true,\n"
"  \"eqeqeq\": true,\n"
"  \"immed\": true,\n"
"  \"indent\": 2,\n"
"  \"latedef\": true,\n"
"  \"newcap\": true,\n"
"  \"noarg\": true,\n"
"  \"quotmark\": \"single\",\n"
"  \"regexp\": true,\n"
"  \"undef\": true,\n"
"  \"unused\": true,\n"
"  \"strict\": true,\n"
"  \"trailing\": true,\n"
"  \"smarttabs\": true,\n"
"  \"predef\": [\n"
"  \"Promise\"\n"
"  ]\n"
"}\n"

                   ))))))


(defun dino-buganizer-open ()
  "Convert buganizer ticket to description"
  (interactive)
  (let ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'word))))
    (when bounds
      (let* ((raw-text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (ticket-id (s-chop-prefixes '("b" "b/") raw-text))
             (url (concat "https://b.corp.google.com/issues/" ticket-id)))
        (browse-url url)))))


;; (defun word-as-number-2x ()
;;   "Convert word (As a number) at point to 2x"
;;   (interactive)
;;   (let ((bounds (if (use-region-p)
;;                      (cons (region-beginning) (region-end))
;;                   (bounds-of-thing-at-point 'symbol))))
;;     (when bounds
;;       (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
;;              (numeric (* 2 (string-to-int text))))
;;         (delete-region (car bounds) (cdr bounds))
;;         (insert (int-to-string numeric))))))
;;
;; (defun word-as-number-half ()
;;   "Convert word (As a number) at point to 1/2"
;;   (interactive)
;;   (let ((bounds (if (use-region-p)
;;                      (cons (region-beginning) (region-end))
;;                   (bounds-of-thing-at-point 'symbol))))
;;     (when bounds
;;       (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
;;              (numeric (/ (string-to-int text) 2)))
;;         (delete-region (car bounds) (cdr bounds))
;;         (insert (int-to-string numeric))))))


(provide 'dino-utility)

;;; dino-utility.el ends here

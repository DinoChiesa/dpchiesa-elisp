;;; spellchek.el --- lookup alternative spellings of a word
;;
;; Author: Dino Chiesa
;; Created: Wed, 21 Nov 2012  23:25
;; Package-Requires: ()
;; URL: http://www.emacswiki.org/cgi-bin/wiki/spellchek.el
;; Version: 2012.11.21
;; Keywords: spelchek spelling spell check
;; License: Revised BSD

;;; Commentary:

;; This module allows a user to look up spelling of a word in
;; google's toolbar service.

;;
;; if you want to all out through a proxy, then
;;   (setq url-proxy-services (list (cons "http" "proxyHost:proxyPort")))

;;
;;; Revisions:
;;
;; 2012.11.21  2012-Nov-21 Dino Chiesa
;;    Initial version
;;

;;; License
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2012, Dino Chiesa
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

(require 'xml)

(defgroup spelchek nil
  "Provides a facility to check spelling of a word."
  :group 'Editing)


(defcustom spelchek-prompt-mechanism 'x-popup-menu
  "The mechanism used to prompt the user for his choice of
spelling. Options: 'x-popup-menu, or 'dropdown-list.  When setting
this, set it to the symbol, not to the string or the actual
function.  Eg

  (setq spelchek-prompt-mechanism 'x-popup-menu)
  (setq spelchek-prompt-mechanism 'dropdown-list)

"
  :type 'symbol
  :options '('x-popup-menu 'dropdown-list)
  :group 'spelchek)

(defvar spelchek--curl-prog "curl")

(defvar spelchek---load-path (or load-file-name "~/spelchek.el")
  "For internal use only. ")

(defvar spelchek-cache-dir (file-name-directory spelchek---load-path))
(defvar spelchek-cache-basefilename ".spelchek.cache")

(defvar spelchek-can-save-cache-p (or
       (and (>= emacs-major-version 23)
            (>= emacs-minor-version 1)
            (null (string-match "23.1.1" (version))))
       (> emacs-major-version 23))
  "Whether it is possible to save the cache")

(defvar spelchek-cache nil)
(defvar spelchek-bounds-of-looked-up-word nil)

(defun spelchek-cache-filename ()
  (concat spelchek-cache-dir spelchek-cache-basefilename))

(defun spelchek-cache-initialize ()
  (make-hash-table :test 'equal))

(defun spelchek-cache-get (key)
  (gethash key spelchek-cache))

(defun spelchek-cache-put (key value)
  (when value
    (puthash key value spelchek-cache)
    ;; saving the cache may get expensive as it gets larger
    (spelchek-cache-save))
  value)


(defun spelchek-hashtable-to-alist (hash)
  "Return an association-list representation of the hashtable HASH."
   (let ((alist nil))
     (maphash
      (lambda (key value)
        (setq alist (cons (cons key value) alist)))
      hash)
     alist))


(defun spelchek-hashtable-from-alist (alist &rest options)
  "Build a hashtable from the values in the association list ALIST."
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     (lambda (kv-pair) (puthash (car kv-pair) (cdr kv-pair) ht))
     alist)
     ht))

(defun spelchek-cache-save ()
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (spelchek-hashtable-to-alist spelchek-cache)))
      (write-region (point-min) (point-max) (spelchek-cache-filename)))))


(defun spelchek-cache-load ()
  (spelchek-hashtable-from-alist
   (with-temp-buffer
     (insert-file-contents (spelchek-cache-filename))
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   :test 'equal))


(defun spelchek-get-buffer-for-word (word)
  "Retrieve a list of synonyms for the given word, from the
web service."
  ;; could insert a chooser function here
  (spelchek-get-buffer-for-word-goog-tb word))

(defun spelchek-msgbox (msg)
  "Display a message in a dialog box."
  (if (spelchek-want-msgbox-via-powershell)
      (spelchek-msgbox-via-powershell msg)
    (message-box msg)))

(defun spelchek-path-of-powershell-exe ()
  "get location of powershell exe."
  (concat
   (or (getenv "windir") "c:\\windows")
   "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))

(defun spelchek-want-msgbox-via-powershell ()
  "Determine if we want to display a message box
using Windows powershell."
  (and (eq system-type 'windows-nt)
       (file-exists-p (spelchek-path-of-powershell-exe))))


(defun spelchek-msgbox-via-powershell (format-string &rest args)
  "Display a message box via powershell and Windows Forms.

The `message-box' fn works poorly on Windows; it does not allow
the encoding of newlines and also the UI generally looks like a
silly toy.

This can be used in place of `message-box' on Windows.

This is used within `spelchek.el' in only one case: to notify
the user that he needs to register for an API key.

"
  (flet ((rris (a1 a2 s) (replace-regexp-in-string a1 a2 s)))
    (let* ((msg (format format-string args))
           (ps-cmd
            ;; This is a command to be passed on the cmd.exe line.
            ;; Newlines encoded as \n or `n do not display properly. This
            ;; code transforms splits the string on newlines, then joins,
            ;; using [char]0x000D as the "glue".  Also - need to perform
            ;; special escaping of single and double quotes.  All this
            ;; because we are passing a script to powershell on the
            ;; command line.
            ;;
            ;; creating a file with script code in it, then passing
            ;; that file to powershell, would avoid the need for
            ;; special escaping.  But that is not feasible, since by
            ;; default powershell prohibits executing scripts. But
            ;; powershell allows running script passed as -Command. So.
            (concat "[void][System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms');"
                    "[Windows.Forms.MessageBox]::Show("
                    (mapconcat '(lambda (elt)
                                  (rris (char-to-string 34)
                                        (char-to-string 39)
                                        (pp-to-string
                                         (rris (char-to-string 34)
                                               "'+[char]0x0022+'"
                                               (rris (char-to-string 39)
                                                     "'+[char]0x0027+'"
                                                     elt)
                                               ))))
                               (split-string msg "\n" nil)
                               "+[char]0x000D+")
                    ",'Message from Emacs',"
                    "[Windows.Forms.MessageBoxButtons]::OK,"
                    "[Windows.Forms.MessageBoxIcon]::Information)"))
           (shell-command
            (format "%s -Command %s"
                    (spelchek-path-of-powershell-exe)
                    (concat "\"& {" ps-cmd "}\""))))
      (shell-command-on-region (point) (point)
                               shell-command
                               nil nil nil))))


(defun spelchek-get-buffer-for-word-goog-tb (word)
  "retrieve a list of synonyms for the given word, from the
Google Toolbar web service."

  (let ((buffername (concat "spelchek-" word ".out")))
    (let ((buf (generate-new-buffer buffername))
          (url "https://www.google.com/tbproxy/spell?lang=en&hl=en")
          (payload (concat
                    "<spellrequest textalreadyclipped='0' ignoredups='0' ignoredigits='1' ignoreallcaps='1'><text>" word "</text></spellrequest>")))

      (if (not spelchek--curl-prog) (setq spelchek--curl-prog "curl"))
      (with-current-buffer buf
        (call-process spelchek--curl-prog nil t t
                      "-s" "-d" payload "-X" "POST" url))
      buf)))



(defun spelchek-cache-reset ()
  "Reset the cache to empty"
  (interactive)
  (setq spelchek-cache (spelchek-cache-initialize)))


(defun spelchek-parse-one-line ()
  "Parse one line in the buffer created by `url-retrieve-synchronously'.
The format of each line is expected to be:

   form|flavor|word

where
   form = {adjective,verb,noun,etc}
   flavor  = {syn,sim,ant,rel}
   word = the actual word

The return value is a list, with those three items in it,
in that order.

"
  (let (start end s parts)
    (setq start  (point)
          end (line-end-position)
          s (buffer-substring-no-properties start end)
          parts (split-string s "|"))
    (delete-region start end)
    (delete-char 1)
    parts))


(defun spelchek-fetch-alternatives (word)
  "fetch alternative spellings for the given word, from a remote source."
  (let ((buf (spelchek-get-buffer-for-word word))
        xlist derp)
    (if buf
        (progn
          (with-current-buffer buf
            ;;(rename-buffer (concat "*spelchek* - " word) t)
              (setq xlist
                    (xml-parse-region (point-min)
                                      (point-max)
                                      (current-buffer)
                                      nil nil)))
          (and (kill-buffer buf)
               (listp xlist)
               (setq derp (nth 2 (nth 2 (nth 0 xlist))))
               (split-string derp))))))


;;;###autoload
(defun spelchek-get-alternatives (word)
  "retrieve alternative spellings for the given word, either from
the cache, or, if there is no cache hit, then from the remote service.
"
  (or (spelchek-cache-get word)
      (spelchek-cache-put word (spelchek-fetch-alternatives word))))


(defun spelchek-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun spelchek--generate-menu (candidates)
  "Generate a menu suitable for use in `x-popup-dialog' from the
list of candidates. Each item in the list of candidates is a
string.

"
  (let ((items (mapcar '(lambda (elt) (cons elt elt)) candidates)))

    ;; this works with x-popup-menu
    (setq items (cons "Ignored pane title" items))
    (list "Replace with..." items)))



(defun spelchek-prompt-user-with-choices (candidates)
  "prompt the user with the available replacement choices.
In this context the list of choices is the list of alternative
spellings.

See `spelchek-prompt-mechanism'.

"
  (cond
   ((not candidates)
    nil)
   ((and (eq spelchek-prompt-mechanism 'dropdown-list)
         (featurep 'dropdown-list))
    (let ((choice-n (dropdown-list (mapcar '(lambda (elt) elt) candidates))))
      (if choice-n
          (nth choice-n candidates)
        (keyboard-quit))))

   (t
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (spelchek-get-menu-position)
                  (spelchek--generate-menu candidates)))))


(defun spelchek-word-at-point ()
  "Uses `bounds-of-thing-at-point', to find and return the word at point.

As a side effect, this fn stores the bounds of the word that is found.
This allows this module to delete the word later, when the user chooses
a replacement word.
"
  (if (get 'word 'thing-at-point)
      (funcall (get 'word 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (progn
            (setq spelchek-bounds-of-looked-up-word bounds)
            (buffer-substring-no-properties (car bounds) (cdr bounds)))))))


;;;###autoload
(defun spelchek-choose-alternative-and-replace (word)
  "The main interactive entry point into the `spelchek.el' capability.
Invoke this interactively, and the fn will prompt the user to
confirm the word to be looked up.  It will then retrieve a list
of alternative spellings for the word, either from the cache or
from a remote service, and prompt the user with a list of
possible replacements.  If the user chooses a replacement, the
original word in the buffer will be removed and the replacement
will be inserted in its place.

"
  (interactive (list (read-string "spelchek word: " (spelchek-word-at-point))))
  (let ((chosen (spelchek-prompt-user-with-choices
                 (spelchek-get-alternatives word))))
    (when chosen
          (goto-char (car spelchek-bounds-of-looked-up-word))
          (delete-region (car spelchek-bounds-of-looked-up-word)
                         (cdr spelchek-bounds-of-looked-up-word))
          (insert chosen))))


(defun spelchek-install ()
  "install `spelchek.el'"
  (setq spelchek-cache
        (if (file-exists-p (spelchek-cache-filename))
            (spelchek-cache-load)
          (spelchek-cache-initialize))))


(eval-when-compile (require 'dropdown-list nil t))

(spelchek-install)

(provide 'spelchek)

;;; spelchek.el ends here

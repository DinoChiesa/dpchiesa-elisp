;;; fly-jshint-npm.el --- Flymake for JS via JSHINT installed from npm
;;
;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : April 2013
;; Modified   : April 2013
;; Version    : 1.0.1
;; Keywords   : JavaScript flymake npm jshint
;; Copyright  : (c) 2013 Dino Chiesa
;; License    : New BSD
;; X-URL      :
;; Last-saved : <2013-April-17 00:37:58>
;;

;;; Commentary:
;;
;;

;;; Usage:
;;
;; (require 'fly-jshint-npm)
;; (setq fly/jshint/npm-jshint-exe "/usr/local/bin/jshint")
;; (add-hook 'js-mode-hook   'my-javascript-mode-fn)
;; (defun my-javascript-mode-fn ()
;;   (flymake-mode 1))
;;


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
;;


(eval-when-compile (require 'flymake))

(defvar fly/jshint/npm-jshint-exe "/usr/local/bin/jshint"
  "Location of the jshint binary installed by npm.")


(defun fly/jshint/npm-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a JavaScript buffer.
This gets called by flymake itself. The output is a list of two elements:
the command to run, and a list of arguments.  The resulting command is like
this:

  /usr/local/bin/jshint file.js

...where file.js is a temporary file location

"
  (list fly/jshint/npm-jshint-exe
        (list
         (expand-file-name source))))


(defun fly/jshint/npm-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

This won't always work; it will fail if the source module
refers to relative paths.
"
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                prefix "-"
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "-"))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


(defun fly/jshint/npm-cleanup ()
     (flymake-simple-cleanup))


(defun fly/jshint/npm-init ()
  "initialize flymake for JavaScript using the jshint npm install."
  (let ((create-temp-f 'fly/jshint/npm-create-temp-intemp)
        (use-relative-base-dir t)
        (use-relative-source t)
        (get-cmdline-f 'fly/jshint/npm-get-cmdline)
        args
        temp-source-file-name)
    (setq temp-source-file-name (flymake-init-create-temp-buffer-copy create-temp-f)
          args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))


(defvar fly/jshint/npm-error-pattern
    "^\\(/[^:\n]+\\.js\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$"
  "The regex pattern for jshint error or warning messages. For use as
an entry in `flymake-err-line-patterns'. ")


(defun fly/jshint/npm-install ()
  "install flymake for jshint."
  (add-to-list
   'flymake-err-line-patterns
   (list fly/jshint/npm-error-pattern 1 2 3 4))

  (let* ((key "\\.js\\'")
         (jsentry (assoc key flymake-allowed-file-name-masks)))
    (if jsentry
        (setcdr jsentry '(fly/jshint/npm-init fly/jshint/npm-cleanup))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'fly/jshint/npm-init 'fly/jshint/npm-cleanup)))))

(eval-after-load "flymake"
  '(progn
     (fly/jshint/npm-install)))


(provide 'fly-jshint-npm)

;;; fly-jshint-npm.el ends here

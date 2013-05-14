;;; apigee.el --- utility functions for working with Apigee platform
;;
;; Copyright (C) 2013 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2013
;; Modified   : May 2013
;; Version    : 1.0
;; Keywords   : apigee
;; Requires   : s.el
;; License    : New BSD
;; X-URL      : https://github.com/dpchiesa/elisp
;; Last-saved : <2013-May-07 09:29:10>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with API PRoxy definition bundles within emacs.
;;
;;; License
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

(require 's) ;; magnars long lost string library

(defgroup apigee nil
  "Utility fns for use with the Apigee platform.")

(defcustom apigee-upload-bundle-pgm "~/dev/apiproxies/pushapi"
  "The script or program that uploads proxies to the Apigee gateway.
Can be a python or bash script or other program. Should be executable.
Specify the full path. Use this in your emacs:

   (require 'apigee)
   (setq apigee-upload-bundle-pgm \"~/dev/apiproxies\"
         apigee-upload-bundle-args \"-v\")

Or you can customize this variable.
"
  :group 'apigee)


(defcustom apigee-upload-bundle-args "-v"
  "The arguments to pass to the script or program that
uploads proxies to the Apigee gateway. Use this in your emacs:

   (require 'apigee)
   (setq apigee-upload-bundle-pgm \"~/dev/apiproxies\"
         apigee-upload-bundle-args \"-v\")

Or you can customize this variable.
"
  :group 'apigee)

(defcustom apigee-temp-dir "/tmp"
  "The temporary directory to use for zip bundles. "
  :type 'string
  :group 'apigee)


;; The command "template" to use when creating the API bundle zip. In
;; this template, the %f is replaced with the name of the zip file to
;; create. This template probably never needs to be customized.
(defvar apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\" ")

;;(setq apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\"")


(defun apigee-path-of-apiproxy ()
  "Returns the path of apiproxy that acts as the parent of
the current file or directory. "
  (interactive)
  (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
        r)
    (while (and elts (not r))
      (if (string= (car elts) "apiproxy")
          (setq r (reverse (cdr elts)))
        (setq elts (cdr elts))))
    (if r
        (mapconcat 'identity r "/"))))



(defun apigee-get-name-for-api-bundle-zip ()
  "Get a timestamped name for the API bundle zip that contains
the file or directory currently being edited.
"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        (let ((api-name (file-name-nondirectory apiproxy-dir))
              (timestamp (format-time-string "%Y%m%d-%H%M%S")))
          (concat api-name "-" timestamp ".zip")))))


(defun apigee-get-create-api-bundle-zip-cmd ()
  "Get the command that creates an API bundle zip. for the file
or directory currently being edited.
"
  (interactive)
  (let ((api-bundle-name (concat apigee-temp-dir "/" (apigee-get-name-for-api-bundle-zip)))
        (txt apigee-create-bundle-zip-command-template))
    (if (and api-bundle-name
             (string-match "^\\(.+ \\)\\(%f\\)\\( .+\\)" txt))
        (concat
         (match-string 1 txt)
         api-bundle-name
         (match-string 3 txt)))))


(defun apigee-create-api-bundle-zip ()
  "Create the API bundle zip that contains the file or directory
currently being edited.
"
  (interactive)
  (let ((cmd (apigee-get-create-api-bundle-zip-cmd))
        (bundle-dir (apigee-path-of-apiproxy))
        buffer)
    (setq buffer (get-buffer-create
                  (concat "*Bundle Create "
                          (file-name-nondirectory bundle-dir) "*")))
    (message "Creating zip via %s" cmd)
    (with-current-buffer buffer
      (setq default-directory (apigee-insure-trailing-slash bundle-dir))
      (goto-char (point-max))
      (insert "\n\n============================================\n")
      (goto-char (point-max))
      (shell-command "pwd" t nil)
      (goto-char (point-max))
      (shell-command (concat "echo " cmd) t nil)
      (goto-char (point-max))
      (shell-command cmd t nil))

    (switch-to-buffer-other-window buffer)))



(defun apigee-insure-trailing-slash (path)
  "Insure the given path ends with a slash.
This is usedful with `default-directory'.  Setting
`default-directory' to a value that does not end with a slash
causes it to use the parent directory.
"
  (if (s-ends-with? "/" path)
      path)
  (concat path "/"))




(defun apigee-upload-bundle-with-pushapi ()
  "Interactive fn that uses the pushapi script to upload the bundle
that contains the file or directory currently being edited.
"
  (interactive)
  (let ((proxy-dir (apigee-path-of-apiproxy)))
    (if proxy-dir
        (if (file-exists-p apigee-upload-bundle-pgm)
            (progn
              (set (make-local-variable 'compile-command)
                   (concat
                    apigee-upload-bundle-pgm " "
                    apigee-upload-bundle-args " "
                    proxy-dir))
              (call-interactively 'compile))))))


(provide 'apigee)

;;; apigee.el ends here

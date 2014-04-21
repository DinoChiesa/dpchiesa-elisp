;;; darwin.el -- my personal customization that is specific to macosx
;;
;; MacOS Specific Configuration
;;

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add this directory to path if it exists
(and (file-directory-p "/usr/local/bin")
     (setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use the mono compiler on MacOSX
(and (boundp 'smart-compile-alist)
     (add-to-list 'smart-compile-alist
             '("\\.cs\\'"         . "gmcs /t:exe /debug+ %f")))



(set-face-font 'tooltip "-outline-Lucida Console-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for dired mode
;;
;; on macosx, the builtin ls does not do the -X option, therefore
;; we use "brew" to install coreutils which gives us gnu ls.
(let ((gls (purecopy "/usr/local/bin/gls")))
  (if (and (eq system-type 'darwin)
           (file-exists-p gls))
      (setq insert-directory-program gls)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle copy/paste intelligently
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  "Handle copy/paste intelligently on osx.
TEXT gets put into the Macosx clipboard.

The PUSH argument is ignored."
  (let* ((process-connection-type nil)
         (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
    (process-send-string proc text)
    (process-send-eof proc)))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow auto decompression when opening binary .plist files,
;; and auto compression when saving, via jka-compr.
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; It is necessary to perform an update!
(jka-compr-update)

;; not really true. This is my own personal darwin customization
(provide 'darwin)
;;; darwin.el ends here

;;; csslint.el --- stuff to run css lint in flymake and compile

;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  10:22
;; Version: 0.1
;;


(defun csslint-flymake-create-temp-intemp (file-name prefix)
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


(defun csslint-flymake-init ()
  "the initialization fn for flymake for CSS"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'csslint-flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (concat (getenv "windir") "\\system32\\cscript.exe")
          (list "c:\\users\\dino\\bin\\csslint-wsh.js"
                "--format=compiler"
                local-file))))

(defvar csslint-error-pattern
  ;;"^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\)) CSSLINT: ?\\(error\\|warning\\) ?: +\\(.+\\)$"
  "^[ \t]*\\([\._A-Za-z0-9][^(\n]+\\.css\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) CSSLINT: ?\\(\\(error\\|warning\\) ?: +\\(.+\\)\\)$"
  "The regex pattern for CSSLint error or warning messages. Follows
the same form as an entry in `flymake-err-line-patterns'. The
value is a STRING, a regex.")


(defun csslint-flymake-install ()
  "install flymake stuff for CSS files."
  (add-to-list
   'flymake-err-line-patterns
   (list csslint-error-pattern 1 2 3 4))

  (let* ((key "\\.css\\'")
         (cssentry (assoc key flymake-allowed-file-name-masks)))
    (if cssentry
        (setcdr cssentry '(csslint-flymake-init))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'csslint-flymake-init)))))


(eval-after-load "compile"
  '(progn
     (if (boundp 'compilation-error-regexp-alist-alist)
      (progn
        (add-to-list
         'compilation-error-regexp-alist-alist
         (list 'csslint-wsh csslint-error-pattern 1 2 3))
        (add-to-list
         'compilation-error-regexp-alist
         'csslint-wsh)))))


(eval-after-load "flymake"
  '(progn
     (csslint-flymake-install)))


(provide 'csslint)
;;; csslint.el ends here

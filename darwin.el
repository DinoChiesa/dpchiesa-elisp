;;; darwin.el -- my personal customization that is specific to macosx
;;
;; MacOS Specific Configuration
;;

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixup path
;; for each directory in the list, if it exists, and is not already
;; present on the path, add it to path.
(let ((path-elts (split-string (getenv "PATH") ":")))
  (dolist (path '("/usr/local/bin"
                  "/Library/Frameworks/Mono.framework/Versions/Current/bin"))
    (and (file-directory-p path)
         (not (member path path-elts))
         (setenv "PATH" (concat (getenv "PATH") ":" path)))))


(require 'csharp-mode)

(defun csharp-set-flycheck-command ()
  "Set the flycheck command, dynamically, as a side effect.

This function is intended for use as a before-syntax-check-hook with
flycheck.  Use it like this:

    (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)

Then, in your csharp file, specify this in the comments at the header.

    // flycheck: gmcs -t:module /debug+ -pkg:dotnet %f

This will cause flycheck to run the given command, replacing the %f with
the source file name."

  (and (eq major-mode 'csharp-mode)
       (let ((cmd-string
              (csharp-get-value-from-comments "flycheck" csharp-cmd-line-limit)))
         (and cmd-string
              (not (eq cmd-string ""))
              (let* ((cmd (split-string cmd-string " "))
                     (ferf (member "%f" cmd)))
                (and ferf (setcar ferf 'source))
                (put 'csharp :flycheck-command cmd))))))



;; (defun flycheck-checker-command (checker)
;;   "Get the command for CHECKER."
;;   (get checker :flycheck-command))


(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker csharp
       "A C# syntax checker for dotnet. By default, it uses the Mono
compiler. If you would like to use a different compiler, see
`csharp-set-flycheck-command'."
       :command ("gmcs" "-target:module" source)
       :error-patterns
       ;; WinFormsHello.cs(17,9): error CS0246: The type or namespace name `derp' could not be found. Are you missing an assembly reference?
       ((error line-start (file-name) "(" line "," column "): error " (message) line-end)
        (warning line-start (file-name) "(" line "," column "): warning " (message) line-end))
       :modes csharp-mode)
     (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)
     (setq flycheck-log-level 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smarter-compile: use the mono compiler on MacOSX
(eval-after-load "smarter-compile"
  '(progn
     (and (boundp 'smart-compile-alist)
          (let ((csharp-entry (assoc "\\.cs\\'" smart-compile-alist)))
            (if csharp-entry
                (setcdr csharp-entry '("gmcs /t:exe /debug+ %f"))
              (add-to-list 'smart-compile-alist
                           '("\\.cs\\'"         . "gmcs /t:exe /debug+ %f")))))))





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

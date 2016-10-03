;;; flymake-jshint.el --- JSHint mode for Emacs
;;
;; Version: 20110901
;;
;; adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript
;;
;; Installation:
;;
;;     (add-to-list 'load-path "~/lib/jshint-mode")
;;     (require 'flymake-jshint)
;;     (add-hook 'javascript-mode-hook
;;         (lambda () (flymake-mode t)))
;;
;; run M-x flymake-mode to turn flymake on and off
;;
;; Note from Dino, Tuesday, 13 September 2016, 12:16
;; I recommend not using flymake and using flycheck instead.
;; It's better designed and lighter-weight and more reliable.
;;

(require 'flymake)

(defcustom jshint-mode-mode "jshint"
  "Can use either jshint or jslint"
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-location (file-name-directory load-file-name)
  "The directory jshint-mode.js may be found in."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-port 3003
  "The port the jshint-mode server runs on."
  :type 'integer
  :group 'flymake-jshint)

(defcustom jshint-mode-host "127.0.0.1"
  "The host the jshint-mode server runs on."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-jshintrc ""
  "Location for the jshintrc file. If not set, looks up directory tree from each JS file."
  :type 'string
  :group 'flymake-jshint)

(setq jshint-process "jshint-mode-server")
(setq jshint-buffer "*jshint-mode*")

(defun jshint-mode-init ()
  "Start the jshint-mode server."
  (interactive)
  (if (eq (process-status jshint-process) 'run)
      'started
    (start-process
     jshint-process
     jshint-buffer
     jshint-mode-node-program
     (expand-file-name (concat jshint-mode-location "/jshint-mode.js"))
     "--host" jshint-mode-host
     "--port" (number-to-string jshint-mode-port))
    (set-process-query-on-exit-flag (get-process jshint-process) nil)
    (message
     (concat "jshint server has started on " jshint-mode-host ":"
             (number-to-string jshint-mode-port)))
    'starting
    ))

(defun jshint-mode-stop ()
  "Stop the jshint-mode server."
  (interactive)
  (delete-process jshint-process))

(defun flyjshint-create-temp-intemp (file-name prefix)
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


(defun flymake-jshint-init ()
  "Called each time a flymake check occurs."
  (if (eq (jshint-mode-init) 'started)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flyjshint-create-temp-intemp)))
        (list "node" (list work-file)))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
              flymake-jshint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
              nil 1 2 3)
            flymake-err-line-patterns))

(provide 'flymake-jshint)

;;; flymake-jshint.el ends here

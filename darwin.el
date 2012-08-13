;;
;; MacOS Specific Configuration
;;

;; Override defaults to use the mac copy and paste

(defun copy-from-osx () (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let* ((process-connection-type nil)
         (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
    (process-send-string proc text)
    (process-send-eof proc)))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

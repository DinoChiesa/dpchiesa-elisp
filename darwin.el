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

;;It is necessary to perform an update!
(jka-compr-update)

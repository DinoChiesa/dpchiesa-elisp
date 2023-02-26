;;
;; Windows-NT Specific Configuration
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powershell

(require 'powershell)
(autoload 'powershell "powershell"
  "Run powershell as a shell within emacs." t)

(autoload 'powershell-mode "powershell-mode"
  "Major mode for editing powershell code." t)

(add-to-list 'auto-mode-alist '( "\\.ps1\\'" . powershell-mode ))


(defun dino-powershell-mode-fn ()
  ;;(setq powershell-indent-width 2)
)

(add-hook 'powershell-mode-hook 'dino-powershell-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Windows only
;; upon kill, check clipboard, and if exists, put it into the kill ring.
(defadvice kill-new (before
                     dino-kill-new-push-xselection-on-kill-ring
                     activate)
  "Before putting new kill onto the kill-ring, add the clipboard/external
selection to the kill ring"
  (let ((have-paste (and interprogram-paste-function
                         (funcall interprogram-paste-function))))
    (when have-paste (push have-paste kill-ring))))



(set-face-font 'tooltip "-outline-Lucida Console-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")



(let ((wget (purecopy "c:\\dev\\dotnet\\wget\\wget.exe")))
  (and (file-exists-p wget)
       (setq httpget--wget-prog wget)))



(defun dino-javascript-mode--win-specific-fn ()
  (if (eq system-type 'windows-nt)
      (progn
        (require 'jscomp)
        (local-set-key "\M-."     'jscomp-complete))))


  ;; The following needs to be in jslint-for-wsh.el or whatever
  ;; ;; jslint-for-wsh.js, produces errors like this:
  ;; ;; file.cs(6,18): JSLINT: The body of a for in should be wrapped in an if statement ...
  ;; (if (and (eq system-type 'windows-nt)
  ;;          (boundp 'compilation-error-regexp-alist-alist))
  ;;     (progn
  ;;       (add-to-list
  ;;        'compilation-error-regexp-alist-alist
  ;;        '(jslint-for-wsh
  ;;          "^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\)) \\(Microsoft JScript runtime error\\|JSLINT\\|JSHINT\\): \\(.+\\)$" 1 2 3))
  ;;       (add-to-list
  ;;        'compilation-error-regexp-alist
  ;;        'jslint-for-wsh))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TFS commands

(require 'tfs)

(setq tfs/login (getenv "CPLOGIN"))
(setq tfs/tf-exe  "c:\\vs2010\\common7\\ide\\tf.exe")
(setq tfs/tfpt-exe "c:\\tfpt\\TFPT.exe")

(global-set-key  "\C-xvo" 'tfs/checkout)
(global-set-key  "\C-xvi" 'tfs/checkin)
(global-set-key  "\C-xvp" 'tfs/properties)
(global-set-key  "\C-xvg" 'tfs/get)
(global-set-key  "\C-xvh" 'tfs/history)
(global-set-key  "\C-xvu" 'tfs/undo)
(global-set-key  "\C-xvd" 'tfs/diff)
(global-set-key  "\C-xvs" 'tfs/status)
(global-set-key  "\C-xvr" 'tfs/rename)
(global-set-key  "\C-xv+" 'tfs/add)
(global-set-key  "\C-xv-" 'tfs/delete)
(global-set-key  "\C-xva" 'tfs/annotate)
(global-set-key  "\C-xvc" 'tfs/changeset)
(global-set-key  "\C-xvw" 'tfs/workitem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS Shell
(require 'jsshell-bundle)
(setq jsshell-profile
   (list "c:\\dev\\js\\json2.js"
         "c:\\dev\\js\\stringExtensions.js"
         "c:\\dev\\js\\moment.js"
         "c:\\dev\\js\\arrayExtensions.js"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-localized-time-format t)
 '(temporary-file-directory "C:/Users/dpchi/AppData/Local/Temp"))

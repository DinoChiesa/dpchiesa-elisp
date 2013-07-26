;;
;; Dino's .emacs setup file.
;; Last saved: <2013-July-25 18:03:47>
;;
;; Works with v24.2 of emacs.
;;

(message "Running emacs.el...")

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function `(lambda ()
                            (set-face-background 'default "DodgerBlue")
                            (set-face-background 'default "black")))

(setq inhibit-splash-screen t)
(tool-bar-mode -1) ;; we don't need no steenking icons
(setq user-mail-address "dpchiesa@hotmail.com")

(setq comment-style 'indent) ;; see doc for variable comment-styles

;; for tetris
(setq tetris-score-file "~/elisp/tetris-scores")
(defadvice tetris-end-game (around zap-scores activate)
  (save-window-excursion ad-do-it))

;; To change the coding system of a visited file,
;; `C-x RET r utf-8-with-signature RET'.
;;
;; Try  M-x list-coding-systems   ... to see a list
;;
(prefer-coding-system 'utf-8-auto) ;; unicode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directory to load additional libraries from :
;; This dir contains:
;;  yasnippet.el defaultcontent.el, csharp-mode.el,
;;  powershell-mode.el, powershell.el, htmlize.el
;;  javascript.el, espresso, etc

(add-to-list 'load-path "~/elisp")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handy utility functions for various purposes
;;

;; Windows only
;; upon kill, check clipboard, and if exists, put it into the kill ring.
(if (eq system-type 'windows-nt)
    (defadvice kill-new (before
                         dino-kill-new-push-xselection-on-kill-ring
                         activate)
      "Before putting new kill onto the kill-ring, add the clipboard/external
selection to the kill ring"
      (let ((have-paste (and interprogram-paste-function
                             (funcall interprogram-paste-function))))
        (when have-paste (push have-paste kill-ring)))))


;; System-specific configuration
;; Loads system-type config; e.g. "darwin.el" on Mac
(let ((system-specific-elisp (concat "~/elisp/" (symbol-name system-type) ".el")))
  (if (file-exists-p system-specific-elisp)
    (load system-specific-elisp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a bunch of random utility functions
;;
(require 'dino-utility)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apigee stuff
;;
(require 'apigee)
(setq apigee-apiproxies-home "~/dev/apiproxies/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups
;;
;; Setting `backup-by-copying' to non-nil says "copy the file to a backup
;; before editing, and edit the original inode."  The default is nil,
;; which *renames* the original file to the backup name, and then edits
;; a copy that is given the original name.  This default behavior means,
;; when editing a hardlinked file, emacs breaks the link.  Very unfortunate.
;;
;; Setting `backup-by-copying' to non-nil  means the right thing gets done
;; with hardlinks.
;;
;; ps: hardlinks can be created in dired mode with H.
;;

(set-variable 'backup-by-copying t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete

;; (add-to-list 'load-path "/Users/Dino/elisp/autocomplete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/Users/Dino/elisp/autocomplete/ac-dict")
;; (ac-config-default)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic, default colors

;; see this for an example of color theming:
;;    http://hi.baidu.com/masterray/blog/item/9caa181fb9177cc2a68669a8.html


(set-background-color "black")  ;; need this if I also do set-face-background?
(set-face-foreground 'default "white")
(set-face-background 'default "black")


;; I couldn't get eval-after-load to work with hl-line, so
;; I made this an after advice??
(defadvice hl-line-mode (after
                         dino-advise-hl-line-mode
                         activate compile)
  (set-face-background hl-line-face "gray13"))

(global-hl-line-mode)

;;(set-face-background 'font-lock-comment-face "black")
(set-face-foreground 'font-lock-comment-face       "VioletRed1")
(set-face-foreground 'font-lock-constant-face      "DodgerBlue")
(set-face-foreground 'font-lock-string-face        "MediumOrchid1")
(set-face-foreground 'font-lock-keyword-face       "Cyan1")
(set-face-foreground 'font-lock-function-name-face "RoyalBlue1")
(set-face-foreground 'font-lock-variable-name-face "LightGoldenrod")
(set-face-foreground 'font-lock-type-face          "PaleGreen")

;;(make-face 'font-lock-reference-face)
;;(set-face-foreground 'font-lock-reference-face "Navy")

(set-face-foreground 'tooltip "Navy")
(set-face-background 'tooltip "khaki1")

(set-face-background 'font-lock-string-face "gray11")


;;(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; httpget

(require 'httpget)
;;(setq httpget--wget-prog "c:\\dev\\dotnet\\wget\\wget.exe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word-count minor mode

(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings
;
(global-set-key "\C-xw" 'dino-fixup-linefeeds)
(global-set-key "\C-cg" 'httpget)
(global-set-key "\C-cu" 'dino-insert-uuid)
(global-set-key "\C-cl" 'lorem-ipsum)
(global-set-key "\C-cb" 'dino-base64-insert-file)
(global-set-key "\C-c1" 'just-one-space)
(global-set-key "\C-x|"     'align-regexp)
(global-set-key "\C-x?"     'describe-text-properties)
(global-set-key "\M-\C-^"   'describe-variable)
(global-set-key "\M-+"      'word-count-mode)
(global-set-key "\C-^"      'describe-key-briefly)
(global-set-key "\C-x\C-d"  'delete-window)
(global-set-key "\C-xd"     'dino-ediff-buffer-against-file)
(global-set-key "\C-x\C-r"  'dino-resize-big)
(global-set-key "\C-xx"     'copy-to-register)
(global-set-key "\C-xg"     'insert-register)
(global-set-key "\C-xp"     'previous-window)
(global-set-key "\C-x\C-p"  'previous-window)
(global-set-key "\C-c\C-x\C-c"  'calendar)
(global-set-key "\C-xn"     'other-window)
(global-set-key "\C-x\C-e"  'smarter-compile)
(global-set-key "\C-x\C-g"  'auto-fill-mode)
(global-set-key "\C-x\C-n"  'next-error)
(global-set-key "\C-xt"     'dino-toggle-truncation)
(global-set-key "\M-\C-y"   'yank-pop)
(global-set-key "\M-g"      'goto-line)
(global-set-key "\M- "      'set-mark-command)
(global-set-key "\M-\C-h"   'backward-kill-word)
(global-set-key "\C-c\C-c"  'center-paragraph)  ; good for text mode
(global-set-key "\C-ck"     'global-set-key)
(global-set-key "\C-cs"     'search-forward-regexp)
(global-set-key "\C-cy"     'linum-mode)

(global-set-key "\C-cq"     'query-replace)
(global-set-key "\C-cc"     'goto-char)
(global-set-key "\C-cr"     'replace-regexp)
(global-set-key "\C-ct"     'dino-insert-timeofday)
;;(global-set-key "\C-c\C-t"  'dino-insert-timestamp)
(global-set-key "\C-c\C-t"  'dino-toggle-frame-split)
(global-set-key "\C-cw"     'where-is)
(global-set-key "\C-c\C-w"  'compare-windows)
(global-set-key "\C-c!"     'revert-buffer-unconditionally)
(global-set-key "\C-x!"     'dino-toggle-buffer-modified)
(global-set-key (kbd "C-<") 'beginning-of-defun)
(global-set-key (kbd "C->") 'end-of-defun)


;;the help key is assigned to Ctrl-\, or Esc-Ctrl-\
(global-set-key "\M-\C-\\"   'help-for-help)
(global-set-key "\C-\\"      'help-for-help)

(require 'skeleton)

;; handle text end-of-line conventions the way it oughta be:
(setq inhibit-eol-conversion nil)

;; turn on font-lock globally
(global-font-lock-mode 1) ;;  'ON

(setq-default fill-column 72)
(setq auto-save-interval 500)
(setq case-fold-search nil)
(setq comment-empty-lines t)


;; helpful for debugging lisp code:
(setq messages-buffer-max-lines 2500)

;; for fontification in emacs progmodes:
(load "font-lock")
(setq font-lock-maximum-decoration t)

(setq completion-auto-help nil)

(put 'eval-expression 'disabled nil)

;; set truncation on side-by-side windows to nil.
(setq truncate-partial-width-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames preferences: initial and default frames
;; see http://www.gnu.org/software/emacs/windows/big.html#windows-frames
(setq default-frame-alist
      '((top . 10) (left . 860)
        (width . 100) (height . 25)
        (cursor-color . "Orange")
        (cursor-type . box)
        ;;(foreground-color . "White")
        ;;(background-color . "Black")
        (mouse-color . "sienna3")
        (font . "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
        )
      )

;;(set-face-font 'tooltip "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
;;(set-face-font 'tooltip "-outline-Lucida Sans Typewriter-normal-r-normal-normal-15-112-96-96-c-*-iso8859-1")
;;(set-face-font 'tooltip "-outline-Lucida Sans Typewriter-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
(if (or (eq system-type 'windows-nt)
       (eq system-type 'darwin))
(set-face-font 'tooltip "-outline-Lucida Console-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1"))


;; (message (face-font 'tooltip))
;; (message (face-font 'default))
;; (set-face-font 'tooltip "-outline-Lucida Sans Typewriter-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1")
;; (tooltip-show "This is a tooltip, in the new font\nthat I set in elisp.")


;; (if (eq system-type 'windows-nt)
;;   (set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))
;;
;; (if (eq window-system 'x)
;;   (set-default-font "Inconsolata-11"))


;; initial frame is 128 wide x 68 high
(setq initial-frame-alist
      '( (top . 30) (left . 10)
         (width . 90) (height . 52)
         ;;(width . 128) (height . 68)
         )
      )

;; what should a frame look like
(setq frame-title-format '("%f [mode: %m]" )    ; "filename [mode]" in title bar
      icon-title-format '("emacs: %b"))   ; "emacs: buffername" in icon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  (setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved


(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'iirf-mode "iirf-mode" "Major mode for editing IIRF ini files." t)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustment to mode mappings
;
;; In the regexp's, the trailing \\' represents "end of string".
;; The $ represents the zero-width place before newline.
;; They are equivalent unless there is a filename with a new line in it
;; (not likely).
(setq auto-mode-alist
      (append
       '(
         ("\\.\\(war\\|ear\\|WAR\\|EAR\\)\\'" . archive-mode)        ; java archives
         ;;("\\.s?html?\\'"                     . nxhtml-mumamo-mode)
         ("\\(Iirf\\|iirf\\|IIRF\\)\\(Global\\)?\\.ini$"   . iirf-mode)
         ("\\.css$"                           . css-mode)
         ("\\.php$"                           . php-mode)
         ("\\.cs$"                            . csharp-mode)
         ("\\.asp$"                           . html-mode)
         ;;("\\.aspx$"                        . html-helper-mode)
         ("\\.aspx$"                          . aspx-mode)
         ("\\.ashx$"                          . csharp-mode)
         ("\\.ascx$"                          . csharp-mode)
         ("\\.s?html?\\'"                     . html-mode)
         ("\\.html$"                          . html-mode)
         ("\\.htm$"                           . html-mode)
         ("\\.md$"                            . fundamental-mode)  ;; markdown
         ("\\.el$"                            . emacs-lisp-mode)
         ;; ("\\.\\(js\\|jsi\\)$"                . javascript-mode)
         ;; ("\\.\\(js\\|jsi\\)$"                . espresso-mode)
         ;; ("\\.js$"                            . js2-mode)
         ("\\.\\(js\\|gs\\|jsi\\)$"           . js-mode)
         ("\\.txt$"                           . text-mode)
         ("\\.asmx$"                          . csharp-mode)         ; likely, could be another language tho
         ("\\.\\(vb\\)$"                      . vbnet-mode)
         ("\\.\\(vbs\\)$"                     . vbs-mode)
         ("\\.\\(cs\\|vb\\|shfb\\)proj$"      . xml-mode)            ; msbuild file
         ("\\.config$"                        . xml-mode)            ; .NET config file
         ("\\.\\(xsd\\|wsdl\\)$"              . xml-mode)            ; schema or WSDL file
         ("\\.sln$             "              . xml-mode)            ; VS2008 .sln file
         ("\\.\\(wxs\\|wxl\\|wixproj\\)$"     . xml-mode)            ; WiX, wixproj, etc
         ("\\.ssml$"                          . xml-mode)            ; Speech markup
         ("\\.\\(aml\\|xaml\\)$"              . xml-mode)            ; SHFB markup, XAML
         ("\\.\\(wsc\\|wsf\\)$"               . xml-mode)            ; Windows Script Component, WSCript file.
         ("\\.\\(xjb\\)$"                     . xml-mode)            ; JAXB bindings file
         ) auto-mode-alist ))


;;;; Keep this snip !
;;;; Modify and Run it to temporarily append an extension to the list, "right now".
;;        (setq auto-mode-alist
;;              (append '(
;;                 ("\\.\\(jsi\\|js\\)$"               . javascript-mode)
;;                        ) auto-mode-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

;; see http://orgmode.org/manual/index.html
(setq org-log-done 'time) ;; timestamps when completing a TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlize

;;(require 'htmlize)
(autoload 'htmlize-buffer "htmlize"
  "Turning code into HTML." t)

(eval-after-load "htmlize"
  '(progn
     (setq htmlize-output-type 'inline-css)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(eval-after-load "image-dired"
  '(progn
     (require 'image-dired-fixups)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua

(autoload 'lua-mode "lua-mode" "major mode for editing Lua." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-default-application "c:\\tools\\lua\\lua52.exe")

(eval-after-load "lua-mode"
  '(progn
     (defadvice lua-start-process (before
                                   dino-set-lua-shell-buffer-name-nicely
                                   activate)
       "Use a proper name for the interactive LUA shell. "
       (let ((arg0 (ad-get-arg 0))
             (arg1 (ad-get-arg 1)))
         (if arg0
             (when (not arg1)
               (ad-set-arg 0 "LuaShell")
               (ad-set-arg 1 arg0))
           (ad-set-arg 0 "LuaShell")
           (ad-set-arg 1 lua-default-application))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML

(defun dino-html-mode-fn ()
  ;;(setq powershell-indent-width 2)
  (local-set-key "\C-c1" 'just-one-space)

  (local-set-key (kbd "<f7>") 'find-file-at-point)

  ;; Make sure this is OFF.
  ;; For some reason this is not working. When I open
  ;; an HTML buffer, refill (autofill) is ON. ???
  (require 'refill)
  (refill-mode 0) )

(add-hook 'html-mode-hook 'dino-html-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSharp Code Completion

(add-to-list 'load-path (file-name-as-directory "~/cscomp"))
(add-to-list 'load-path (file-name-as-directory "~/auto-complete-1.3.1"))


;;(require 'csharp-completion)
(autoload 'cscomp-complete-at-point      "csharp-completion"  "code-completion for C#" t)
(autoload 'cscomp-complete-at-point-menu "csharp-completion"  "code-completion for C#" t)

(eval-after-load "csharp-completion"
  '(progn
     (setq cscomp-assembly-search-paths
      (list "c:\\.net2.0"
            "c:\\.net3.0RA" ;; speech, WPF, WCF, UIAutomation
            ;;"c:\\.net3.5" -- not necessary, there are no DLLs there
            "c:\\Program Files (x86)\\Microsoft ASP.NET\\ASP.NET MVC 2\\Assemblies"             ;; for ASPNET MVC2
            "c:\\.net3.5ra"  ;; Linq, System.ServiceModel.Web, etc
            "c:\\users\\dino\\dev\\DotNet"  ;; for ICSharpCode.NRefactory.dll, etc
            "c:\\users\\dino\\bin"  ;; for Ionic.Zip.dll
            "c:\\Program Files (x86)\\Microsoft WCF REST\\WCF REST Starter Kit Preview 2\\Assemblies"
            ))
     ))

;;TODO: fix this
;; (csde-complete-load-additional-assemblies
;;   (list "Ionic.Zip" "System.Xml" "System.Data" "System.Drawing" "System.Windows.Forms"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;
;; grabbed 2008 april 11
;; see http://capitaomorte.github.com/yasnippet/snippet-organization.html
;;


(require 'yasnippet)
(require 's) ;; string library

;; If I don't set both yas/snippet-dirs and yas/root-directory, I
;; get complaints in *Messages*.

(setq yas/snippet-dirs (list "~/elisp/snippets"))
(yas/initialize)
(setq yas/root-directory "~/elisp/snippets")
(yas/load-directory yas/root-directory)

(defun dino-recompile-then-reload-all-snippets (&optional dir)
  "Recompile and reload all snippets in a toplevel snippets
directory specified by DIR, or in `yas/root-directory' if DIR is
not specified.

A simple reload-all does not re-compile snippets; instead it
reloads the 'precompiled' snippets. And, that's not really a good
idea, because precompiling snippets manually does not compile
them correctly: they lack the correct mode specification. Huh?
Obviously nobody tested this. Open-sores software.

It is unbelievable to me that it takes a custom-written function
to accomplish this. I'm certain I must be doing something wrong,
but I was unable to fall into a pit of success after much
searching, wailing, and gnashing of teeth.

So I wrote this function which does what I think it should do,
which is to recompile and then reload all the snippets in my
snippets directory.

"
  (interactive)
  (let ((top-dir (or dir yas/root-directory)))
    ;; delete all pre-compiled
    (dolist (subdir (yas/subdirs top-dir))
      (let ((fq-elfile (concat subdir "/.yas-compiled-snippets.el")))
        (message (concat "checking " fq-elfile))
        (if (file-exists-p fq-elfile)
            (delete-file fq-elfile))))
    (yas/compile-top-level-dir top-dir)
    (yas/load-directory top-dir)))


;; REDEFINE
;; This fixes up a defun in yasnippet.
;; The original does not specify the mode in the yas/define-snippet
;; call in the generated .el file.
(defun yas/compile-snippets (input-dir &optional output-file)
  "Compile snippets files in INPUT-DIR to OUTPUT-FILE file.

Prompts for INPUT-DIR and OUTPUT-FILE if called-interactively"
  (interactive (let* ((input-dir (read-directory-name "Snippet dir "))
                      (output-file (let ((ido-everywhere nil))
                                     (read-file-name "Output file "
                                                     input-dir nil nil
                                                     ".yas-compiled-snippets.el"
                                                     nil))))
                 (list input-dir output-file)))
  (let ((default-directory input-dir)
        (major-mode-and-parents (yas/compute-major-mode-and-parents
                                 (concat input-dir "/dummy"))))
    (with-temp-file (setq output-file (or output-file ".yas-compiled-snippets.el"))
      (flet ((yas/define-snippets
              (mode snippets &optional parent-or-parents)
              (insert (format ";;; %s - automatically compiled snippets for `%s' , do not edit!\n"
                              (file-name-nondirectory output-file) mode))
              (insert ";;;\n")
              (let ((literal-snippets (list)))
                (dolist (snippet snippets)
                  (let ((key                    (first   snippet))
                        (template-content       (second  snippet))
                        (name                   (third   snippet))
                        (condition              (fourth  snippet))
                        (group                  (fifth   snippet))
                        (expand-env             (sixth   snippet))
                        (file                   nil) ;; (seventh snippet)) ;; omit on purpose
                        (binding                (eighth  snippet))
                        (uuid                    (ninth   snippet)))
                    (push `(,key
                            ,template-content
                            ,name
                            ,condition
                            ,group
                            ,expand-env
                            ,file
                            ,binding
                            ,uuid)
                          literal-snippets)))
                (insert (pp-to-string `(yas/define-snippets ',mode ',literal-snippets ',parent-or-parents)))
                (insert "\n\n")
                (insert (format ";;; %s - automatically compiled snippets for `%s' end here\n"
                                (file-name-nondirectory output-file) mode))
                (insert ";;;"))))
        (yas/load-directory-1 input-dir (car major-mode-and-parents)
                              nil 'no-compiled-snippets))))

  (if (and (called-interactively-p)
           (yes-or-no-p (format "Open the resulting file (%s)? "
                                (expand-file-name output-file))))
      (find-file-other-window output-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VBNET mode (and also, VBScript)

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(autoload 'vbs-mode "vbs-mode" "Mode for editing VBScript code." t)

(defun dino-vbnet-mode-fn ()
  "My hook for VB.NET mode (and VBScript)"
  (interactive)
  (turn-on-font-lock)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-w" 'compare-windows)
  (local-set-key "\C-c\C-c"  'comment-region)

  (turn-on-auto-revert-mode)

  ;; "no tabs" -- use only spaces
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  ;; for snippets support:
  (require 'yasnippet)
  (yas/minor-mode-on)

  (local-set-key "\C-c\C-n"  'flymake-goto-next-error)
  (local-set-key "\C-c\C-m"  'flymake-display-err-menu-for-current-line)

  (require 'autopair)
  (autopair-mode 1)

  ;;(require 'myfixme)
  ;;(myfixme-mode 1)

  (require 'rfringe)
  (setq comment-empty-lines t))


(add-hook 'vbnet-mode-hook 'dino-vbnet-mode-fn)
(add-hook 'vbs-mode-hook 'dino-vbnet-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS mode

;;(require 'css-mode)
(autoload 'css-mode "css-mode" "Mode for editing Cascading Stylesheets." t)

(defun dino-css-mode-fn ()
  "My hook for CSS mode"
  (interactive)
  (turn-on-font-lock)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-w" 'compare-windows)
  (local-set-key "\C-c\C-c" 'comment-region)

  (turn-on-auto-revert-mode)

  (require 'autopair)
  (autopair-mode)

  (setq css-indent-offset 2)

  ;; auto-complete mode is on globally
  ;; make sure that <RETURN> is not an autocomplete key
  (define-key ac-complete-mode-map "\r" nil)

  ;; make auto-complete start only after 2 chars
  (setq ac-auto-start 2)  ;;or 3?

  (require 'flymake)
  (flymake-mode 1)

  ;; "no tabs" -- use only spaces
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil))

(add-hook 'css-mode-hook 'dino-css-mode-fn)

(require 'csslint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defaultcontent
;;
;; This package inserts "default content" into a new  file of a given
;; type, when it is first edited in emacs.
;; See the elisp/defaultcontent directory for templates.
;;
;; http://pagesperso-systeme.lip6.fr/Christian.Queinnec/Miscellaneous/defaultcontent.el

(require 'defaultcontent)

;; specify the directory to look in for templates
(setq dc-auto-insert-directory "/Users/Dino/elisp/defaultcontent")
(setq dc-fast-variable-handling t)

;; specify the template to use for various files:
(setq dc-auto-insert-alist
      '(
        ("\\.cs$"                      .  "Template.cs" )
        ("\\.css$"                     .  "Template.css" )
        ("\\.asp$"                     .  "Template.asp" )
        ("\\.aspx$"                    .  "Template.aspx" )
        ("\\.aml$"                     .  "Template.aml" )
        ("\\.csproj$"                  .  "Template.csproj" )
        ("\\.vb$"                      .  "Template.vb" )
        ("\\.vbs$"                     .  "Template.vbs" )
        ("\\.java$"                    .  "Template.java" )
        ("PostProcessMsi\\.js$"        .  "Template-PostProcessMsi.js" )
        ("\\.js$"                      .  "Template.js" )
        ("\\.wsf$"                     .  "Template.wsf" )
        ;;("\\.\\(htm\\|html\\)$"        .  "Template.htm" )
        ("\\.htm$"                     .  "Template.htm" )
        ("\\.html$"                    .  "Template.html" )
        ("\\.c$"                       .  "Template.c" )
        ("\\.wsc$"                     .  "Template.wsc" )
        ("\\.ashx$"                    .  "Template.ashx" )
        ("\\(-\\|\\.\\)vb\\.ashx$"     .  "Template-vb.ashx" )
        ("\\(-\\|\\.\\)cs\\.ashx$"     .  "Template.ashx" )
        ("\-vb\\.ashx$"                .  "Template-vb.ashx" )
        ("\\.wsdl$"                    .  "Template.wsdl" )
        ("\\.xsd$"                     .  "Template.xsd" )
        ("\\.xsl$"                     .  "Template.xsl" )
        ("\\.ssml$"                    .  "Template.ssml" )
        ("\\.bat$"                     .  "Template.bat" )
        ("\\.cmd$"                     .  "Template.bat" )
        ("makefile$"                   .  "Template.makefile" )
        ("\\.wixproj$"                 .  "Template.wixproj" )
        ("\\.rss$"                     .  "Template.rss" )
        ) )

;; (setq dc-auto-insert-alist
;;       (append '(
;;                 ("\\.\\(htm\\|html\\)$"   .  "Template.htm" )
;;                 ) dc-auto-insert-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure external utilities

(if (file-exists-p "/Users/Dino/bin/unzip.exe")
    (progn
      (setq archive-zip-use-pkzip nil   ; i.e. use unzip instead
            archive-zip-extract '("/Users/Dino/bin/unzip.exe" "-"))))

(setq-default grep-command "grep -i ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timestamp
;;
;; put this in a file in the first N lines to get an auto-updated timestamp:
;;  Last Saved: <>
;;
;; I use a custom pattern to allow a bunch of variations on how the
;; timestamp appears.
;;

(setq
  time-stamp-active t          ; do enable time-stamps
  ;; time-stamp-line-limit 34     ; check first N buffer lines for Time-stamp: <>
  ;; example: Tuesday, July 15, 2008  10:59:09  (by dinoch)
  ;;time-stamp-format "%:a, %:b %02d, %04y  %02H:%02M:%02S %Z (by %u)") ; date format
  ;;time-stamp-format "%04y-%:b-%02d %02H:%02M:%02S" ; date format
  time-stamp-pattern "34/\\(\\(L\\|l\\)ast\\( \\|-\\)\\(\\(S\\|s\\)aved\\|\\(M\\|m\\)odified\\|\\(U\\|u\\)pdated\\)\\|Time-stamp\\) *: <%04y-%:b-%02d %02H:%02M:%02S>")

;; can also add this to source code: // (set-variable time-stamp-format "%04y-%:b-%02d %02H:%02M:%02S")

(add-hook 'before-save-hook 'time-stamp)  ; update time stamps when saving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode

(require 'dired)
(require 'dired-fixups)

(defun dino-dired-mode-hook-fn ()
  (hl-line-mode 1)

  ;; do not want auto-revert.
  ;; It doesn't work completely, and it may have side effects.
  ;; ;; (turn-on-auto-revert-mode)

  (local-set-key "\C-c\C-c"  'dino-dired-copy-file-to-dir-in-other-window)

  (local-set-key "F" 'dino-dired-do-find))

(add-hook 'dired-mode-hook 'dino-dired-mode-hook-fn)

;; eliminate the gid in dired on windows
(setq ls-lisp-verbosity '(links uid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode

(defun dino-fix-abbrev-table ()
  "set up a custom abbrev table. The normal
saving isn't allowed on my computer. Really these are
just auto-corrects on common mis-spellings by me. "

  (define-abbrev-table 'text-mode-abbrev-table
    '(
      ("rigueur" "rigeuer" nil 1)
      ("riguer" "rigeuer" nil 1)
      ("hygeine" "hygiene" nil 0)
      ("comittee" "committee" nil 0)
      ("recieve" "receive" nil 0)
      ("vairous" "various" nil 0)
      ("acheive" "achieve" nil 0)
      ("acheived" "achieved" nil 0)
      ("APigee" "Apigee" nil 1)
      ("teh" "the" nil 1)
      ("becasue" "because" nil 1)
      ("btw" "by the way" nil 3)
      )
    ))


(defun dino-text-mode-hook-fn ()
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (dino-fix-abbrev-table)
  ;;
  ;; add new abbreviations above.
  ;;

  ;; (require 'refill) ;; automatically refill paragraphs
  ;; (refill-mode 1)
  )

(add-hook 'text-mode-hook 'dino-text-mode-hook-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linum - line numbering
;

;;(require 'linum)
(autoload 'linum-on "linum-ex" nil t)
(autoload 'linum-mode "linum-ex" nil t)

(eval-after-load "linum-ex"
  '(progn

    ;;(message "%s" (prin1-to-string (defined-colors))) ;; to list possible colors
     ;;(list-colors-display) to show a list of colors in a new buffer
    (set-face-foreground 'linum "SlateGray")
    ;;(set-face-background 'linum "WhiteSmoke")
    (set-face-background 'linum "gray19")
    ;; to see font string M-x describe-font
    ;;(set-face-font 'linum "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
    (set-face-font 'linum "-*-Consolas-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
    ;;(setq setnu-line-number-face 'setnu)
    (setq linum-delay t)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
;
; for a potential source of additional fixes/changes/updates, see
; https://github.com/illusori/emacs-flymake . The problem with that
; update is that the changes are not incremental - need to adopt
; everything all at once.  It includes fixes to, for example, run the
; flymake check only on the current buffer.  If you have 10 open PHP
; modules, instead of flymake checking 10 buffers, it checks only the
; current one for changes. This can save busy work, save cpu stress.
;


(setq nmake.exe "\\vs2010\\VC\\bin\\amd64\\nmake.exe")

(defun dino-flymake-create-temp-intemp (file-name prefix)
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

;; configuration for flymake.el
(require 'flymake-fixups)

;; enhancements for displaying flymake errors
(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode  (common)
;
(defun dino-c-mode-common-hook-fn ()
  (cond
   (window-system

    (turn-on-auto-revert-mode)

    ;; RETURN means newline-and-indent in any c-mode buffer
    (local-set-key (kbd "RET") 'newline-and-indent)

    ;; for re-tabbing a region or buffer of code:
    (local-set-key "\M-\C-R" 'indent-region)
    (local-set-key "\M-#"    'dino-indent-buffer)
    ;; set this key binding on for c-mode, in lieu of c-subword mode,
    ;; which overwrites my preference
    (local-set-key "\C-c\C-w"  'compare-windows)

    (hl-line-mode 1)

    ;; ;; allow fill-paragraph to work on xml code doc
    ;; (make-local-variable 'paragraph-separate)

    ;; ;; whitespc
    ;; ;; two or more slashes or one or more stars
    ;; (setq paragraph-separate "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

    ;; never convert leading spaces to tabs:
    ;;(make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil)

    ;; remove trailing whitespace in C files
    ;; http://stackoverflow.com/questions/1931784
    ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
    (add-hook 'local-write-file-hooks
              '(lambda ()
                 (save-excursion
                   (delete-trailing-whitespace))))

    (message "dino-c-mode-common-hook-fn: done."))))

(add-hook 'c-mode-common-hook 'dino-c-mode-common-hook-fn)


;;;; This hook does an untabify upon opening.  maybe not desirable,
;;;; because all newly opened buffers are marked as modified.
;; (defun dino-c-mode-common-untabify ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "[ \t]+$" nil t)
;;       (delete-region (match-beginning 0) (match-end 0)))
;;     (goto-char (point-min))
;;     (if (search-forward "\t" nil t)
;;      (untabify (1- (point)) (point-max))))
;;   nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Programming language
(defun dino-c-mode-hook-fn ()
  (cond (window-system

         (c-set-style "myCStyle")

         ;; turn off the c-indent-region thing; I don't like the indents
         ;; it gives me!  by using nil here, I get
         ;; indent-according-to-mode, which is what works for me.
         (setq indent-region-function nil)

         (setq c-auto-newline nil)

         (require 'flymake)
         (flymake-mode 1)

         (set (make-local-variable 'comment-start) "// ")
         (set (make-local-variable 'comment-end) "")

         (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
         (set (make-local-variable 'skeleton-pair) t)

         ;; allow fill-paragraph to work on xml code doc
         (set (make-local-variable 'paragraph-separate)
              ;; whitespc + two or more slashes or one or more stars
              "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")
         )))

(add-hook 'c-mode-hook 'dino-c-mode-hook-fn)


(c-add-style "myCStyle"
             '("bsd"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 4)
               (c-echo-syntactic-information-p . t)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((c                     . c-lineup-C-comments)
                                   (statement-case-open   . 0)
                                   (case-label            . +)
                                   (substatement-open     . 0)
                                   ))
               ))


(fset 'dino-start-c-comment   "/* ")

(fset 'dino-end-c-comment   " */")

(fset 'dino-end-block-comment
      [escape ?\C-b escape ?  ?\C-a ?\C-w ?\C-y escape ?\C-f ?  ?/ ?/ ?  ?\C-y ?\C-x ?\C-x ?\C-c ?1])


(defun dino-c-get-value-from-comments (marker-string line-limit)
  "gets a string from the header comments in the current buffer.

    This is used to extract the compile command from the comments. It
    could be used for other purposes too.

    It looks for \"marker-string:\" and returns the string that
    follows it, or returns nil if that string is not found.

    eg, when marker-string is \"compile\", and the following
    string is found at the top of the buffer:

         compile: cl.exe /I uthash

    ...then this command will return the string

         \"cl.exe /I uthash\"

    It's ok to have whitespace between the marker and the following
    colon.

    "
      (let (start search-limit found)
        ;; determine what lines to look in
        (save-excursion
          (save-restriction
            (widen)
            (cond ((> line-limit 0)
                   (goto-char (setq start (point-min)))
                   (forward-line line-limit)
                   (setq search-limit (point)))
                  ((< line-limit 0)
                   (goto-char (setq search-limit (point-max)))
                   (forward-line line-limit)
                   (setq start (point)))
                  (t                        ;0 => no limit (use with care!)
                   (setq start (point-min))
                   (setq search-limit (point-max))))))

        ;; look in those lines
        (save-excursion
          (save-restriction
            (widen)
            (let ((re-string
                   (concat "\\b" marker-string "[ \t]*:[ \t]*\\(.+\\)$")))
              (if (and start
                       (< (goto-char start) search-limit)
                       (re-search-forward re-string search-limit 'move))

                  (buffer-substring-no-properties
                   (match-beginning 1)
                   (match-end 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpp mode
                                        ;
;;    (c-basic-offset . 4)
;;    (c-comment-only-line-offset . (0 . 0))
;;    (c-offsets-alist
;;     . (
;;        (access-label . -)
;;        (arglist-close . c-lineup-arglist)
;;        (arglist-cont . 0)
;;        (arglist-cont-nonempty . c-lineup-arglist)
;;        (arglist-intro . c-lineup-arglist-intro-after-paren)
;;        (block-close . 0)
;;        (block-open . 0)
;;        (brace-entry-open . 0)
;;        (brace-list-close . 0)
;;        (brace-list-entry . 0)
;;        (brace-list-intro . +)
;;        (brace-list-open . +)
;;        (c . c-lineup-C-comments)
;;        (case-label . +)
;;        (catch-clause . 0)
;;        (class-close . 0)
;;        (class-open . 0)
;;        (comment-intro . c-lineup-comment)
;;        (cpp-macro . 0)
;;        (cpp-macro-cont . c-lineup-dont-change)
;;        (defun-block-intro . +)
;;        (defun-close . 0)
;;        (defun-open . 0)
;;        (do-while-closure . 0)
;;        (else-clause . 0)
;;        (extern-lang-close . 0)
;;        (extern-lang-open . 0)
;;        (friend . 0)
;;        (func-decl-cont . +)
;;        (inclass . +)
;;        (inexpr-class . +)
;;        (inexpr-statement . 0)
;;        (inextern-lang . +)
;;        (inher-cont . c-lineup-multi-inher)
;;        (inher-intro . +)
;;        (inlambda . c-lineup-inexpr-block)
;;        (inline-close . 0)
;;        (inline-open . 0)
;;        (innamespace . +)
;;        (knr-argdecl . 0)
;;        (knr-argdecl-intro . 5)
;;        (label . 0)
;;        (lambda-intro-cont . +)
;;        (member-init-cont . c-lineup-multi-inher)
;;        (member-init-intro . +)
;;        (namespace-close . 0)
;;        (namespace-open . 0)
;;        (objc-method-args-cont . c-lineup-ObjC-method-args)
;;        (objc-method-call-cont . c-lineup-ObjC-method-call)
;;        (objc-method-intro . [0])
;;        (statement . 0)
;;        (statement-block-intro . +)
;;        (statement-case-intro . +)
;;        (statement-case-open . +)
;;        (statement-cont . +)
;;        (stream-op . c-lineup-streamop)
;;        (string . c-lineup-dont-change)
;;        (substatement . +)
;;        (substatement-open . 0)
;;        (template-args-cont c-lineup-template-args +)
;;        (topmost-intro . 0)
;;        (topmost-intro-cont . 0)
;;        ))
;;    ))


(defconst my-cpp-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-tab-always-indent        . t)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C++ Programming Style")

(c-add-style "myCppStyle" my-cpp-style)


(defun dino-cpp-mode-fn ()
  (cond (window-system
         (c-set-style "myCppStyle")

         (turn-on-font-lock)

         (message "setting local key bindings....")

         (local-set-key "\M-\C-R"  'indent-region)
         (local-set-key "\M-#"     'dino-indent-buffer)
         (local-set-key "\C-c\C-w" 'compare-windows)

         ;; for skeleton stuff
         (set (make-local-variable 'skeleton-pair) t)

         ;; for commenting
         (setq comment-empty-lines t)

         (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
         ;;(local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

         ;; these allow typeover of matching brackets
         (local-set-key (kbd "\"") 'dino-skeleton-pair-end)
         (local-set-key (kbd ")") 'dino-skeleton-pair-end)
         (local-set-key (kbd "]") 'dino-skeleton-pair-end)

         (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
         ;;(local-set-key (kbd "{") 'dino-insert-open-brace)

         ;; Default to auto-indent on Enter
         ;;(define-key csharp-mode-map [(control j)] 'newline)
         ;;(define-key csharp-mode-map [(control m)] 'newline-and-indent)

         ;;(define-key csharp-mode-map [return] 'newline-and-indent)

         ;; for snippets support:
         (require 'yasnippet)
         (yas/minor-mode-on)

         ;; for flymake support:
         ;;(flymake-mode)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (local-set-key "\C-c>"  'hs-hide-block)
         (local-set-key "\C-c<"  'hs-show-block)

         ;; autorevert.el is built-in to emacs; if files
         ;; are changed outside of emacs, the buffer auto-reverts.
         (turn-on-auto-revert-mode)

         ;; allow fill-paragraph to work on xml code doc
         (set (make-local-variable 'paragraph-separate)
              ;; whitespace
              ;; two or more slashes or one or more stars
              "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

         ;; never convert leading spaces to tabs:
         ;;(make-local-variable 'indent-tabs-mode)
         (setq indent-tabs-mode nil)

         ;; Sun, 20 Apr 2008  20:52
         ;; semantic stuff for code completion?
         ;;     (message "fiddling with semantic....")
         ;;      (semantic-load-enable-gaudy-code-helpers)

         ;; the above was throwing errors on Tue, 12 May 2009  03:15

         (message "dino-cpp-mode-fn: done.")
         )))


(add-hook 'c++-mode-hook 'dino-cpp-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csharp mode

(c-add-style
 "myC#Style"
 '("C#"  ; this must be defined elsewhere
   (c-basic-offset . 4)
   (c-echo-syntactic-information-p . t)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . ((c                     . c-lineup-C-comments)
                       (namespace-open        . 0)
                       (namespace-close       . 0)
                       (innamespace           . +)
                       (class-open            . 0)
                       (class-close           . 0)
                       (inclass               . +)
                       (block-open            . 0)  ;; eg, open a block under a function name or if stmt;
                                                    ;; want this to be flush with prev line.
                       (arglist-cont          . +)
                       (substatement-open     . 0)  ;; I think this is for a try {} or if{} or etc. why this is not block open, I don't know!
                       (defun-open            . 0)  ;; method defn? (but no!)
                       (defun-block-intro     . +)  ;;0 ; block within a function????
                       (inline-open           . 0)  ;; eg, opening a function? ??
                       (statement-block-intro . +)  ;; unknown what this is
                       (brace-list-open       . 0)  ;; list open (like an enum, array initializer)
                       (brace-list-intro      . +)  ;; first item in the list
                       (brace-list-entry      . 0)  ;; subsequent items in the list
                       (brace-list-close      . 0)  ;; list close
                       (cpp-macro             . (csharp-lineup-region 0))    ;; align region/endregion
                       ;;(cpp-macro             . (csharp-lineup-if-and-region 0))    ;; align region/endregion and if/else/endif
                       (statement-cont        . (dinoch-csharp-lineup-string-continuations +))
                       ))
   ))



;; Aligns long strings broken across multiple lines.
;; Also aligns attributes preceding methods or classes,
;; and aligns the lines following attributes.
;; need this in the styles list:
;;      (statement-cont . (dinoch-csharp-lineup-string-cont +))
(defun dinoch-csharp-lineup-string-continuations (langelem)
  "Like `c-lineup-string-cont' but works with csharp string continuations."
  (let ((original-point (point))
        (string-decl-regex
         (concat "\\(\\(public\\|private\\|protected\\)[ \t\n\r\f\v]+\\)?"
                 "\\(string\\|var\\)[ \t\n\r\f\v]+"
                 "\\([[:alpha:]_][[:alnum:]_]*\\)"
                 "[ \t\n\r\f\v]*"
                 "="
                 "[ \t\n\r\f\v]*"
                 "@?\""
                 ))
        tmp)

    (save-excursion

      (cond
       ;; Case 1: declaration of a literal string.
       ((progn
          (goto-char (cdr langelem))
          (looking-at string-decl-regex))
        (goto-char (1- (match-end 0)))
        (vector (current-column)))

       ;; Case 2a: declaration of a literal string, not at the top of the class.
       ;;          on the first line of the continuation, or where the previous
       ;;          line of the continuation begins with an immediate string.
       ;;
       ;; Like this:
       ;;
       ;;   void Method1()
       ;;   {
       ;;   }
       ;;
       ;;   private string Foo = "djdkdj" +
       ;;
       ;;
       ;; For some reason, cc-mode gives us the langelem that belongs to the
       ;; Method1, not to Foo.??
       ;;
       ;; So, hack it.
       ;;
       ((progn
          (goto-char original-point)
          (back-to-indentation)
          (c-backward-syntactic-ws)
          (back-to-indentation)
          (setq tmp (or
                     (looking-at string-decl-regex)
                     (looking-at "@?\""))))

        (goto-char (if (eq (char-after (match-beginning 0)) ?@)
                       (match-beginning 0)
                     (1- (match-end 0))))
        (vector (current-column)))


       ;; Case 2b: declaration of a literal string, not at the top of the class,
       ;;          where the previous line of the continuation begins with
       ;;          a symbol name or an integer.
       ;;
       ;; Like this:
       ;;
       ;;   void Method1()
       ;;   {
       ;;   }
       ;;
       ;;   private string Foo = "djdkdj" +
       ;;                        "zoweee" +
       ;;
       ((progn
          (goto-char original-point)
          (back-to-indentation)
          (c-backward-syntactic-ws)
          (back-to-indentation)
          (or
           (looking-at "\\([[:alpha:]_][[:alnum:]_]*\\)[ \t\n\r\f\v]*\\+")
           (looking-at "\\([0-9]+\\)[ \t\n\r\f\v]*\\+")))

        (goto-char (match-beginning 0))
        (vector (current-column)))


       ;; case 3: use of a literal string
       ((progn
          (goto-char (cdr langelem))
          (looking-at "@?\""))
        (goto-char (match-beginning 0))
        (vector (current-column)))

       ;;        ;; case 4: everything else
       ;;        ;; If neither matches, then check if preceding line is an attribute.
       ;;        ;; If yes, then indent properly.  If not, then return nil, so the next
       ;;        ;; line-up fn will be invoked.
       ;;        (t
       ;;
       ;;         (progn
       ;;           (goto-char original-point)
       ;;           ;; go to the indentation of the previous line
       ;;           (c-backward-syntactic-ws)
       ;;           (back-to-indentation)
       ;;           ;; is it an attribute?
       ;;           (cond
       ;;            ((looking-at "\\[")
       ;;             (vector (- (c-point 'boi) (c-point 'bol) )))
       ;;
       ;;            ;; not an attribute
       ;;            (t nil))))

       (t nil)
       ))))




;; for hideshow.el
(require 'hideshow)
(defun csharp-hs-forward-sexp (&optional arg)

  "I set hs-forward-sexp-func to this function.

I found this customization necessary to do the hide/show magic in C#
code, when dealing with region/endregion. This routine
goes forward one s-expression, whether it is defined by curly braces
or region/endregion. It handles nesting, too.

The forward-sexp method takes an arg which can be negative, which
indicates the move should be backward.  Therefore, to be fully
correct this function should also handle a negative arg. However,
the hideshow.el package never uses negative args to its
hs-forward-sexp-func, so it doesn't matter that this function does not
do negative numbers.

The arg can also be greater than 1, which means go forward
multiple times. This function doesn't handle that EITHER.  But
again, I haven't see that as a problem."

  (let ((nestlevel 0)
        (mark1 (point))
        (done nil))

    (if (and arg (< arg 0))
        ;; a negative argument; we want to back up!
        (message "negative arg (%d) is not supported..." arg)

      ;; else, we have a positive argument, hence move forward.
      ;; simple case is just move forward one brace
      (if (looking-at "{")
          (and
           (forward-sexp arg)
           )

        ;; The more complex case is dealing with a "region/endregion" block.
        ;;We have to deal with nested regions!
        (and
         (while (not done)
           (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                              (point-max) 'move)
           (cond

            ((eobp))                            ; do nothing if at end of buffer

            ((and
              (match-beginning 1)

              ;; if the match is longer than 6 chars, we know it is "endregion"
              (if (> (- (match-end 1) (match-beginning 1)) 6)
                  (setq nestlevel (1- nestlevel))
                (setq nestlevel (1+ nestlevel))
                )
              )))

           (setq done (not (and (> nestlevel 0) (not (eobp)))))

           ) ; while

         (if (= nestlevel 0)
             (goto-char (match-end 2))))))))


;; more for hideshow.el
(unless (assoc 'csharp-mode hs-special-modes-alist)
  (push '(csharp-mode

          ;; "\\(^\\s*#\\s*region\\b\\)\\|{"       ; regexp for start block
          "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"    ; regexp for start block


          ;; "\\(^\\s*#\\s*endregion\\b\\)\\|}"    ; regexp for end block
          "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}" ; regexp for end block


          "/[*/]"                                  ; regexp for comment start

          csharp-hs-forward-sexp                   ; hs-forward-sexp-func

          ;;csharp-hs-adjust-block-beginning       ; csharp adjust ?
          hs-c-like-adjust-block-beginning         ; c-like adjust (1 char)
          )
        hs-special-modes-alist))


                                        ;    (?} . ?{)
(defvar dino-skeleton-pair-alist
  '((?\) . ?\()
    (?\] . ?\[)
    (?" . ?")))


(defun dino-skeleton-pair-end (arg)
  "Skip the char if it is an ending, otherwise insert it."
  (interactive "*p")
  (let ((char last-command-char))
    (if (and (assq char dino-skeleton-pair-alist)
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg)))))


(defun dino-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (cond (window-system
         (turn-on-font-lock)
         (c-set-style "myC#Style")
         (message "setting local key bindings....")

         (local-set-key "\M-\C-R"  'indent-region)
         (local-set-key "\M-#" 'dino-indent-buffer)
         (local-set-key "\C-c\C-w" 'compare-windows)

         (local-set-key "\C-c\C-y"  'csharp-show-syntax-table-prop)
         (local-set-key "\C-c\C-h"  'csharp-show-parse-state)
         (local-set-key "\C-c\C-v" 'my-flymake-show-next-error)

         (local-set-key (kbd "C-<") 'csharp-move-back-to-beginning-of-defun)
         (local-set-key (kbd "C->") 'csharp-move-fwd-to-end-of-defun)

         ;; this works??
         (local-set-key (kbd "C-M-\<") 'csharp-move-back-to-beginning-of-class)
         (local-set-key (kbd "C-M-\>") 'csharp-move-fwd-to-end-of-class)

         ;; this is illegal, causes emacs to fail to start
         ;;(local-set-key "\M-\C-<" 'csharp-move-back-to-beginning-of-class)
         ;;(local-set-key "\M-\C->" 'csharp-move-fwd-to-end-of-class)

         ;; this messes up all the other M-?? bindings , like M-:
         ;;(local-set-key (kbd "<escape> C-<") 'csharp-move-back-to-beginning-of-class)
         ;;(local-set-key (kbd "<escape> C->") 'csharp-move-fwd-to-end-of-class)

         ;; this does nothing
         ;;(local-set-key (kbd "<meta> C-<") 'csharp-move-back-to-beginning-of-class)
         ;;(local-set-key (kbd "<meta> C->") 'csharp-move-fwd-to-end-of-class)

         ;; this also has no discernable effect
         ;;(local-set-key "C-M-\<" 'csharp-move-back-to-beginning-of-class)
         ;;(local-set-key "C-M-\>" 'csharp-move-fwd-to-end-of-class)


         (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
         ;;(local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

         ;; these allow typeover of matching brackets
         (local-set-key (kbd "\"") 'dino-skeleton-pair-end)
         (local-set-key (kbd ")") 'dino-skeleton-pair-end)
         (local-set-key (kbd "]") 'dino-skeleton-pair-end)

         ;;(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)

         ;; Default to auto-indent on Enter
         ;;(define-key csharp-mode-map [(control j)] 'newline)
         ;;(define-key csharp-mode-map [(control m)] 'newline-and-indent)

         ;;(define-key csharp-mode-map [return] 'newline-and-indent)

         ;; for skeleton stuff
         (set (make-local-variable 'skeleton-pair) t)

         (yas/minor-mode-on)
         (show-paren-mode 1)
         (hl-line-mode 1)

         (require 'flymake)
         (flymake-mode 1)
         (local-set-key "\C-c\C-n"  'flymake-goto-next-error)
         (local-set-key "\C-c\C-m"  'flymake-display-err-menu-for-current-line)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (local-set-key "\C-c>"  'hs-hide-block)
         (local-set-key "\C-c<"  'hs-show-block)

         ;; autorevert.el is built-in to emacs; if files
         ;; are changed outside of emacs, the buffer auto-reverts.
         (turn-on-auto-revert-mode)

         ;; never convert leading spaces to tabs:
         ;; (setting this variable automatically makes it local)
         (setq indent-tabs-mode nil)

         ;; ;; C# code completion
         (require 'csharp-completion)
         ;; ;;(csharp-analysis-get-analysis)

         ;; the imenu stuff doesn't perform well; impractical
         (setq csharp-want-imenu nil)

         ;; Sun, 08 Apr 2012  15:09
         ;; I had trouble setting csharp as a ac-source.
         ;; it works fine with cscomp popping the completion menu.
         ;; (require 'auto-complete-config)
         ;; (auto-complete-mode 1)
         ;; ;; require an auto-complete key, instead of
         ;; ;; doing it automatically.
         ;; (setq ac-auto-start nil)
         ;; ;;(setq ac-auto-start 2)  ;;or 3?
         ;;(local-set-key "\M-\\"   'ac-complete-csharp)

         (local-set-key "\M-\\"   'cscomp-complete-at-point)
         ;;(local-set-key "\M-\\"   'cscomp-complete-at-point-menu)
         (local-set-key "\M-\."   'cscomp-complete-at-point-menu)

         (require 'rfringe)
         (message "dino-csharp-mode-fn: done.")
         )))


;; ;; autocomplete
;; (defun ac-csharp-mode-setup ()
;;   (setq ac-sources (list 'ac-source-csharp)))
;; (add-hook 'csharp-mode-hook 'ac-csharp-mode-setup)



(eval-after-load "csharp-mode"
  '(progn
     (require 'compile)
     (add-hook  'csharp-mode-hook 'dino-csharp-mode-fn t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile
;;


;;  (progn
;;    (add-to-list
;;     'compilation-error-regexp-alist-alist
;; '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(error\\|warning\\) MSB[0-9]+:" 1 2 3 4)
;;    (add-to-list
;;     'compilation-error-regexp-alist
;;     'ms-resx)))



(eval-after-load "compile"
  '(progn

     ;; Each elt has the form (SYMBOL REGEXP FILE [LINE COLUMN TYPE
     ;; HYPERLINK HIGHLIGHT...]).  If REGEXP matches, the FILE'th
     ;; subexpression gives the file name, and the LINE'th subexpression
     ;; gives the line number.  The COLUMN'th subexpression gives the
     ;; column number on that line.

     (mapcar

      (lambda (x)
        (add-to-list 'compilation-error-regexp-alist-alist x)
        (add-to-list
         'compilation-error-regexp-alist (car x)))

      (list
       ;;    '(jslint
       ;;  "^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\): \\(.+\\)$" 1 2 3)

       ;; Microsoft VJC:
       ;;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
       '(msvjc "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) VJS[0-9]+:" 1 3 4)


       ;; Microsoft Xaml:
       ;;sample.xaml(6,1) : error J0020: Expected 'class' or 'interface'
       '(xaml "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) VJS[0-9]+:" 1 3 4)

       ;; Microsoft C/C++:
       ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
       ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
       ;;  .\cppcli1.cpp(36): error C2059: syntax error : 'public'
       ;;  e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
       ;;  myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
       ;;   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?\: \\(error\\|warning\\) C[0-9]+:" 1 3)
       '(msvc "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.\\(cpp\\|c\\|h\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\) C[0-9]+:" 1 3)


       ;; Microsoft RESX compiler
       ;;  c:\dev\XPathVisualizerTool.resx(257,5): error MSB3103: Invalid Resx file. Invalid length
       '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(warning\\) MSB[0-9]+:" 1 2 3 1)

       '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(error\\) MSB[0-9]+:" 1 2 3 2)


       ;; Candle.exe or light.exe (wix SDK)
       ;;
       ;; c:\dinoch\dev\WiX\FirstTry\First.wxs(19) : error CNDL0006 : Blah blah blah
       '(wix-candle "^[ \t]*\\([A-Za-z0-9][^\r\n](+\\.wxs\\)(\\([0-9]+\\)) ?: +\\(error\\|warning\\) \\(CNDL\\|LGHT\\)[0-9]+ *:" 1 2)


       ;; Sun javac.exe, and javadoc
       ;;
       ;; javac:
       ;; TestClient.java:222: cannot find symbol
       ;;
       ;; javadoc:
       ;; .\Message.java:110: warning - Tag @see: can't find fetch(String) in ionic.Msmq.Message
       '(javac "^\\([\.\\A-Za-z0-9][^\n:]+\\.java\\):\\([0-9]+\\): +\\([^\n]+\\)$" 1 2)

       ;; makefiles (nmake)
       ;;
       ;; makefile(136) : fatal error U1033: syntax error : ':' unexpected
       '(nmake "^\\(makefile\\)(\\([0-9]+\\)) +: +\\([^\n]+\\)$" 1 2)

       ;; elisp  (byte-compile-file)
       ;; emacs.el:53:2:Warning: `set-face-underline' is an obsolete function (as of Emacs 22.1)
       '(elisp "^\\([\.\\A-Za-z0-9][^\n:]+\\.el\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(Error\\|Warning\\): [^\n]+\\)$" 1 2 3)
       ))

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP mode
;;

(defun mark-current-word ()
  "Select the word under cursor.
'word' here is considered any alphanumeric sequence or _ .

Does not consider word syntax tables.
"
 (interactive)
 (let (pt)
   (skip-chars-backward "_A-Za-z0-9")
   (setq pt (point))
   (skip-chars-forward "_A-Za-z0-9")
   (set-mark pt)))


(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
    the lowercase version preceded by an underscore.

    The first char, if capitalized (eg, PascalCase) is just
    downcased, no preceding underscore.
    "
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))


;; (defun un-camelcase-string (s &optional sep start)
;;   "Convert CamelCase string S to lower case with word separator SEP.
;; Default for SEP is a underscore \"_\".
;;
;; If third argument START is non-nil, convert words after that
;; index in STRING."
;;   (let ((case-fold-search nil))
;;     (while (string-match "[A-Z]" s (or start 1))
;;       (setq s (replace-match (concat (or sep "_")
;;                                      (downcase (match-string 0 s)))
;;                              t nil s)))
;;     (downcase s)))


(defun dino-php-mode-fn ()
  "Function to run when php-mode is initialized for a buffer."
  (require 'flymake)
  (flymake-mode 1)

  (setq c-default-style "bsd"
        c-basic-offset 2)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-\C-C"  'un-camelcase-word-at-point)

  ;; not sure if necessary or not.
  (modify-syntax-entry ?/ ". 124b" php-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" php-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  php-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" php-mode-syntax-table)

  (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t))


(eval-after-load "php-mode"
  '(progn
     (require 'compile)
     (require 'flymake)
     (add-hook 'php-mode-hook 'dino-php-mode-fn t)))


(defvar dc-php-program "/usr/bin/php" "PHP interpreter")

(defun dino-php-flymake-get-cmdline  (source base-dir)
  "Gets the cmd line for running a flymake session in a PHP buffer.
This gets called by flymake itself."

       (dino-log "PHP" "flymake cmdline for %s" source)

        (list dc-php-program
              (list "-f" (expand-file-name source)  "-l")))


(defun dino-php-flymake-init ()
  "initialize flymake for php"
  (let ((create-temp-f 'dino-flymake-create-temp-intemp)
        ;;(create-temp-f 'flymake-create-temp-inplace)
        (use-relative-base-dir t)
        (use-relative-source t)
        (get-cmdline-f 'dino-php-flymake-get-cmdline)
        args
        temp-source-file-name)

     (dino-log "PHP" "flymake-for-php invoke...")

    (setq temp-source-file-name (flymake-init-create-temp-buffer-copy create-temp-f)

          args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))


(defun dino-php-flymake-cleanup ()
     (dino-log "PHP" "flymake-for-php cleanup...")
     (flymake-simple-cleanup) )

(eval-after-load "flymake"
  '(progn
     (if (file-exists-p dc-php-program)
         ;; 1. add a PHP entry to the flymake-allowed-file-name-masks
         (let* ((key "\\.php\\'")
                (phpentry (assoc key flymake-allowed-file-name-masks)))
           (if phpentry
               (setcdr phpentry '(dino-php-flymake-init dino-php-flymake-cleanup))
             (add-to-list
              'flymake-allowed-file-name-masks
              (list key 'dino-php-flymake-init 'dino-php-flymake-cleanup)))))))


;; ;; use PHP CodeSniffer instead of just regular PHP.exe
;; (require 'flyphpcs)
;;
;; (if (file-exists-p dc-php-program)
;;     (setq fly/phpcs-phpcs-dir "c:\\dev\\phpcs"
;;       fly/phpcs-phpexe "c:\\php\\php.exe"
;;       fly/phpcs-standard "Dino" ;; Zend, PEAR, PHPCS, etc
;;       fly/phpcs-phpinc "c:\\dev\\phplibs" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML (nxml-mode)
;;

(defun dino-xml-mode-fn ()
  (turn-on-auto-revert-mode)
  ;; for hide/show support
  (hs-minor-mode 1)
  (setq hs-isearch-open t)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\C-cn"    'sgml-name-char) ;; inserts entity ref of pressed char
  (local-set-key "\M-#"     'dino-xml-pretty-print-buffer)

  (local-set-key (kbd "C-<")  'nxml-backward-element)
  (local-set-key (kbd "C->")  'nxml-forward-element)
  (local-set-key "\C-c\C-c"  'dino-xml-comment-region)

  ;; C-M-f will jump over complete elements

  (setq nxml-sexp-element-flag t
        nxml-child-indent 2)

  ;; never convert leading spaces to tabs:
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  ;; Include single-quote as a string-quote char
  ;; without this, it was being treated as part of a word,
  ;; I guess because xml-mode is derived from text-mode where
  ;; it's an apostrophe used in contractions.
  ;; But treating it as part of a word is counter-productive in an XML buffer.
  (if (boundp 'sgml-mode-syntax-table)
      (modify-syntax-entry ?' "\"'" sgml-mode-syntax-table))

  ;; http://stackoverflow.com/questions/1931784
  ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
  (add-hook 'local-write-file-hooks
              '(lambda ()
                 (save-excursion
                   (delete-trailing-whitespace))))

  ;; when `nxml-slash-auto-complete-flag' is non-nil, get completion
  (setq nxml-slash-auto-complete-flag t)

  ;; ;;; this pair of sets almost works, except that
  ;; ;;; it un-indents the intervening XML when removing
  ;; ;;; comments.  If I remove the comment-continue thing,
  ;; ;;; then the comment-block does not get really completely removed
  ;; ;;; on uncommenting.
  ;; (set (make-local-variable 'comment-style) 'multi-line)
  ;; (set (make-local-variable 'comment-continue) " ")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (dino-set-alist-entry comment-styles                                                                      ;;
  ;;                       'multi-line                                                                         ;;
  ;;                       (list t nil nil t "One 'block' comment for all lines, end on last commented line")) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)

(add-hook 'sgml-mode-hook 'dino-xml-mode-fn)
(add-hook 'nxml-mode-hook 'dino-xml-mode-fn)


(add-to-list 'hs-special-modes-alist
             '(sgml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"                ;; regexp for comment start. (need this??)
               sgml-skip-tag-forward
               nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp mode
;;

(defun dino-elisp-mode-fn ()
  (local-set-key "\r"        'newline-and-indent)
  (local-set-key "\C-c\C-c"  'comment-region)
  (local-set-key "\M-\C-R"   'indent-region)
  (local-set-key "\C-ce"     'eval-buffer)
  (local-set-key "\C-c\C-e"  'eval-region)
  (local-set-key "\C-x\C-e"  'byte-compile-file)

  ;; never convert leading spaces to tabs:
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (hl-line-mode 1)
  (turn-on-auto-revert-mode)

  ;; This write-contents-functions hook seems awesome except it wasn't
  ;; working for me.  It's possible that it was not working because of a
  ;; side-effect of the markdown fn (dino-do-markdown), which I created
  ;; to prevent the deletion of trailing whitespace in markdown
  ;; buffers. I don't have time for all this nonsense.

  ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
  (add-hook 'local-write-file-hooks
              '(lambda ()
                 (save-excursion
                   (delete-trailing-whitespace))))
  )


(add-hook 'emacs-lisp-mode-hook 'dino-elisp-mode-fn)

;; This is for scratch buffer
(add-hook 'lisp-interaction-mode-hook 'dino-elisp-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(defun dino-python-mode-fn ()

  (local-set-key "\M-\C-R" 'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-c"  'comment-region)

   ;; python-mode resets \C-c\C-w to  `python-check'.  Silly.
  (local-set-key "\C-c\C-w"  'compare-windows)

  (set (make-local-variable 'indent-tabs-mode) nil)

  (require 'autopair)
  (autopair-mode)

  ;; ya-snippet
  (yas/minor-mode-on)

  ;; use flymake with pyflakes
  (require 'flymake)
  (flymake-mode 1)
  (local-set-key "\C-c\C-n"  'flymake-goto-next-error)
  (local-set-key "\C-c\C-m"  'flymake-display-err-menu-for-current-line)

  (show-paren-mode 1))

(add-hook 'python-mode-hook 'dino-python-mode-fn)


(eval-after-load "flymake"
  '(progn
     (defun dino-flymake-pyflakes-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))

         (list "c:\\Python27\\pyflakes.cmd" (list local-file))))

     (let* ((key "\\.py\\'")
            (pyentry (assoc key flymake-allowed-file-name-masks)))
       (if pyentry
           (setcdr pyentry '(dino-flymake-pyflakes-init))
         (add-to-list
          'flymake-allowed-file-name-masks
          (list key 'dino-flymake-pyflakes-init))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Macro counters
;;
;; see more on http://www.emacswiki.org/emacs/EmacsKeyboardMacroCounter
(defun init-macro-counter-default ()
  "Set the initial counter to 1 and reset every time it's called.
To set to a different value call `kmacro-set-counter' interactively
i.e M-x kmacro-set-counter."
  (interactive)
  (kmacro-set-counter 1))

(global-set-key (kbd "<f5>") 'init-macro-counter-default)
(global-set-key (kbd "<f6>") 'kmacro-insert-counter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
(autoload 'js-mode "js" nil t)
;;(autoload 'js2-mode "js2" nil t)

(defun dino-javascript-mode-fn ()
  (turn-on-font-lock)
  (local-set-key "\M-\C-R" 'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-c"  'comment-region)

  (set (make-local-variable 'indent-tabs-mode) nil)

  ;; indent increment
  (setq js-indent-level 2)

  ;; interactive javascript shell
  ;;(local-set-key "\C-x\C-e" 'jsshell-send-last-sexp)
  (local-set-key "\C-\M-x"  'jsshell-send-last-sexp-and-pop)
  (local-set-key "\C-cb"    'jsshell-send-buffer)
  (local-set-key "\C-c\C-b" 'jsshell-send-buffer-and-pop)
  (local-set-key "\C-cl"    'jsshell-load-file-and-pop)
  (local-set-key "\C-c\C-e" 'jsshell-send-region)

  (linum-on)

  ;; use autopair for curlies, parens, square brackets.
  ;; electric mode doesn't provide auto-typeover of the ending char
  (require 'autopair)
  (autopair-mode)

  ;; turn on flymake
  (require 'flymake)
  (local-set-key "\C-c\C-n"  'flymake-goto-next-error)
  (local-set-key "\C-c\C-m"  'flymake-display-err-menu-for-current-line)

  ;; (require 'flymake-for-jslint-for-wsh)
  ;; ;;(setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jslint-for-wsh.js")
  ;; (setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jshint-for-wsh.js")

  (require
   (if (eq system-type 'windows-nt)
       'fly-jshint-wsh 'fly-jshint-npm))

  ;;(setq fly/jshint/npm-jshint-exe "/usr/local/bin/jshint")

  ;; ;;(setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jslint-for-wsh.js")
  ;; (setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jshint-for-wsh.js")
  (flymake-mode 1)

  ;; ya-snippet
  ;;(add-to-list 'yas/known-modes 'espresso-mode) ;; need this?
  (add-to-list 'yas/known-modes 'js-mode) ;; need this?
  (yas/minor-mode-on)

  (add-hook 'local-write-file-hooks
              '(lambda ()
                 (save-excursion
                   (delete-trailing-whitespace))))

  (require 'imenu)
  (imenu-add-menubar-index)

  (require 'hideshow)
  (hs-minor-mode t)

  (require 'smart-op) ;; for smart insertion of ++ and == and += etc
  (smart-op-mode)

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

;; ;; to allow jshint to work?
;;(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")
;;     "/usr/bin"

(add-hook 'js-mode-hook   'dino-javascript-mode-fn)

(require 'js-mode-fixups)
(require 'json-reformat)

;; function alias
(defalias 'json-prettify-region 'json-reformat-region)

(defun json-prettify-buffer ()
  "prettifies a json buffer."
  (interactive)
  (save-excursion
    (json-prettify-region (point-min) (point-max))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;(require 'aspx-mode)
(autoload  'aspx-mode "aspx-mode" nil t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java

(defun dino-java-mode-fn ()
  (c-set-style "myJavaStyle")
  (turn-on-font-lock)
  (local-set-key "\M-\C-R" 'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)

  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'skeleton-pair) t)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))

(add-hook 'java-mode-hook 'dino-java-mode-fn)


(c-add-style "myJavaStyle"
             '("Java"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 4)
               (c-echo-syntactic-information-p . t)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . (
                                   (c                     . c-lineup-C-comments)
                                   (statement-case-open   . 0)
                                   (case-label            . +)
                                   (substatement-open     . 0)
                                   ))
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile stuff

(require 'smarter-compile)

(setq smart-compile-alist
      (append
       '(
         ("\\.wxs\\'"      . "%M %n.msi")
         ("\\.css\\'"      . "~/js/csslint.node.js %f")
         ("\\.js\\'"       . "~/js/jshint.node.js %f")
         ) smart-compile-alist ))

(eval-after-load "compile"
  '(progn
     (setq compilation-scroll-output "first-error")
     (setq-default compile-command (concat nmake.exe " "))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode

(defalias 'perl-mode 'cperl-mode)
(require 'cperl-mode)

(autoload 'perl-mode
  "/Applications/Emacs.app/Contents/Resources/lisp/progmodes/cperl-mode" "" t)
;;(autoload 'perl-mode "/emacs/lisp/progmodes/cperl-mode" "" t)
;;(autoload 'perl-mode "c:/emacs/lisp/progmodes/perl-mode" "" t)

;;(setq cperl-comment-column   44)                 ; must this be set globally?

(defconst my-cperl-style
  '( ("MyPerl"
      (cperl-indent-level               .  0)
      (cperl-brace-offset               .  2)
      (cperl-continued-statement-offset .  2)
      (cperl-continued-brace-offset     . -2)
      (cperl-label-offset               . -2)
      (cperl-close-paren-offset         . -1))))


(defun cperl-mode-hook-fn ()
  "My hook for perl mode"
  (set-variable 'c-indent-level 0)      ; required for c-outline
  (turn-on-font-lock)
  (set (make-local-variable 'cperl-style-alist) 'my-cperl-style))


(add-hook 'cperl-mode-hook 'cperl-mode-hook-fn)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'ediff)
(autoload 'ediff-buffers "ediff" nil t)


(defun dino-ediff-buffer-against-file (file)
  "diff the current [edited] buffer and the file of the same name"
  (interactive
   (list (ediff-read-file-name
          "File to compare:" default-directory buffer-file-name)))
  (let ((buf-buf-name (buffer-name))
        (file-buf-name (create-file-buffer file)))
    (with-current-buffer file-buf-name
      (insert-file-contents file t nil nil t))
    (ediff-buffers buf-buf-name file-buf-name)))



;; (defun diff (old new &optional switches no-async)
;;   "Find and display the differences between OLD and NEW files.
;; Interactively the current buffer's file name is the default for NEW
;; and a backup file for NEW is the default for OLD.
;; If NO-ASYNC is non-nil, call diff synchronously.
;; With prefix arg, prompt for diff switches."
;;   (interactive
;;    (let (oldf newf)
;;      (setq newf (buffer-file-name)
;;         newf (if (and newf (file-exists-p newf))
;;                  (read-file-name
;;                   (concat "Diff new file (default "
;;                           (file-name-nondirectory newf) "): ")
;;                   nil newf t)
;;                (read-file-name "Diff new file: " nil nil t)))
;;      (setq oldf (file-newest-backup newf)
;;         oldf (if (and oldf (file-exists-p oldf))
;;                  (read-file-name
;;                   (concat "Diff original file (default "
;;                           (file-name-nondirectory oldf) "): ")
;;                   (file-name-directory oldf) oldf t)
;;                (read-file-name "Diff original file: "
;;                                (file-name-directory newf) nil t)))
;;      (list oldf newf (diff-switches))))
;;   (setq new (expand-file-name new)
;;      old (expand-file-name old))
;;   (or switches (setq switches diff-switches)) ; If not specified, use default.
;;   (let* ((old-alt (file-local-copy old))
;;      (new-alt (file-local-copy new))
;;       (command
;;        (mapconcat 'identity
;;                   `(,diff-command
;;                     ;; Use explicitly specified switches
;;                     ,@(if (listp switches) switches (list switches))
;;                     ,@(if (or old-alt new-alt)
;;                           (list "-L" old "-L" new))
;;                     ,(shell-quote-argument (or old-alt old))
;;                     ,(shell-quote-argument (or new-alt new)))
;;                   " "))
;;       (buf (get-buffer-create "*Diff*"))
;;       (thisdir default-directory)
;;       proc)
;;     (save-excursion
;;       (display-buffer buf)
;;       (set-buffer buf)
;;       (setq buffer-read-only nil)
;;       (buffer-disable-undo (current-buffer))
;;       (let ((inhibit-read-only t))
;;      (erase-buffer))
;;       (buffer-enable-undo (current-buffer))
;;       (diff-mode)
;;       (set (make-local-variable 'revert-buffer-function)
;;         `(lambda (ignore-auto noconfirm)
;;            (diff ',old ',new ',switches ',no-async)))
;;       (set (make-local-variable 'diff-old-temp-file) old-alt)
;;       (set (make-local-variable 'diff-new-temp-file) new-alt)
;;       (setq default-directory thisdir)
;;       (let ((inhibit-read-only t))
;;      (insert command "\n"))
;;       (if (and (not no-async) (fboundp 'start-process))
;;        (progn
;;          (setq proc (start-process "Diff" buf shell-file-name
;;                                    shell-command-switch command))
;;          (set-process-filter proc 'diff-process-filter)
;;          (set-process-sentinel
;;           proc (lambda (proc msg)
;;                  (with-current-buffer (process-buffer proc)
;;                    (diff-sentinel (process-exit-status proc))))))
;;      ;; Async processes aren't available.
;;      (let ((inhibit-read-only t))
;;        (diff-sentinel
;;         (call-process shell-file-name nil buf nil
;;                       shell-command-switch command)))))
;;     buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random stuff
;;

;; The following function (scarfed off an emacs bboard) will allow you to
;; see what emacs is seeing when you press any key.
(defun dino-see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
        (inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
            quit-flag nil))         ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))


(require 'lorem)

(defun mspl ()
  "inserts MS-PL text at point"
  (interactive)
  (insert-file-contents "c:\\users\\dino\\Documents\\MS-PL.txt"))

(defun bsd ()
  "inserts New BSD text at point"
  (interactive)
  (insert-file-contents "c:\\users\\dino\\Documents\\BSD.txt"))


;08.04.2003: Kai Großjohann
(defun increment-number-at-point (amount)
  "Increment number at point by given AMOUNT."
  (interactive "NIncrement by: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (old-num (number-at-point)))
    (unless old-num
      (error "No number at point"))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%d" (+ old-num amount)))))


(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals '())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))


(defun open-in-finder ()
  "Open current folder in Finder. Works in dired mode."
  (interactive)
  (shell-command "open ."))
(global-set-key (kbd "<f8>") 'open-in-finder)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable tramp?
;;

(defun dino-disable-tramp ()
  "This function tries to disable tramp.

It does this by doctoring the variable `file-name-handler-alist'
to remove all tramp symbols in that alist. That list is used by
emacs to connect to logic that handles filenames of various
forms.  The list associates a regex with a function that handles
filenames that match the regex.  When tramp loads, it injects
pairs into that alist, so that filenames that look like URLs can
be loaded via tramp magic.  Removing the tramp pairs unhooks
tramp from the file name handling logic.

Why would anyone want to do this?  I'll tell you: I've found
tramp to be worse than useless on Windows, always turning on for
reasons that are not apparent to me, providing no discernable
utility, and interfering with normal, expected operation.

When I use emacs to open a file that resides on a mapped
drive (for example, G:\), tramp pipes in and begins trying to
help.  It then messes up dired, somehow, so that I can no longer
navigate in directories that are on mapped drives. Confoundingly,
it also prevents me from closing buffers, including dired buffers
and file buffers that are open on mapped drives. Why or how it
would do this, I don't know, and I don't care to spend the time
finding out. I think the basic problem is that the tramp regexes
are broken, but I'm not sure why g:\ would be treated any
differently than c:\.  In any case it isn't worth my time to find
out.

All these problems happened on Windows, but I still have weird
behavior from tramp on MAcOS.  I have no idea why, but it's very
unpleasant and requires me to stop and restart emacs
periodically, because tramp goes haywire. Keep in mind that I
never purposely invoke tramp.  I suppose sometimes I fat-finger
something and it causes tramp to wake up and go crazy. It
perpetually generates errors complaining about tramp-ftp-method
being an unknown variable. WTF? It's a scourge.

Tramp is documented as providing the ability to do remote file
editing, via things like rsh/rcp and ssh/scp.  I don't want or
need that.  I'll map my own drives, thanks, and I don't need
emacs doing it for me.
"
  (interactive)
  (let (new-alist)
    (dolist (pair file-name-handler-alist)
      (let ((sym (cdr pair)))
        (if (string-match "^tramp-" (symbol-name sym))
            (message "removing: %s" sym)
          (message "keeping: %s" sym)
          (setq new-alist
                (cons pair new-alist)))))
    (setq file-name-handler-alist (reverse new-alist)))
  (tramp-unload-tramp)) ;;; please! go away!

;; not sure this really works
(eval-after-load "tramp" '(dino-disable-tramp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url
;;
;; ;; If you want emacs to use the system proxy, you need to do
;; ;; this:
;; ;;; set up the proxy
;; (setq url-using-proxy t)
;; (setq url-proxy-services
;; '(("http" . "localhost:8888")))
;;
;; To make this happen automatically, read the registry before
;; each URL retrieval, and set the proxy appropriately.
;;
(if (eq system-type 'windows-nt)
    (eval-after-load "url"
      '(progn
         (require 'w32-registry)
         (defadvice url-retrieve (before
                                  dino-set-proxy-dynamically
                                  activate)
           "Before retrieving a URL, query the IE Proxy settings, and use them."
           (let ((proxy (w32reg-get-ie-proxy-config)))
             (setq url-using-proxy proxy
                   url-proxy-services proxy))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ------------------------------------------------------------------
;;
;; use C-x C-r to open "Recent files"
;;
;; ------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-string fname tocpl)))))
(global-set-key [(control x)(control r)] 'recentf-open-files-compl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thesaurus
;;
(require 'thesaurus)
(thesaurus-set-bhl-api-key-from-file "~/BigHugeLabs.apikey.txt")
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelchek
;;
(require 'spelchek)
(define-key global-map (kbd "C-x c") 'spelchek-choose-alternative-and-replace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wordnik (dictionary)
;;
(require 'wordnik)
(wordnik-set-api-key-from-file "~/wordnik.apikey.txt")
(define-key global-map (kbd "C-c ?") 'wordnik-show-definition)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Before running an emacsclient, need to set the environment variable.
;; set EMACS_SERVER_FILE=c:\users\dino\elisp\.emacs.d\server\server
(require 'server)
(if (not (eq t (server-running-p server-name)))
    (server-start))

;; For some reason, the font-face reverts during load of various elisp
;; libraries above.  So I set it again, here.

(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-face-background 'region "grey6") ;; very subtle

(setq line-move-visual t    ;; ??
      line-number-mode t    ;; modeline
      column-number-mode t) ;; modeline

;; do I want this to be disabled?
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-localized-time-format t)
 '(temporary-file-directory "/tmp"))

;; auto-revert for all files.
(add-hook 'find-file-hook
              (lambda () (turn-on-auto-revert-mode)))

(message "Done with emacs.el...")

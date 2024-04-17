;;; emacs.el -- Dino's .emacs setup file.
;;
;; Last saved: <2024-April-10 16:22:33>
;;
;; Works with v24.5 and v25.1 of emacs.
;;

;;; Commentary:

;;; Code:
(message "Running emacs.el...")

(setq inhibit-splash-screen t)
(setq visible-bell nil) ;; quiet, please! No dinging!
(setq ring-bell-function `(lambda ()
                            (set-face-background 'default "DodgerBlue")
                            (set-face-background 'default "black")))

(setq scroll-error-top-bottom t) ;; move cursor when scrolling not possible
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; we don't need no steenking icons
(setq user-mail-address "dpchiesa@hotmail.com")
(setq comment-style 'indent) ;; see doc for variable comment-styles
(setq Buffer-menu-name-width 40)

;; for tetris
(and (boundp 'tetris-score-file)
     (setq tetris-score-file "~/elisp/tetris-scores")
     (defadvice tetris-end-game (around zap-scores activate)
       (save-window-excursion ad-do-it)))


;; To change the coding system of a visited file,
;; `C-x RET r utf-8-with-signature RET'.
;;
;; Try  M-x list-coding-systems   ... to see a list
;;
(if (boundp 'utf-8-auto)
    (prefer-coding-system 'utf-8-auto)) ;; unicode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directory to load additional libraries from :

(add-to-list 'load-path "~/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a bunch of random utility functions
;;
(require 'dino-utility)
(add-hook 'before-save-hook 'dino-untabify-maybe)
(global-set-key "\C-x7"     'dino-toggle-frame-split)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;
(require 'package)
(dolist (item (list
               ;; '("MELPA Stable"     . "https://stable.melpa.org/packages/")
              '("MELPA"     . "https://melpa.org/packages/")
              '("org"              . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives item))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; set load-path for elpa things. Some of these may take precedence
;; over builtin packages.  Eg org mode, which gets updated more
;; frequently than emacs.
;;
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;; Maybe check out https://github.com/jwiegley/use-package?
;; My own implementation is used here.
;; Simply add package names to the list.
(dino-ensure-package-installed
 'apheleia        ;; clever reformatting engine
 'company         ;; COMPlete ANYthing
 'company-box     ;; i guess this makes the popup a little nicer?
 'auto-complete   ;; i should probably convert to company. I think this is legacy now.
 ;; 'company-go
 'dash
 'dash-functional
 'default-text-scale
 'expand-region
 'flycheck
 ;; 'go-autocomplete
 'go-mode
 'dart-mode
 'js2-mode
 'js2-refactor
 'json-mode
 'logito ;; a tiny logging framework for emacs. Not sure where this is used
 'lsp-mode
 'magit
 'markdown-mode
 'path-helper
 'popup
 's
 'seq
 'typescript-mode
 'yasnippet
 'yaxception
 )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; set path correctly on MacOS, based on /etc/paths
    (if (memq window-system '(ns mac))
      (path-helper-setenv "PATH"))


;;(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;;(setq exec-path (split-string (getenv "PATH") ":"))
;;(add-to-list 'exec-path "/usr/local/bin")
;;     "/usr/bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'default-text-scale)
(default-text-scale-mode)
(global-set-key (kbd "C-=") 'default-text-scale-increase)
(global-set-key (kbd "C--") 'default-text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The magit doc says setting this var directly is a kludge, should rely on
;; $PATH.  but it works for me.;; (--first is from the dash.el module)

(require 'dash)
(setq magit-git-executable
      (--first
       (file-exists-p it)
       '("c:/Program Files/Git/cmd/git.exe"
          "/usr/local/git/current/bin/git")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck, always
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load "flycheck"
  '(progn
     (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
     ;; (setq flycheck-phpcs-standard "Drupal") ;; DinoChiesa
     (setq flycheck-phpcs-standard "/usr/local/share/pear/PHP/CodeSniffer/src/Standards/DinoChiesa")
     )) ;; DinoChiesa

;; $ pear config-get php_dir
;; will show the pear PHP config directory.  Eg /usr/local/share/pear
;; .. which means the phpcs dir is /usr/local/share/pear/PHP/CodeSniffer
;; .. and the standards are in     /usr/local/share/pear/PHP/CodeSniffer/src/Standards
;;
;; ?? or this?
;; $ php composer.phar global show -P
;; which shows
;; ~/.composer/vendor/squizlabs/php_codesniffer/CodeSniffer/Standards/
;;
;; regardless, wherever the Standards live, you can add a new one.
;;   cd   ${phpcs_dir}/Standards
;;   mkdir DinoChiesa
;;
;; then, place the following content in DinoChiesa/ruleset.xml
;;
;; <ruleset name="Custom Standard">
;;   <description>My custom coding standard</description>
;;   <rule ref="PEAR">
;;     <exclude name="PEAR.WhiteSpace.ScopeClosingBrace.BreakIndent"/>
;;     <exclude name="PEAR.WhiteSpace.ScopeIndent"/>
;;     <exclude name="Generic.PHP.LowerCaseConstant.Found"/>
;;     <exclude name="PEAR.NamingConventions.ValidFunctionName.FunctionNoCapital"/>
;;     <exclude name="Generic.Commenting.DocComment.MissingShort"/>
;;     <exclude name="PEAR.Commenting.ClassComment"/>
;;     <exclude name="PEAR.Commenting.FileComment"/>
;;     <exclude name="PEAR.Commenting.FunctionComment"/>
;;     <exclude name="PEAR.Commenting.InlineComment"/>
;;     <exclude name="PEAR.Classes.ClassDeclaration"/>
;;     <exclude name="PEAR.Functions.FunctionDeclaration.BraceOnSameLine"/>
;;     <exclude name="PEAR.Functions.FunctionCallSignature.ContentAfterOpenBracket" />
;;     <exclude name="Generic.Files.LineEndings"/>
;;     <exclude name="Generic.Files.LineLength.TooLong"/>
;;     <exclude name="PEAR.ControlStructures.ControlSignature"/>
;;     <exclude name="PEAR.Functions.FunctionCallSignature.CloseBracketLine"/>
;;     <exclude name="PEAR.Functions.FunctionCallSignature.Indent"/>
;;   </rule>
;;   <rule ref="PEAR.WhiteSpace.ScopeIndent">
;;     <properties>
;;       <property name="indent" value="2"/>
;;     </properties>
;;   </rule>
;; </ruleset>


;; for all modes
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

;;; Tips from https://www.youtube.com/watch?v=p3Te_a-AGqM
;; for marking ever-larger regions iteratively
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; for visually editing similar things with one key sequence
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; edit files in a grep output buffer
(require 'wgrep)
(global-set-key (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff for Apigee
;;
;; (require 'apigee)
;; (setq apigee-apiproxies-home "~/dev/apiproxies/")

;; with apigee.el in elisp/apigee/apigee.el
;; M-x load-library RET "apigee/apigee" RET

(eval-after-load "apigee"
  '(progn
     ;;(debug)
     (setcar (alist-get 'apigeecli apigee-programs-alist)
         "~/.apigeecli/bin/apigeecli")
     (setcar (alist-get 'apigeelint apigee-programs-alist)
         "node ~/dev/apigeelint/cli.js")
     (setcar (alist-get 'gcloud apigee-programs-alist)
         "~/google-cloud-sdk/bin/gcloud")
     (setcar (alist-get 'lint apigee-commands-alist)
             "%apigeelint -s ./apiproxy -e TD002,TD004 -f visualstudio.js")
     (setq apigee-environment (getenv "ENV")
           apigee-organization (getenv "ORG"))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apheleia - clever code reformatting for multiple languages.
;;
;; https://github.com/radian-software/apheleia
;; It runs code formatters on after-save-hook, and then resaves only if there
;; are changes. It preserves cursor location across these changes. Sweet.

(require 'apheleia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh-mode
(setcar (alist-get 'shfmt apheleia-formatters)
         "~/go/bin/shfmt")
(push '(sh-mode . shfmt) apheleia-mode-alist)

(defun dino-sh-mode-fn ()
  (display-line-numbers-mode)
  (sh-electric-here-document-mode)
  ;; 20230918-1015
  (apheleia-mode)
)
(add-hook 'sh-mode-hook 'dino-sh-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang
;;
;; (add-to-list 'load-path "/usr/local/go/misc/emacs") ;; removed in golang 1.4
;; (add-to-list 'load-path "~/elisp/go-mode.el")
;; (add-to-list 'load-path "~/.emacs.d/elpa/go-mode-1.4.0")

;;(require 'go-mode-autoloads) ;; editing mode
(load "go-mode-autoloads") ;; editing mode; there is no provide statement!
;; for flycheck or compile support, I need the go binary on the path
(setenv "GOPATH" "/Users/dchiesa/dev/go")

(defun dino-go-mode-fn ()
  ;;(setq-default)
  (setq tab-width 2
        standard-indent 2
        indent-tabs-mode t) ;; golang prefers tabs, ugh

  (require 'go-autocomplete)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-w" 'compare-windows)
  (local-set-key "\C-c\C-c"  'comment-region)

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".go")))

    (eval-after-load "flycheck"
    '(progn
       (add-to-list
        'flycheck-disabled-checkers 'go-build))) ;; go-gofmt?

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)

  (display-line-numbers-mode)

  ;; 20230918-1015
  (apheleia-mode)

  (require 'goflycheck)
  (flycheck-mode 1)

  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local)
  )

(add-hook 'go-mode-hook 'dino-go-mode-fn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode, including html5 presentations from .org documents
;;
;;  (require 'org)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (sh . t)
;;    (python . t)
;;    (perl . t)
;;    ))
(defun my-org-confirm-babel-evaluate (lang body)
  (not  ; don't ask for any of the following languages
   (or
    (string= lang "sh")
    )))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; (require 'ox-taskjuggler)
;; (add-to-list 'org-export-backends 'taskjuggler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; including html5 presentations from .org documents
;;

;; 2023 Feb 26 TODO -  remove ox-reveal.el, it is broken
;; my own home-built thing. Not quite as cool as Org-export with reveal.js .
;; (require 'dpreso)
;;(require 'ox-reveal)
;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
;; to use: M-x org-reveal-export-to-html

(setq org-reveal-root "http://dinochiesa.github.io/rv/")

(require 'org-fixups)
(add-hook 'org-fixups/after-export-reveal-file
          (lambda (filename)
            (message "the file was exported to %s" expanded-filename)))

(add-hook 'org-fixups/after-export-reveal-file
          'my-copy-and-open)

(defun my-copy-and-open (filename)
  "fn to copy a file and open it in the browser."
  (let* ((base-fname
          (file-name-nondirectory filename))
         (new-fname
          (concat "/Users/dchiesa/dev/html/dpreso/" base-fname)))

    (rename-file filename new-fname t)
    (call-process "open" nil t t
                  (concat "http://localhost:80/html/dpreso/"
                          base-fname))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WSD mode
;;
(require 'wsd-mode)


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
;; yasnippet
;;

;; claim: this must be added before auto-complete-config
(require 'yasnippet)
(setq yas-snippet-dirs (list "~/elisp/yasnippets"))
(yas-global-mode 1)
;; (setq yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))
;; prettier popup choices when running in a windowed environment:
(setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

;;(yas-load-directory (car yas-snippet-dirs))
;; ------------------------------------------------
;;;; Expand snippet synchronously
(defvar yas--recursive-edit-flag nil)

(defun yas-expand-sync ()
  "Execute `yas-expand'. This function exits after expanding snippet."
  (interactive)
  (let ((yas--recursive-edit-flag t))
    (call-interactively 'yas-expand)
    (recursive-edit)))

(defun yas-expand-snippet-sync (content &optional start end expand-env)
  "Execute `yas-expand-snippet'. This function exits after expanding snippet."
  (let ((yas--recursive-edit-flag t))
    ;;(sit-for 0.6) ;; timing issue?
    (yas-expand-snippet content start end expand-env)
    ;;(sit-for 0.2) ;; timing issue?
    (recursive-edit)))
(defun my-yas-after-exit-snippet-hook--recursive-edit ()
  (when yas--recursive-edit-flag
    (throw 'exit nil)))
(add-hook 'yas-after-exit-snippet-hook 'my-yas-after-exit-snippet-hook--recursive-edit)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete

;; (add-to-list 'load-path "/Users/Dino/elisp/autocomplete")
(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/Users/Dino/elisp/autocomplete/ac-dict")
(ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; httpget
(require 'httpget)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word-count minor mode
(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;
(global-set-key "\C-xw" 'dino-fixup-linefeeds)
(global-set-key "\C-cg" 'httpget)
(global-set-key "\C-cu" 'dino-insert-uuid)
(global-set-key "\C-cf" 'dino-insert-filename)
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
(global-set-key "\C-x&"     'dino-encode-uri-component-in-region)
(global-set-key "\C-xx"     'copy-to-register)
(global-set-key "\C-xg"     'insert-register)
(global-set-key "\C-xp"     'previous-window)
(global-set-key "\C-x\C-p"  'previous-window)
(global-set-key "\C-c\C-x\C-c"  'calendar)
(global-set-key "\C-xn"     'other-window)
(global-set-key "\C-x\C-e"  'smarter-compile)
(global-set-key "\C-xE"     'smarter-compile-run)
(global-set-key "\C-x\C-g"  'auto-fill-mode)
(global-set-key "\C-x\C-n"  'next-error)
;(global-set-key "\C-xt"     'dino-toggle-truncation)
(global-set-key "\M-\C-y"   'yank-pop)
(global-set-key "\M-g"      'goto-line)
(global-set-key "\M- "      'set-mark-command)
(global-set-key "\M-\C-h"   'backward-kill-word)
(global-set-key "\C-c\C-c"  'center-paragraph)  ; good for text mode
(global-set-key "\C-ck"     'global-set-key)
(global-set-key "\C-cs"     'search-forward-regexp)
(global-set-key "\C-cy"     'display-line-numbers-mode)
;;(global-set-key "\C-c\C-p"  'dino-copy-value-from-key-into-killring)
;;(global-set-key "\C-cm"     'dino-gtm-url)
(global-set-key "\C-cq"     'query-replace)
(global-set-key "\C-cc"     'goto-char)
(global-set-key "\C-cr"     'replace-regexp)
(global-set-key "\C-xt"     'dino-insert-timeofday)
;;(global-set-key "\C-c\C-t"  'dino-insert-timestamp)
(global-set-key "\C-c\C-t"  'dino-toggle-frame-split)
(global-set-key "\C-cw"     'where-is)
(global-set-key "\C-c\C-w"  'compare-windows)
(global-set-key "\C-c~"     'revert-buffer-unconditionally)
(global-set-key "\C-x~"     'dino-toggle-buffer-modified)
(global-set-key (kbd "C-<") 'beginning-of-defun)
(global-set-key (kbd "C->") 'end-of-defun)


;; unicode helpers
(define-key key-translation-map (kbd "\C-x 8 i") (kbd "∞")) ;; infinity
(define-key key-translation-map (kbd "\C-x 8 y") (kbd "λ")) ;; lambda
(define-key key-translation-map (kbd "\C-x 8 a") (kbd "α")) ;; alpha
(define-key key-translation-map (kbd "\C-x 8 b") (kbd "β")) ;; beta
(define-key key-translation-map (kbd "\C-x 8 d") (kbd "Δ")) ;; delta
(define-key key-translation-map (kbd "\C-x 8 m") (kbd "µ")) ;; mu / micro
(define-key key-translation-map (kbd "\C-x 8 e") (kbd "ε")) ;; epsilon
(define-key key-translation-map (kbd "\C-x 8 p") (kbd "π")) ;; pi


;;the help key is assigned to Ctrl-\, or Esc-Ctrl-\
(global-set-key "\M-\C-\\"   'help-for-help)
(global-set-key "\C-\\"      'help-for-help)

(require 'skeleton)

;; handle text end-of-line conventions the way it oughta be:
(setq inhibit-eol-conversion nil)

;; turn on font-lock globally
;; for fontification in emacs progmodes:
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1) ;;  'ON

(setq-default fill-column 80)
(setq auto-save-interval 500)
(setq case-fold-search nil)
(setq comment-empty-lines t)

;; fringe (between line numbers and buffer text)
(setq-default left-fringe-width  10)
(set-face-attribute 'fringe nil :background "black")

;; helpful for debugging lisp code:
(setq messages-buffer-max-lines 2500)
(setq completion-auto-help nil)
(put 'eval-expression 'disabled nil)

;; set truncation on side-by-side windows to nil.
(setq truncate-partial-width-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames preferences: initial and default frames
;; see http://www.gnu.org/software/emacs/windows/big.html#windows-frames
;;(setq dino-name-of-preferred-font "-*-Consolas-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")

(setq default-frame-alist
      '((top . 320) (left . 460)
        (width . 100) (height . 25)
        (cursor-color . "Orange")
        (cursor-type . box)
        ;;(foreground-color . "White")
        ;;(background-color . "Black")
        (mouse-color . "sienna3")
        ;; works with macos
        (font . "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
        ;; works with windows
        ;;(font . "-*-Consolas-normal-normal-normal-*-20-*-*-*-c-*-iso8859-1")
        ;; The font spec is so intuitive! To inquire the current font spec, open a text file, then C-u C-x =
        )
      )

;; works with Windows
;;(set-face-attribute 'default t :font "Consolas-11")

;; (message (face-font 'tooltip))
;; (message (face-font 'default))
;; (set-face-font 'tooltip "-outline-Lucida Sans Typewriter-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1")
;; (tooltip-show "This is a tooltip, in the new font\nthat I set in elisp.")


;; (if (eq system-type 'windows-nt)
;;   (set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))
;;
;; (if (eq window-system 'x)
;;   (set-default-font "Inconsolata-11"))


;; initial frame will be 128 wide x 68 high, also specify frame position.
(setq initial-frame-alist
      '( (top . 80) (left . 840)
         (width . 128) (height . 68)
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
;; web

(require 'web-mode)
(defun dino-web-mode-fn ()
  "My hook for web mode"
  (turn-on-font-lock)
  ;;; minor-mode
  ;;(hs-minor-mode 1)
  (display-line-numbers-mode)
  ;; why I have to re-set this key is baffling to me.
  ;; and this does not seem to work...
  (local-set-key "\M-\C-R"  'indent-region)
  ;; Make sure autofill is OFF.
  (auto-fill-mode -1))

(add-hook 'web-mode-hook 'dino-web-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown

(require 'markdown-mode)
(defun dino-markdown-mode-fn ()
  "My hook for markdown mode"
  (modify-syntax-entry ?_ "w")
  (auto-fill-mode -1))

(add-hook 'markdown-mode-hook 'dino-markdown-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustment to mode mappings
;
;; NB: In the regexp's, the trailing \\' represents "end of string".
;; The $ represents the zero-width place before newline.  They are
;; equivalent unless there is a filename with a new line in it (not
;; likely).
;;

(setq auto-mode-alist
      (append
       '(
         ("\\.yaml$"                          . yaml-mode)
         ("\\.\\(war\\|ear\\|WAR\\|EAR\\)\\'" . archive-mode)        ; java archives
         ;;("\\.s?html?\\'"                     . nxhtml-mumamo-mode)
         ("\\(Iirf\\|iirf\\|IIRF\\)\\(Global\\)?\\.ini$"   . iirf-mode)
         ("\\.css$"                           . css-mode)
         ("\\.proto$"                         . protobuf-mode)
         ("\\.\\(php\\|module\\)$"            . php-mode)
         ("\\.md$"                            . markdown-mode)
         ("\\.cs$"                            . csharp-mode)
         ("\\.asp$"                           . html-mode)
         ;;("\\.aspx$"                        . html-helper-mode)
         ("\\.aspx$"                          . aspx-mode)
         ("\\.ashx$"                          . csharp-mode)
         ("\\.ascx$"                          . csharp-mode)
         ("\\.s?html?\\'"                     . html-mode)
         ("\\.html$"                          . web-mode)
         ("\\.htm$"                           . web-mode)
         ("\\.md$"                            . markdown-mode)
         ("\\.dart$"                          . dart-mode)
         ("\\.el$"                            . emacs-lisp-mode)
         ("\\.js$"                            . js2-mode)
         ("\\.gs$"                            . js2-mode)            ;; google script
         ;;("\\.\\(js\\|gs\\|jsi\\)$"           . js2-mode)
         ("\\.\\(avsc\\)$"                    . json-mode)           ;; avro schema
         ("\\.txt$"                           . text-mode)
         ("\\.asmx$"                          . csharp-mode)         ; likely, could be another language tho
         ("\\.\\(vb\\)$"                      . vbnet-mode)
         ("\\.\\(vbs\\|vba\\)$"               . vbs-mode)
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



;; replace all html-mode in this alist with web-mode, which is more betta.
(mapc
     (lambda (pair)
       (if (eq (cdr pair) 'html-mode)
           (setcdr pair 'web-mode)))
     auto-mode-alist)

;; 20230828-1703 replace java-mode with java-ts-mode
(mapc
     (lambda (pair)
       (if (eq (cdr pair) 'java-mode)
           (setcdr pair 'java-ts-mode)))
     auto-mode-alist)


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
;; speedbar

(require 'sr-speedbar) ;; put speedbar in same frame
(setq speedbar-use-images nil)
;; use sr-speedbar-open to open the speedbar window


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

;;(require 'gist)

;; Sometimes when invoking gist-buffer, you get an error like this:
;; Invalid slot type: gh-gist-gist, id, string, nil
;; If so, just run 'gist-list' and retry the gist-buffer.
(eval-after-load "gist"
  '(progn
;; note that we added the DESCRIPTION argument
(defun gist-region-with-description (begin end &optional description private callback)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nsGist Description: \nP") ;; we handle the prompt here!
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt"))
         (fname (concat (file-name-sans-extension name) "." ext))
         (files (list
                 (gh-gist-gist-file "file"
                                    :filename fname
                                    :content (buffer-substring begin end)))))
    ;; finally we use our new arg to specify the description in the internal call
    (gist-internal-new files private description callback)))
     ))

;; (defun dino-add-user-agent (old-function &rest arguments)
;;   "set the user agent when invoking github APIs"
;;   (let ((url-user-agent "emacs/url-http.el"))
;;     (apply old-function arguments)))
;;(advice-add #'gh-api-authenticated-request :around #'dino-add-user-agent)
;;(advice-remove #'gh-api-authenticated-request  #'dino-add-user-agent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-dired
(eval-after-load "image-dired"
  '(progn
     (require 'image-dired-fixups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-mode

(defun dino/image-transform-fit-to-window()
  "Resize the image to fit the width or height based on the image and window ratios."
  (interactive)
  (let* ( (img-size (image-display-size (image-get-display-property) t))
          (img-width (car img-size))
          (img-height (cdr img-size))
          (img-h/w-ratio (/ (float img-height) (float img-width)))
          (win-width (- (nth 2 (window-inside-pixel-edges))
                        (nth 0 (window-inside-pixel-edges))))
          (win-height (- (nth 3 (window-inside-pixel-edges))
                         (nth 1 (window-inside-pixel-edges))))
          (win-h/w-ratio (/ (float win-height) (float win-width))))
    ;; Fit image by width if the h/w ratio of window is > h/w ratio of the image
    (if (> win-h/w-ratio img-h/w-ratio)
        (image-transform-fit-to-width)
      ;; Else fit by height
      (image-transform-fit-to-height))))

(defun dino-image-mode-fn ()
  "My hook for image-mode"
  (dino/image-transform-fit-to-window)
  (local-set-key "h"  'image-transform-fit-to-height)
  (local-set-key "w"  'image-transform-fit-to-width))

(add-hook 'image-mode-hook 'dino-image-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml
(require 'yaml-mode)
(require 'yaml-pretty-mode)
(defun dino-yaml-mode-fn ()
  "My hook for YAML mode"
  (interactive)
  (turn-on-font-lock)
  (turn-on-auto-revert-mode)
  (display-line-numbers-mode)
  (yaml-pretty-mode)
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil))

(add-hook 'yaml-mode-hook 'dino-yaml-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun dino-typescript-mode-fn ()
  (turn-on-font-lock)
  (local-set-key "\M-\C-R"  'indent-region)
  (turn-on-auto-revert-mode)
  (setq typescript-indent-level 2)
  (display-line-numbers-mode)
  (auto-fill-mode -1)
  )
(add-hook 'typescript-mode-hook 'dino-typescript-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powershell

(autoload 'powershell-mode "powershell-mode" "major mode for editing powershell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))

(eval-after-load "hideshow"
  '(progn
     ;; hideshow for powershell
     (setq dpc-hs-settings-for-powershell-mode
           '(powershell-mode
             "{"                                 ;; regexp for start block
             "}"                                 ;; regexp for end block
             "[ \\t]*#"                          ;; regexp for comment start
             forward-sexp                        ;; hs-forward-sexp-func
             hs-c-like-adjust-block-beginning    ;; c-like adjust (1 char)
             ))

     (unless (assoc 'powershell-mode hs-special-modes-alist)
       (push dpc-hs-settings-for-powershell-mode hs-special-modes-alist))))

;; replace:
;;(setf (cdr (rassoc 'powershell-mode hs-special-modes-alist) ) something-here)

;; shadow:
;; (add-to-list 'hs-special-modes-alist dpc-hs-settings-for-powershell-mode )

;; delete:
;; (assq-delete-all 'powershell-mode hs-special-modes-alist)

(defun dino-powershell-mode-fn ()
  (electric-pair-mode 1)
  (require 'hideshow)
  (hs-minor-mode t)
  (display-line-numbers-mode)
  (dino-enable-delete-trailing-whitespace)
  )

(add-hook 'powershell-mode-hook 'dino-powershell-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua

(autoload 'lua-mode "lua-mode" "major mode for editing Lua." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(if (eq system-type 'windows-nt)
    (setq lua-default-application "c:\\tools\\lua\\lua52.exe")
  )

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
  (local-set-key "\C-c1" 'just-one-space)
  (local-set-key (kbd "<f7>") 'find-file-at-point)
  ;; Make sure autofill is OFF.
  (auto-fill-mode -1)
  )

(add-hook 'html-mode-hook 'dino-html-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;; (add-to-list 'load-path (file-name-as-directory "~/elisp/auto-complete-1.3.1"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSharp Code Completion

(add-to-list 'load-path (file-name-as-directory "~/elisp/cscomp"))

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
  (yas-minor-mode-on)

  ;; use autopair for curlies, parens, square brackets.
  ;; electric-pair-mode works better than autopair.el in 24.4,
  ;; and is important for use with popup / auto-complete.
  (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
      (progn (require 'autopair) (autopair-mode))
    (electric-pair-mode))

  ;;(require 'myfixme)
  ;;(myfixme-mode 1)

  (require 'rfringe)
  (setq comment-empty-lines t))


(add-hook 'vbnet-mode-hook 'dino-vbnet-mode-fn)
(add-hook 'vbs-mode-hook 'dino-vbnet-mode-fn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS mode

;;(require 'css-mode)
(autoload 'css-mode "css-mode" "Mode for editing Cascading Stylesheets." t)

(push '(prettier-css-dino .
        ("/usr/local/bin/prettier"
         "--stdin-filepath" filepath "--parser=css"))
      apheleia-formatters)

(push '(css-mode . prettier-css-dino) apheleia-mode-alist)

;; Overwrite existing scss-stylelint checker to not use --syntax
(flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            ;; "--syntax" "scss"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :modes (scss-mode))

(defun dino-css-mode-fn ()
  "My hook for CSS mode"
  (interactive)
  (turn-on-font-lock)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-c\C-w" 'compare-windows)
  (local-set-key "\C-c\C-c" 'comment-region)

  (turn-on-auto-revert-mode)
  (display-line-numbers-mode)
  (apheleia-mode)

  ;; use autopair for curlies, parens, square brackets.
  ;; electric-pair-mode works better than autopair.el in 24.4,
  ;; and is important for use with popup / auto-complete.
  (if (not (fboundp 'version<))
      (progn (require 'autopair) (autopair-mode))
    (electric-pair-mode))

  (setq css-indent-offset 2)

  ;; auto-complete mode is on globally
  ;; make sure that <RETURN> is not an autocomplete key
  (define-key ac-complete-mode-map "\r" nil)

  ;; make auto-complete start only after 2 chars
  (setq ac-auto-start 2)  ;;or 3?

  (require 'flycheck)
  (flycheck-mode)

  ;; to install the external checkers:
  ;; sudo npm install -g csslint
  ;; sudo npm install -g stylelint stylelint-config-standard stylelint-scss
  (flycheck-select-checker
   (if (string= mode-name "SCSS") 'scss-stylelint 'css-csslint))

  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local)

  ;; rainbow-mode is no longer needed as of emacs 26.1. css-mode now colorizes
  ;; color expressions automatically.
  ;; ============================================
  ;; ;; display CSS colors in color
  ;; ;; (require 'rainbow-mode)
  ;; ;; (rainbow-mode)

  (display-line-numbers-mode)

  ;; "no tabs" -- use only spaces
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil))

(add-hook 'css-mode-hook 'dino-css-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert - 20231005-1717

(auto-insert-mode 1);; global minor mode
(setq auto-insert-query nil) ;; no prompt before auto-insertion:
(setq auto-insert-directory "~/elisp/auto-insert-content")

(require 'auto-insert-plus)

;; specify the template to use for various filename regexi:
(setq my-auto-insert-alist
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
        ("\\.org$"                     .  "Template.org" )
        ) )

(setq auto-insert-alist
      (aip/fixup-auto-insert-alist my-auto-insert-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure external utilities

(if (eq system-type 'windows-nt)
    (if (file-exists-p "c:/Users/dpchi/bin/unzip.exe")
        (progn
          (setq archive-zip-use-pkzip nil   ; i.e. use unzip instead
                archive-zip-extract '("c:/Users/dpchi/bin/unzip.exe" "-"))))
  )

(setq-default grep-command "grep -i -n ")



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
  ;;time-stamp-format "%:a, %:b %02d, %Y  %02H:%02M:%02S %Z (by %u)") ; date format
  ;;time-stamp-format "%Y-%:b-%02d %02H:%02M:%02S" ; date format
  time-stamp-pattern "34/\\(\\(L\\|l\\)ast\\( \\|-\\)\\(\\(S\\|s\\)aved\\|\\(M\\|m\\)odified\\|\\(U\\|u\\)pdated\\)\\|Time-stamp\\) *: <%Y-%:b-%02d %02H:%02M:%02S>")

;; can also add this to source code: // (set-variable time-stamp-format "%Y-%:b-%02d %02H:%02M:%02S")

(add-hook 'before-save-hook 'time-stamp)  ; update time stamps when saving






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode

(require 'dired)
(require 'dino-dired-fixups)

(defun dino-dired-mode-hook-fn ()
  (hl-line-mode 1)

  ;; do not want auto-revert.
  ;; It doesn't work completely, and it may have side effects.
  ;; ;; (turn-on-auto-revert-mode)
  ;; Anyway, there is a revert-on-timer thing provided in dired-fixups.el

  (local-set-key "\C-c\C-g"  'dino-dired-kill-new-file-contents)
  (local-set-key "\C-c\C-c"  'dino-dired-copy-file-to-dir-in-other-window)
  (local-set-key "\C-c\C-m"  'dino-dired-move-file-to-dir-in-other-window)
  (local-set-key "F" 'dino-dired-do-find)
  (local-set-key "s" 'dino-dired-sort-cycle)
  (dino-dired-sort-cycle "t") ;; by default, sort by time
  (local-set-key "K" 'dired-kill-subdir)) ;; opposite of i (dired-maybe-insert-subdir)

(add-hook 'dired-mode-hook 'dino-dired-mode-hook-fn)

;; eliminate the gid in dired on windows
(setq ls-lisp-verbosity '(links uid))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode

(defun dino-fix-abbrev-table ()
  "set up a custom abbrev table. The normal
saving isn't allowed on my computer. Really these are
just auto-corrects on common mis-spellings by me."

  (define-abbrev-table 'text-mode-abbrev-table
    '(
      ("teh" "the" nil 1)
      ("somehting" "something" nil 1)
      ("deprectaed" "deprecated" nil 0)
      ("APigee" "Apigee" nil 1)
      ("hting" "thing" nil 1)
      ("rigueur" "rigeuer" nil 1)
      ("riguer" "rigeuer" nil 1)
      ("submint" "submit" nil 1)
      ("rwquest" "request" nil 1)
      ("hygeine" "hygiene" nil 0)
      ("laucnhed" "launched" nil 0)
      ("supproted" "supported" nil 0)
      ("comittee" "committee" nil 0)
      ("machien" "machine" nil 0)
      ("siilar" "similar" nil 0)
      ("machiens" "machines" nil 0)
      ("cusotmer" "customer" nil 0)
      ("accommplish" "accomplish" nil 0)
      ("accomodate" "accommodate" nil 0)
      ("recieve" "receive" nil 0)
      ("vairous" "various" nil 0)
      ("multipel" "multiple" nil 0)
      ("acheive" "achieve" nil 0)
      ("acheived" "achieved" nil 0)
      ("APigee" "Apigee" nil 1)
      ("becasue" "because" nil 1)
      ("btw" "by the way" nil 3)
      ("omw" "on my way" nil 3)
      )
    ))


(defun dino-text-mode-hook-fn ()
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (dino-fix-abbrev-table)
  ;;(variable-pitch-mode)
  ;;
  ;; add new abbreviations above.
  ;;

  ;; (require 'refill) ;; automatically refill paragraphs
  ;; (refill-mode 1)
  )

(add-hook 'text-mode-hook 'dino-text-mode-hook-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linum-ex - linum-mode with enhancements for coloring and delay (for performance)

;; ;;(require 'linum-ex)
;; (autoload 'linum-on "linum-ex" nil t)
;; (autoload 'linum-mode "linum-ex" nil t)
;; (eval-after-load "linum-ex"
;;   '(progn
;;     ;;(message "%s" (prin1-to-string (defined-colors))) ;; to list possible colors
;;      ;;(list-colors-display) to show a list of colors in a new buffer
;;     (set-face-foreground 'linum "SlateGray")
;;     ;;(set-face-background 'linum "WhiteSmoke")
;;     (set-face-background 'linum "gray19")
;;     ;; to see font string M-x describe-font
;;     ;;(set-face-font 'linum "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
;;     ;;(set-face-font 'linum "-*-Consolas-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
;;     ;;(setq setnu-line-number-face 'setnu)
;;     (setq linum-delay t)
;; ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dino-enable-delete-trailing-whitespace ()
  "remove trailing whitespace"
  (interactive)
    ;; remove trailing whitespace in C files
    ;; http://stackoverflow.com/questions/1931784
    ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode  (common)
                                        ;
(require 'dtrt-indent)
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
    (dtrt-indent-mode t)
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
    (add-hook 'before-save-hook
              (lambda ()
                 (save-excursion
                   (delete-trailing-whitespace)))
              nil 'local)

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

         (set (make-local-variable 'comment-start) "// ")
         (set (make-local-variable 'comment-end) "")

         (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
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
         (yas-minor-mode-on)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobufs

(require 'protobuf-mode)
(defconst my-protobuf-style
   '((c-basic-offset . 2)
     (indent-tabs-mode . nil)))
(defun dino-protobuf-mode-hook-fn ()
  "my mode hook for protobuf-mode"
         (local-set-key "\M-\C-R"  'indent-region)
         (local-set-key "\M-#"     'dino-indent-buffer)
         (local-set-key "\C-c\C-w" 'compare-windows)
  (display-line-numbers-mode)

  (c-add-style "my-style" my-protobuf-style t)
  ;;(require 'flycheck)
  ;;(flycheck-mode 1)
  )
(add-hook 'protobuf-mode-hook 'dino-protobuf-mode-fn)


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


;; hideshow for csharp - really this should be in csharp-mode.el
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




(defun dino-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (cond (window-system
         (turn-on-font-lock)
         (c-set-style "myC#Style")
         (setq c-basic-offset 2) ;; width of one indent level
         (message "setting local key bindings....")

         (local-set-key "\M-\C-R"  'indent-region)
         (local-set-key "\M-#" 'dino-indent-buffer)
         (local-set-key "\C-c\C-w" 'compare-windows)

         (local-set-key "\C-c\C-y"  'csharp-show-syntax-table-prop)
         (local-set-key "\C-c\C-h"  'csharp-show-parse-state)

         (local-set-key (kbd "C-<") 'csharp-move-back-to-beginning-of-defun)
         (local-set-key (kbd "C->") 'csharp-move-fwd-to-end-of-defun)

         ;; this works
         (local-set-key (kbd "C-M-\<") 'csharp-move-back-to-beginning-of-class)
         (local-set-key (kbd "C-M-\>") 'csharp-move-fwd-to-end-of-class)

         ;; TODO: consider relying on electric-pair
         (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)

         ;; insert a pair of quotes
         (local-set-key (kbd "\"") 'dino-insert-paired-quotes)
         (local-set-key (kbd "\'") 'dino-insert-paired-quotes)

         ;; these allow typeover of matching brackets
         ;; (local-set-key (kbd "\"") 'dino-skeleton-pair-end)
         (local-set-key (kbd ">") 'dino-skeleton-pair-end)
         (local-set-key (kbd ")") 'dino-skeleton-pair-end)
         (local-set-key (kbd "]") 'dino-skeleton-pair-end)

         ;;(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)

         ;; Default to auto-indent on Enter
         ;;(define-key csharp-mode-map [(control j)] 'newline)
         ;;(define-key csharp-mode-map [(control m)] 'newline-and-indent)

         ;;(define-key csharp-mode-map [return] 'newline-and-indent)

         ;; for skeleton stuff
         (set (make-local-variable 'skeleton-pair) t)

         (yas-minor-mode-on)
         (show-paren-mode 1)
         (hl-line-mode 1)

         (require 'flycheck)
         (flycheck-mode)
;;         (flycheck-select-checker 'csharp)

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

         (display-line-numbers-mode)

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

  (require 'flycheck)
  (flycheck-mode)
  ;;(flycheck-select-checker 'php-phpcs) ;; style and syntax
  (flycheck-select-checker 'php) ;; syntax only

  (setq c-default-style "bsd"
        c-basic-offset 2)

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-\C-C"  'un-camelcase-word-at-point)

  ;; not sure if necessary or not.
  (modify-syntax-entry ?/ ". 124b" php-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" php-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  php-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" php-mode-syntax-table)

  ;; why / how is the following getting overridden?
  (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t)
  )
(add-hook 'php-mode-hook 'dino-php-mode-fn t)

(eval-after-load "php-mode"
  '(progn
     (require 'compile)
     ))



(defvar dc-php-program "/usr/bin/php" "PHP interpreter")

;; (defun dino-php-flymake-get-cmdline  (source base-dir)
;;   "Gets the cmd line for running a flymake session in a PHP buffer.
;; This gets called by flymake itself."
;;
;;   (dino-log "PHP" "flymake cmdline for %s" source)
;;
;;   (list dc-php-program
;;         (list "-f" (expand-file-name source)  "-l")))
;;
;;
;; (defun dino-php-flymake-init ()
;;   "initialize flymake for php"
;;   (let ((create-temp-f 'dino-flymake-create-temp-intemp)
;;         ;;(create-temp-f 'flymake-create-temp-inplace)
;;         (use-relative-base-dir t)
;;         (use-relative-source t)
;;         (get-cmdline-f 'dino-php-flymake-get-cmdline)
;;         args
;;         temp-source-file-name)
;;
;;     (dino-log "PHP" "flymake-for-php invoke...")
;;
;;     (setq temp-source-file-name (flymake-init-create-temp-buffer-copy create-temp-f)
;;
;;           args (flymake-get-syntax-check-program-args
;;                 temp-source-file-name "."
;;                 use-relative-base-dir use-relative-source
;;                 get-cmdline-f))
;;     args))
;;
;;
;; (defun dino-php-flymake-cleanup ()
;;   (dino-log "PHP" "flymake-for-php cleanup...")
;;   (flymake-simple-cleanup) )
;;
;; (eval-after-load "flymake"
;;   '(progn
;;      (if (file-exists-p dc-php-program)
;;          ;; 1. add a PHP entry to the flymake-allowed-file-name-masks
;;          (let* ((key "\\.php\\'")
;;                 (phpentry (assoc key flymake-allowed-file-name-masks)))
;;            (if phpentry
;;                (setcdr phpentry '(dino-php-flymake-init dino-php-flymake-cleanup))
;;              (add-to-list
;;               'flymake-allowed-file-name-masks
;;               (list key 'dino-php-flymake-init 'dino-php-flymake-cleanup)))))))



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
(push '(dino-xmlpretty .
        ("java" "-jar"
         "/Users/dchiesa/dev/java/XmlPretty/target/com.google.dchiesa-xml-prettifier-20230725.jar"
         "-"))
      apheleia-formatters)

(push '(xml-prettier .
        ("/Users/dchiesa/dev/java/XmlPretty/node_modules/.bin/prettier"
         "--config" "/Users/dchiesa/dev/java/XmlPretty/prettier-config.json"
         "--stdin-filepath" "foo.xml"))
      apheleia-formatters)


;; ;; change an existing formatter in the alist (during development only)
;; (setf (alist-get 'xml-prettier apheleia-formatters)
;;        '("/Users/dchiesa/dev/java/XmlPretty/node_modules/.bin/prettier"
;;           "--config" "/Users/dchiesa/dev/java/XmlPretty/prettier-config.json"
;;          "--stdin-filepath" "foo.xml"))

;; to specify an apheleia plugin for a mode that is not currently in the list:
;;(push '(nxml-mode . dino-xmlpretty) apheleia-mode-alist)
(push '(nxml-mode . xml-prettier) apheleia-mode-alist)

;; ;; to switch between previously set plugin for a mode:
;; (setf (alist-get 'nxml-mode apheleia-mode-alist) 'xml-prettier)
;; ;;(setf (alist-get 'nxml-mode apheleia-mode-alist) 'dino-xmlpretty)


(defun dino-xml-mode-fn ()
  (turn-on-auto-revert-mode)
  ;; for hide/show support
  (hs-minor-mode 1)
  (setq hs-isearch-open t)
  (display-line-numbers-mode)
  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\C-cn"    'sgml-name-char) ;; inserts entity ref of pressed char
  (local-set-key "\M-#"     'dino-xml-pretty-print-buffer)
  (local-set-key "\C-cf"    'dino-replace-filename-no-extension)

  (local-set-key (kbd "C-<")  'nxml-backward-element)
  (local-set-key (kbd "C->")  'nxml-forward-element)
  (local-set-key "\C-c\C-c"  'dino-xml-comment-region)

  ;; C-M-f will jump over complete elements

  (setq nxml-sexp-element-flag t
        nxml-child-indent 2)

  ;; never convert leading spaces to tabs:
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  ;; 20230718-1235
  ;;(apheleia-mode)

  ;; Include single-quote as a string-quote char
  ;; Without this, it was being treated as part of a word,
  ;; I guess because xml-mode is derived from text-mode where
  ;; it's an apostrophe used in contractions.
  ;; But treating it as part of a word is counter-productive in an XML buffer.
  (if (boundp 'sgml-mode-syntax-table)
      (modify-syntax-entry ?\' "\"" sgml-mode-syntax-table)
    (modify-syntax-entry ?\' ".")) ;; . = punctuation

  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local)

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

;; not sure why I have to do this again, here.
(add-hook 'nxml-mode-hook
          (lambda () (modify-syntax-entry ?\' ".")))


(add-to-list 'hs-special-modes-alist ;; for hideshow.el?
             '(sgml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"                ;; regexp for comment start. (need this??)
               sgml-skip-tag-forward
               nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlighting specific chars
;;

(require 'highlight-chars) ; Load this library.

;; (global-set-key (kbd "<f8>")
;;                 'hc-toggle-highlight-trailing-whitespace)

(defun dino-enable-highlight-trailing-ws-based-on-extension ()
  "turns on highlighting of trailing whitespace based on file extension"
  (let ((extension (file-name-extension buffer-file-name))
        (extensions-that-get-highlighting '("md" "css" "java" "js" "go") ))
    (if (member "go" extensions-that-get-highlighting)
          (hc-highlight-trailing-whitespace))))

(add-hook 'find-file-hook 'dino-enable-highlight-trailing-ws-based-on-extension)


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
  (display-line-numbers-mode)

  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local)
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
  (display-line-numbers-mode)

  ;; Use autopair for curlies, parens, square brackets.
  ;; electric-pair-mode works better than autopair.el in 24.4,
  ;; and is important for use with popup / auto-complete.
  (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
      (progn (require 'autopair) (autopair-mode))
    (electric-pair-mode))

  ;; ya-snippet
  (yas-minor-mode-on)

  (show-paren-mode 1))

(add-hook 'python-mode-hook 'dino-python-mode-fn)


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
;; tern
;;
;; I think I don't need this, now that I am using deno.
;;
;; (add-to-list 'load-path (file-name-as-directory "~/elisp/tern/emacs/"))
;; (autoload 'tern-mode "tern.el" nil t)
;;
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript - js2-mode (new - 20180416-1511)
(autoload 'js2-mode "js2-mode" nil t)
;;(eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(push '(prettier-js-dino .
        ("/usr/local/bin/prettier"
         "--stdin-filepath" filepath "--parser=babel-flow"
         "--trailing-comma" "none"
         (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
      apheleia-formatters)

(push '(js-mode . prettier-js-dino) apheleia-mode-alist)

(defun dino-js2-mode-fn ()
  ;;(tern-mode)
  (auto-complete-mode 0) ;; turn off auto-complete-mode
  (lsp)
  ;; you automatically get deno-lint with lsp mode.
  ;; The linter is not configurable.  You can turn it on or turn it off.
  ;; To turn it off:
  ;; (setq lsp-clients-deno-enable-lint nil)

  ;; lsp, when configured to use posframe for signature display, uses the
  ;; background and foreground from the lsp-signature-posframe face to display
  ;; the signature.
  (set-face-attribute 'lsp-signature-posframe nil :background "LightSteelBlue1" )
  ;; (set-face-attribute 'lsp-signature-posframe t :background "LightSteelBlue1")
  ;; (face-attribute 'lsp-signature-posframe :background nil t)
  ;; (face-attribute 'lsp-signature-posframe :foreground nil t)

  ;; 20230918-1015
  (apheleia-mode)

  (company-mode)
  (company-box-mode)
  (define-key company-mode-map (kbd "M-<tab>") 'company-complete)
  (setq company-minimum-prefix-length 2
   lsp-signature-function 'lsp-signature-posframe
   js2-basic-offset 2)
  )
(add-hook 'js2-mode-hook #'dino-js2-mode-fn)

(defun dino-js2-apply-globals-from-jshintrc (&rest arguments)
  "load extra globals from jshintrc when sending requests"
  (js2-add-additional-externs (dino-read-globals-from-jshintrc)))

(eval-after-load "js2-mode"
  '(progn
     (advice-add 'js2-apply-jslint-globals :after #'dino-js2-apply-globals-from-jshintrc)))


(defun dino-posframe-swap-background (str)

  "HACK 20230627 the posframe package exposes an emacs bug, I
think. After displaying a frame, as with the information from an
LSP server showing the function definition, the background and
foreground colors for the frame can ... change.  Even while the
frame is still displayed. The background can sometimes revert to the
default background, which is black. Used with a dark foreground, the
signature definition can be unreadable.

You'd think just displaying a new frame would solve it but
posframe caches the old frame and checks the frame params for new
frames against the cached one. I guess for performance. Anyway
the result is, once the bg color is munged, it stays that way.

The documented way to override the fg
and bg for `lsp-signature-posframe' is to set the fg and bg
properties on the `lsp-signature-posframe' face. Eg

(set-face-attribute 'lsp-signature-posframe nil :background \"lightyellow\") .

This should directly affect the cached posframe, but because of the bug,
somehow it does not.

This function swaps between two similar bg colors, to prevent
posframe from caching the frame, which then... allows the frames
to display properly. It swaps only when the input str is blank,
which happens when the help is to disappear. That makes the face
color ready for next time.
"
  (if (not str)
      (let ((cur-bg (face-attribute 'lsp-signature-posframe :background nil t)))
        (set-face-attribute 'lsp-signature-posframe nil :background
                            (if (string= "LightSteelBlue1" cur-bg)
                                "SlateGray1" "LightSteelBlue1")))))
(eval-after-load "lsp"
  '(progn
     ;; I think this might help avoid a frame bug?
     (plist-put lsp-signature-posframe-params :font "Menlo")
     (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)))

;;    (set-face-attribute 'lsp-signature-posframe nil :background "LightSteelBlue1" )

;;     (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)
;;     (advice-remove 'lsp-signature-posframe #'dino-posframe-swap-background)



(js2r-add-keybindings-with-prefix "C-c C-m")

;; xxx WHY have I deleted this?
;;
;; (defun dino-js2-mode-fn ()
;;   (turn-on-font-lock)
;;
;;   (local-set-key "\M-\C-R"  'indent-region)
;;   (local-set-key "\M-#"     'dino-indent-buffer)
;;   (local-set-key "\C-c\C-c" 'comment-region)
;;   (local-set-key (kbd "<C-tab>") 'yas-expand)
;;
;;   (set (make-local-variable 'indent-tabs-mode) nil)
;;   ;; indent increment
;;   (set (make-local-variable 'js-indent-level) 2)
;;   (linum-on)
;;
;;   ;; use autopair for curlies, parens, square brackets.
;;   ;; electric-pair-mode works better than autopair.el in 24.4,
;;   ;; and is important for use with popup / auto-complete.
;;   (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
;;       (progn (require 'autopair) (autopair-mode))
;;     (electric-pair-mode))
;;
;;   ;; json-mode is a child mode of js-mode. Select different checker
;;   ;; based on the file extension.
;;   (require 'flycheck)
;;   (if (and buffer-file-name
;;            (file-name-directory buffer-file-name))
;;        (progn
;;          (flycheck-mode)
;;          (flycheck-select-checker
;;           (if (string-suffix-p ".json" buffer-file-name)
;;               'json-jsonlint
;;             'javascript-jshint))))
;;
;;   (yas-minor-mode-on)
;;
;;   (require 'smart-op) ;; for smart insertion of ++ and == and += etc
;;   (smart-op-mode)
;;   )
;;
;;
;; (add-hook 'js2-mode-hook   'dino-js2-mode-fn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript - js-mode (old?)
;;(autoload 'js-mode "js" nil t)  ;; 20220426-2016
(defun dino-js-mode-fn ()
  ;; https://stackoverflow.com/a/15239704/48082
  (set (make-local-variable 'font-lock-multiline) t)
  ;; (add-hook 'font-lock-extend-region-functions
  ;;           'js-fixup-font-lock-extend-region)

  (turn-on-font-lock)

  ;; Dino 20210127-1259 - trying to diagnose checker errors
  ;;
  ;; ;; for syntax-checking, auto-complete, etc
  ;; (require 'tern)
  ;; (tern-mode t)
  ;; I have now moved to deno and lsp-mode

  (local-set-key "\M-\C-R"  'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)
  (local-set-key "\C-cc"    'comment-region)
  ;; (local-set-key "\C-c."    'tern-ac-complete)
  ;;(local-set-key (kbd "C-.")  'tern-ac-complete)
  ;;(local-set-key (kbd "<M-tab>") 'tern-ac-complete)
  (local-set-key (kbd "C-.")  'company-capf)
  (local-set-key "\C-c."    'company-capf)
  (local-set-key (kbd "<M-tab>") 'company-capf)
  (local-set-key (kbd "TAB") 'js-indent-line)
  (local-set-key (kbd "<C-tab>") 'yas-expand)

  (set (make-local-variable 'indent-tabs-mode) nil)

  ;; indent increment
  (set (make-local-variable 'js-indent-level) 2)

  ;; interactive javascript shell
  ;;(local-set-key "\C-x\C-e" 'jsshell-send-last-sexp)
  ;; (local-set-key "\C-\M-x"  'jsshell-send-last-sexp-and-pop)
  ;; (local-set-key "\C-cb"    'jsshell-send-buffer)
  ;; (local-set-key "\C-c\C-b" 'jsshell-send-buffer-and-pop)
  ;; (local-set-key "\C-cl"    'jsshell-load-file-and-pop)
  ;; (local-set-key "\C-c\C-e" 'jsshell-send-region)

  ;; use autopair for curlies, parens, square brackets.
  ;; electric-pair-mode works better than autopair.el in 24.4,
  ;; and is important for use with popup / auto-complete.
  (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
      (progn (require 'autopair) (autopair-mode))
    (electric-pair-mode))

  ;; Dino 20210127-1259 - trying to diagnose checker errors
  ;;
  ;; ;; json-mode is a child mode of js-mode. Select different checker
  ;; ;; based on the file extension.
  ;; (require 'flycheck)
  ;; (if (and buffer-file-name
  ;;          (file-name-directory buffer-file-name))
  ;;      (progn
  ;;        (flycheck-mode)
  ;;        (flycheck-select-checker
  ;;         (if (string-suffix-p ".json" buffer-file-name)
  ;;             'json-jsonlint
  ;;           'javascript-jshint))))

  ;;(flycheck-select-checker 'javascript-eslint) ;; for more control?
  ;;
  ;; Tuesday,  2 January 2018, 15:36
  ;; I tried eslint for emacs and found that it complained a lot about
  ;; the indent style I prefer. Also I could not figure out how to get it to
  ;; stop complaining. So I didn't use it, and still use jshint, which
  ;; seems to work just fine.


  ;; (require
  ;;  (if (eq system-type 'windows-nt)
  ;;      'fly-jshint-wsh 'fly-jshint-npm))

  ;;(setq fly/jshint/npm-jshint-exe "/usr/local/bin/jshint")

  ;; ;;(setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jslint-for-wsh.js")
  ;; (setq flyjs-jslintwsh-location "c:\\users\\dino\\bin\\jshint-for-wsh.js")

  (yas-minor-mode-on)

  ;; always delete trailing whitespace
  (add-hook 'before-save-hook
            (lambda ()
               (save-excursion
                 (delete-trailing-whitespace)))
            nil 'local)

  ;;(dino-enable-delete-trailing-whitespace)

  (require 'imenu)
  (imenu-add-menubar-index)

  (require 'hideshow)
  (hs-minor-mode t)

  (require 'smart-op) ;; for smart insertion of ++ and == and += etc
  (smart-op-mode)

  (display-line-numbers-mode)
  )

(add-hook 'js-mode-hook   'dino-js-mode-fn)
;;(add-hook 'js2-mode-hook   'dino-js-mode-fn)

;; for {jshint, jslint, flycheck javascript-jshint} to work,
;; the path must have been previously set correctly.

(require 'js-mode-fixups)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
(require 'json-mode)
(require 'json-reformat)

(autoload 'json-mode "json" nil t)

;; add a new element to the front of the list and it will shadow matches further down the list.
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(defun dino-json-mode-fn ()
  ;;(turn-on-font-lock)
  ;;(flycheck-mode 0)
  )

(add-hook 'json-mode-hook   'dino-json-mode-fn)

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

;; change the existing google-java-format in the builtin apheleia-formatters
(setf (alist-get 'google-java-format apheleia-formatters)
      '("java" "-jar" "/Users/dchiesa/dev/java/lib/google-java-format-1.17.0-all-deps.jar" "-"))

(defun dino-java-mode-fn ()
  (if c-buffer-is-cc-mode
  (c-set-style "myJavaStyle"))
  (turn-on-font-lock)
  (local-set-key "\M-\C-R" 'indent-region)
  (local-set-key "\M-#"     'dino-indent-buffer)

  (modify-syntax-entry ?_ "w")

  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'c-basic-offset) 2) ;; dino 20190418-1407

  ;; 20191015-1837 - better than autopair or skeleton pair
  (electric-pair-mode)

  ;; 20230718-1235
  (apheleia-mode)

  ;;(set (make-local-variable 'skeleton-pair) t)
  ;;(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  ;;(local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  ;; ;;(local-set-key (kbd "\"") 'dino-skeleton-pair-end)

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".java")))

  ;; some of my own java-mode helpers
  (require 'dcjava)
  (local-set-key "\C-ci"     'dcjava-auto-add-import)
  (local-set-key "\C-c\C-i"  'dcjava-auto-add-import)
  (local-set-key "\C-cp"     'dcjava-insert-inferred-package-name)
  (local-set-key "\C-c\C-l"  'dcjava-learn-new-import)
  (local-set-key "\C-c\C-f"  'dcjava-find-wacapps-java-source-for-class-at-point)
  (local-set-key "\C-c\C-r"  'dcjava-reload-classlist)
  (local-set-key "\C-c\C-s"  'dcjava-sort-import-statements)

  ;; 20230828 With apheleia-mode, the manual gformat is unnecessary. just save.
  (local-set-key "\C-c\C-g"  'dcjava-gformat-buffer)
  (dino-enable-delete-trailing-whitespace)
  (display-line-numbers-mode)
  )

(add-hook 'java-mode-hook 'dino-java-mode-fn)
;;(remove-hook 'java-ts-mode-hook 'dino-java-mode-fn)

;; 20230828-1658
;; treesitter mode for Java with emacs 29.1.
;; It provides better, more reliable syntax analysis, and better highlighting.
;;
;; As a one-time setup thing, need to do this:
;;   M-x treesit-install-language-grammar RET java
;; to build the language grammar. This requires a compiler.
;; See: https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html
;;
;; Some changes: the indent is no longer based on c-basic-offset.
;; It is `java-ts-mode-indent-offset'.  So we need a different mode hook.
;; For some reason, the java-ts-mode is no longer a cc-mode, so
;; calling `c-set-style' will result in an error.  So I needed to change that
;; in the java-mode hook, to check whether the mode is a cc-mode or not.
;;
;; You can try M-x load-library RET treesit-fold
;; and then ‘treesit-fold-toggle’ to expand/collapse blocks.
;;
(defun dino-java-ts-mode-fn ()
  (setq java-ts-mode-indent-offset 2)
  (dino-java-mode-fn)
  )

(add-hook 'java-ts-mode-hook 'dino-java-ts-mode-fn)



;;



(c-add-style "myJavaStyle"
             '("Java"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 2)
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

;; proselint

(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker proselint
       "A linter for prose."
       :command ("proselint" source-inplace)
       :error-patterns
       ((warning line-start (file-name) ":" line ":" column ": "
                 (id (one-or-more (not (any " "))))
                 (message (one-or-more not-newline)
                          (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                 line-end))
       :modes (text-mode markdown-mode gfm-mode))
     (add-to-list 'flycheck-checkers 'proselint)
     ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile stuff

(require 'smarter-compile)

(setq smart-compile-alist
      (append
       '(
         ("\\.go\\'"      . "go build %f")
         ("\\.txt\\'"      . "proselint %f")
         ("\\.wxs\\'"      . "%M %n.msi")
         ("\\.css\\'"      . "~/js/csslint.node.js %f")
         ("\\.js\\'"       . "~/js/jshint.node.js %f")
         ) smart-compile-alist ))

(eval-after-load "compile"
  '(progn
     (setq compilation-scroll-output "first-error")
     ;;(setq-default compile-command (concat nmake.exe " "))
     (setq-default compile-command "make ")
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; salted files
(require 'salted)
(setq salted--salt-file-utility "~/dev/go/src/github.com/DinoChiesa/salted/salt_file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github GraphQL mode
(autoload 'gh-graphql-mode "gh-graphql")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restclient - invoke REST API calls from within Emacs

(require 'restclient)
(require 'dino-netrc)

(eval-after-load "restclient"
  '(progn

     (defadvice restclient-http-do (around dino-restclient-eliminate-giant-useless-header activate)
       "make emacs be less chatty when sending requests"
       (let (url-mime-charset-string url-user-agent url-extensions-header)
         ad-do-it))

     (if (not (fboundp 'json-pretty-print-buffer))
         (defun json-pretty-print-buffer ()
           (json-prettify-buffer)))
     ))


(eval-after-load "url"
  '(progn
     (defun dino-url-http-cleaner-request (old-function &rest arguments)
       "make url-http be less chatty when sending requests"
       (let (url-mime-charset-string
             url-extensions-header
             (url-user-agent "User-Agent: emacs/url-http.el\r\n"))
         (apply old-function arguments)))
     (advice-add #'url-http-create-request :around #'dino-url-http-cleaner-request)))


;; to disable at runtime:
;; (ad-disable-advice 'url-http-create-request 'around 'dino-url-eliminate-giant-useless-header)
;; (ad-activate 'url-http-create-request)

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
         (defadvice url-http-create-request (before
                                  dino-set-proxy-dynamically
                                  activate)
           "Before retrieving a URL, query the IE Proxy settings, and use them."
           (let ((proxy (w32reg-get-ie-proxy-config)))
             (setq url-using-proxy proxy
                   url-proxy-services proxy))))))

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
                         (lambda (x) (cons (file-name-nondirectory x) x)))
                        all-files))
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
(global-set-key "\C-ct"     'thesaurus-choose-synonym-and-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dictionary
;;
(require 'dictionary)
(global-set-key "\C-c\C-d"     'dictionary-get-definition)

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


;; System-specific configuration
;; Loads system-type config; e.g. "darwin.el" on Mac
(let ((system-specific-elisp (concat "~/elisp/" (symbol-name system-type) ".el")))
  (if (file-exists-p system-specific-elisp)
    (load system-specific-elisp)))

;; auto-revert for all files.
(add-hook 'find-file-hook
              (lambda () (turn-on-auto-revert-mode)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; google things
;; (add-to-list 'load-path "~/elisp/site-lisp/emacs-google-config/devtools/editors/emacs")
;; ;; (load-file
;; ;;  "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")
;; (require 'google-java-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic, default colors

;; see this for an example of color theming:
;;    http://hi.baidu.com/masterray/blog/item/9caa181fb9177cc2a68669a8.html

(set-background-color "black")  ;; need this if I also do set-face-background?
(set-face-foreground 'default "white")
(set-face-background 'default "black")


;; I couldn't get eval-after-load to work with hl-line, so
;; I made this an after advice.
(defadvice hl-line-mode (after
                         dino-advise-hl-line-mode
                         activate compile)
  (set-face-background hl-line-face "gray18"))

(global-hl-line-mode)

(custom-set-faces
 '(flycheck-error               ((t (:background "firebrick4"))))
 '(font-lock-comment-face       ((t (:foreground "PaleVioletRed3"))))
 '(font-lock-keyword-face       ((t (:foreground "CadetBlue2"))))
 ;;'(font-lock-keyword-face       ((t (:foreground "Cyan1"))))
 '(font-lock-type-face          ((t (:foreground "PaleGreen"))))
 '(font-lock-constant-face      ((t (:foreground "DodgerBlue"))))
 '(font-lock-function-name-face ((t (:foreground "RoyalBlue1"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-string-face        ((t (:background "gray11" :foreground "MediumOrchid1")))))

(set-face-foreground 'tooltip "Navy")
(set-face-background 'tooltip "khaki1")

(set-face-background 'font-lock-string-face "gray11")

;;(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq default-directory "~/")
(message "Done with emacs.el...")

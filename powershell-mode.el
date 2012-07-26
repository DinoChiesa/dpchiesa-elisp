;; powershell-mode.el, version 0.4
;; 
;; Author: Vivek Sharma (http://www.viveksharma.com/techlog) 
;; Provides: Major mode for editing PS (PowerShell) scripts 
;; Last Updated: 12/29/2006 
;;
;; TODO
;; - Indentation support  (done)
;; - support here strings (done)
;; - C# class support 
;; - lots of other stuff
;;
;; CHANGES
;; 0.1 - initial version with text highliting support and indenting 
;; 0.2 - fixed file based on feedback to handle xemacs and syntax file changes 
;; 0.3 - updated to reflect Monad --> PowerShell name change 
;; 0.4 - added manual indentation support, not totally what i'd like, but good enough 




;; custom hooks 
(defvar powershell-mode-hook nil)

;; default mode map, really simple 
(defvar powershell-mode-map  
  (let ((powershell-mode-map (make-keymap)))
;;    (define-key powershell-mode-map "\r" 'powershell-indent-line)
    (define-key powershell-mode-map "\t" 'powershell-indent-line)
    (define-key powershell-mode-map "{" 'powershell-electric-brace)
    (define-key powershell-mode-map "}" 'powershell-electric-brace)
    powershell-mode-map)
  "Keymap for PS major mode")


;; For some reason I get a "invalid escape character syntax " error when compiling this defun.
;; Thu, 01 Mar 2007  15:34
;; make braces indent properly 
(defun powershell-electric-brace (arg)
  "Correct indentation for curly brace"
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (unless
      (save-excursion
	(beginning-of-line)
        (or
	  (looking-at "$\w+") ;Don't do this in a variable 
	  (looking-at "{\s*|[^}]") ;Don't do this in an open procedure block
	  (looking-at "\"[^\"]*$")) ;Don't do this in a open string
       )
    (powershell-indent-line)
    (forward-char 1)
  )
)


(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))


;; Function to control indenting.
(defun powershell-indent-line ()
  "Indent current PowerShell script line"
  (interactive)

  ;; Set the point to beginning of line.
  (beginning-of-line)

  (if (bobp)
      (indent-line-to 0)

    (let ((not-indented t) (lines-back 0) cur-indent)

      (if (looking-at "^[ \t]*}") ; Check for closing brace
	  (progn
	    (save-excursion
	      (forward-line -1)
              (setq lines-back (+ lines-back 1))
	      (if (looking-at "^[ \t]*{") ; If now looking at opening block
		(setq cur-indent (current-indentation)) ;; duplicate indent
		(setq cur-indent (- (current-indentation) powershell-indent-width)))
            )

  ;; Safety check to make sure we don't indent negative.
	    (if (< cur-indent 0)
		(setq cur-indent 0)))

	(save-excursion 
	  (if (looking-at "^[ \t]*{") ; Opening block
	      (progn
		(forward-line -1)
		(setq lines-back (+ lines-back 1))
		(setq cur-indent (current-indentation))
		(setq not-indented nil))

	    (while not-indented
	      (forward-line -1)
              (setq lines-back (+ lines-back 1))
	      (if (looking-at "^[ \t]*}") ; Closing block
		  (progn
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil))
		
		(if (looking-at "^[ \t]*{") ; Opening block
		    (progn
		      (setq cur-indent (+ (current-indentation) powershell-indent-width))
		      (setq not-indented nil))
		      
		(if (looking-at "^[ \t]*\\(if\\|for\\|foreach\\|function\\|else\\|do\\|while\\)\\>")
		    (progn
		      (setq cur-indent (current-indentation))
		      (forward-line 1)
		      (setq lines-back (- lines-back 1))
		      (if (looking-at "^[ \t]*{") ; Has block
			  (setq not-indented nil)
			(if (equal lines-back 0) ; No block
			    (progn
			      (setq cur-indent (+ cur-indent powershell-indent-width))
			      (setq not-indented nil))
			  (setq not-indented nil)))
		      )
		  (if (bobp)
		      (setq not-indented nil)))))))))

      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))



;; only defined one keyword list right now 
(defconst powershell-font-lock-keywords-3
  (list
   '("\\<\\(d\\(?:o\\|efault\\)\\|else\\|f\\(?:oreach\\|unction\\)\\|if\\|switch\\|t\\(?:hrow\\|rap\\)\\|w\\(?:here\\|hile\\)\\)\\>" . font-lock-keyword-face)
   '("$[a-zA-Z0-9_\\.:{}]+\\>" . font-lock-variable-name-face)
   '("\\<\\w+-\\w+\\>" . font-lock-function-name-face)
   '("\\<-\\w+\\>" . font-lock-builtin-face)
   '("@'[A-z0-9\n\t ]+'@" . font-lock-string-face)
   '("@\"[A-z0-9\n\t ]+\"@" . font-lock-string-face)
   '("\\(-\\)\\([a-z][a-zA-Z0-9]+\\)" . font-lock-type-face)) 
  "Maximum highlighting for PowerShell major mode")

(defvar powershell-font-lock-keywords powershell-font-lock-keywords-3
  "Maximum highlighting for PowerShell major mode")

;; is adding punctuation to word syntax appropriate?? 
(defvar powershell-mode-syntax-table
  (let ((powershell-mode-syntax-table (make-syntax-table)))
   (modify-syntax-entry ?. "_" powershell-mode-syntax-table)
   (modify-syntax-entry ?: "_" powershell-mode-syntax-table)
   (modify-syntax-entry ?{ "(" powershell-mode-syntax-table)
   (modify-syntax-entry ?} ")" powershell-mode-syntax-table)
   (modify-syntax-entry ?[ "(" powershell-mode-syntax-table)
   (modify-syntax-entry ?] ")" powershell-mode-syntax-table)
   (modify-syntax-entry ?( "(" powershell-mode-syntax-table)
   (modify-syntax-entry ?) ")" powershell-mode-syntax-table)
   (modify-syntax-entry ?` "\\" powershell-mode-syntax-table)
   (modify-syntax-entry ?_ "w" powershell-mode-syntax-table)
   (modify-syntax-entry ?# "<" powershell-mode-syntax-table)
   (modify-syntax-entry ?\n ">" powershell-mode-syntax-table)
   (modify-syntax-entry ?' "\"" powershell-mode-syntax-table)
    powershell-mode-syntax-table)
  "Syntax for PowerShell major mode")



(defun powershell-mode ()
  "Major mode for editing PowerShell files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table powershell-mode-syntax-table)
  (use-local-map powershell-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(powershell-font-lock-keywords))

  (make-local-variable 'compile-command)
  (setq compile-command (format "PowerShell -noprofile -nologo -command %s"
			 (expand-file-name buffer-file-name)))


   ; Here we register our line indentation function with Emacs. Now Emacs will
   ; call our function every time line indentation is required (like when the
   ; user calls indent-region).
;;  (make-local-variable 'indent-line-function)
;;  (setq indent-line-function 'powershell-indent-line)

   ; Set indentation defaults.
  (make-local-variable 'powershell-indent-width)
  (setq powershell-indent-width 5)

  (setq major-mode 'powershell-mode)
  (setq mode-name "PowerShell")
  (run-hooks 'powershell-mode-hook))



(provide 'powershell-mode)



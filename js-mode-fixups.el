;;; js-mode-fixups.el --- fixups for js-mode quirks, shortcomings, bugs

;; Author: Dino Chiesa
;; Version: 0.1
;; Created: Sat, 31 Mar 2012  09:48
;; Last Updated: <2015-February-16 11:51:35>



;; (defun dino-js-varify (start end)
;;   "Turn the selection into a string var declaration terminated with a semicolon."
;;   (interactive "r")
;;   (goto-char end)
;;   (insert "';")
;;   (goto-char start)
;;   (insert "var x = '")
;;   (backward-char 5)
;;   (js-indent-line))
;;   ;; (yas/expand-snippet elaborated-template (point) (point)))))))))))


(defun dino-js-beautify ()
  "replace the current buffer with the jsBeautify'd version."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "~/js/jsBeautify.js" nil t))


(eval-after-load "js"
  '(progn

     ;; fix broken sorting of imenus
     (defadvice imenu--split-menu (around
                                   js--imenu-split-menu-patch
                                   activate compile)
       ;; This advice will run in all buffers.  Let's may sure we
       ;; actually execute the important bits only when a C# buffer is active.
       (if (string-match "\\.[Jj][Ss]$"  (file-relative-name buffer-file-name))
           (let ((menulist (copy-sequence menulist))
                 keep-at-top)
             (if (memq imenu--rescan-item menulist)
                 (setq keep-at-top (list imenu--rescan-item)
                       menulist (delq imenu--rescan-item menulist)))
             ;; This is the part from the original imenu code
             ;; that puts submenus at the top.  huh? why?
             ;; --------------------------------------------
             ;; (setq tail menulist)
             ;; (dolist (item tail)
             ;;   (when (imenu--subalist-p item)
             ;;     (push item keep-at-top)
             ;;     (setq menulist (delq item menulist))))
             (if imenu-sort-function
                 (setq menulist (sort menulist imenu-sort-function)))
             (if (> (length menulist) imenu-max-items)
                 (setq menulist
                       (mapcar
                        (lambda (menu)
                          (cons (format "From: %s" (caar menu)) menu))
                        (imenu--split menulist imenu-max-items))))
             (setq ad-return-value
                   (cons title
                         (nconc (nreverse keep-at-top) menulist))))
         ;; else
         ad-do-it))




     ;; redefine this fn to recurse on nested function creation,
     ;; To allow imenu to see those fns as well.
     (defun js--pitems-to-imenu (pitems unknown-ctr)
       "Convert list of pitems PITEMS to imenu format"

       (let (imenu-items pitem pitem-type pitem-name subitems)

         (while (setq pitem (pop pitems))
           (setq pitem-type (js--pitem-type pitem))
           (setq pitem-name (js--pitem-strname pitem))
           (when (eq pitem-name t)
             (setq pitem-name (format "Anonymous %s"
                                      (incf (car unknown-ctr)))))

           (cond
            ((memq pitem-type '(function macro))
             (assert (integerp (js--pitem-h-begin pitem)))

             ;; check for children (nested fnc defns)
             (cond
              ((js--pitem-children pitem)
               (setq subitems (js--pitems-to-imenu
                               (js--pitem-children pitem)
                               unknown-ctr))
               (cond
                (subitems
                 ;; DPC index "top" and "bottom" of the enclosure
                 (push (cons "(top)"
                             (js--maybe-make-marker
                              (js--pitem-h-begin pitem)))
                       subitems)

                 (let ((b-end (js--pitem-b-end pitem)))
                   (if b-end
                       (setq subitems
                             (append subitems
                                     (list (cons "(bottom)"
                                                 (js--maybe-make-marker b-end)))
                                     subitems))))

                 (push (cons pitem-name subitems)
                       imenu-items))

                (t
                 (push (cons pitem-name
                             (js--maybe-make-marker
                              (js--pitem-h-begin pitem)))
                       imenu-items))))

              (t
               (push (cons pitem-name
                           (js--maybe-make-marker
                            (js--pitem-h-begin pitem)))
                     imenu-items))))

            ((consp pitem-type) ; class definition
             (setq subitems (js--pitems-to-imenu
                             (js--pitem-children pitem)
                             unknown-ctr))
             (cond (subitems
                    (push (cons pitem-name subitems)
                          imenu-items))

                   ((js--pitem-h-begin pitem)
                    (assert (integerp (js--pitem-h-begin pitem)))
                    (setq subitems (list
                                    (cons "[empty]"
                                          (js--maybe-make-marker
                                           (js--pitem-h-begin pitem)))))
                    (push (cons pitem-name subitems)
                          imenu-items))))

            (t (error "Unknown item type: %S" pitem-type))))

         imenu-items))

     ;; new helper fn to aid in handling commas in var statements
     ;; (defun js--continued-var-decl ()
     ;;        "Helper function for `js--proper-indentation'.
     ;; Return the proper indentation of the current line if it continues
     ;; a var declaration. otherwise nil.
     ;; "
     ;;        (save-excursion
     ;;          (back-to-indentation)
     ;;          (and (not (eq (point-at-bol) (point-min)))
     ;;               (not (looking-at "[{]"))
     ;;               (progn
     ;;                 (js--backward-syntactic-ws) ;; skip back
     ;;                 (and (char-before)
     ;;                      (= (char-before) ?,)
     ;;                      (progn
     ;;                        (back-to-indentation)
     ;;                        (if (looking-at "var ")
     ;;                            (+ (current-column) js-indent-level)
     ;;                          (current-column))))))))


     ;; new fn to move backward one statement
     (defun js--backto-beginning-of-expression ()
       "Move backward over a whole JavaScript expression.
"
       (let ((limit (point)))
         (js--backward-syntactic-ws)
         (if (memq (char-before) '(?\; ?\{)) (forward-char -1))

         (while (not (or (bobp)
                         (progn
                           (js--backward-syntactic-ws) ;; skip back
                           (memq (char-before) '(?\; ?\{ ?\( ?\[)))))

           (backward-sexp))

         (js--forward-syntactic-ws limit)))


     ;; new helper fn
     (defun js--first-stmt-in-curly ()
       "Helper function for `js--proper-indentation'."
       (let (cur-indent close-curly)
         (save-excursion
           (back-to-indentation)
           (setq close-curly (eq (char-after) ?\})
                 cur-indent (current-column))
           (js--backward-syntactic-ws)
           (if (eq (char-before) ?\{)
               (progn
                 (back-to-indentation)
                 (setq cur-indent (current-column))
                 (+ cur-indent (if close-curly 0 js-indent-level)))
             nil))))

;; new helper fn to aid in handling commas in var statements
(defun js--continued-var-decl ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it continues
a var declaration. otherwise nil.
"
  (save-excursion
    (back-to-indentation)
    (and (not (eq (point-at-bol) (point-min)))
         (not (looking-at "[{]"))
         (not (looking-at "var "))
         (progn
           (js--backward-syntactic-ws)
           (and (char-before)
                (= (char-before) ?,)
                (or (js--backto-beginning-of-expression) t);; skip back
                (cond
                 ((looking-at "var ")
                  ;;(+ (current-column) js-indent-level)
                  (+ (current-column) 4) ;; because var always gets +4
                  )
                 (t
                  nil)))))))


(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((js--ctrl-statement-indentation))
          ((js--continued-var-decl))
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (if (looking-at "var ")
                              (+ (current-column) 4)
                          (current-column)))
                         (continued-expr-p
                          (+ (current-column) (* 2 js-indent-level)
                             js-expr-indent-offset))
                         ((looking-at "var ")
                          (+ (current-column) js-indent-level 4))

                         (t
                          (+ (current-column) js-indent-level
                             (pcase (char-after (nth 1 parse-status))
                               (?\( js-paren-indent-offset)
                               (?\[ js-square-indent-offset)
                               (?\{ js-curly-indent-offset))))))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))


;;;;;
;; I think I need to add the following to js--font-lock-keywords-3:
;;
;;    (js--continued-var-decl-matcher
;;     (1 font-lock-variable-name-face nil t))


(defun js--fontlock-continued-var-decl-matcher (limit)
  "Font-lock matcher for variable names 2...N in a multi-line variable declaration.
"
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((found
               (save-excursion
                 (back-to-indentation)
                 (and (not (eq (point-at-bol) (point-min)))
                      (not (looking-at "[{]"))
                      (not (looking-at "var "))
                      (progn
                        (js--backward-syntactic-ws)
                        (and (char-before)
                             (= (char-before) ?,)
                             (not (= (char-before) ?=))
                             (or (js--backto-beginning-of-expression) t);; skip back
                             (cond
                              ((looking-at "var ")
                               t)
                              (t nil))))))))
          (if found
              (re-search-forward
               (concat "\\(" js--name-re "\\)")))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil)))

(defconst js--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@js--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq js--tmp-location (match-end 2))
           (goto-char js--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq js--tmp-location nil)
       (goto-char (point-at-eol)))
     (when js--tmp-location
       (save-excursion
         (goto-char js--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (js--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" js--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" js--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*" js--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" js--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (js--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" js--basic-type-re)
      (list #'js--variable-decl-matcher nil nil nil))

    ;; continued variable decls - dino
    (js--fontlock-continued-var-decl-matcher
     (1 font-lock-variable-name-face nil t))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" js--name-re "\\)?\\s-*(\\s-*"
       js--name-start-re)
      (list (concat "\\(" js--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" js--name-re "\\s-*[,)]")
      (list js--name-re
            '(if (save-excursion (backward-char)
                                 (js--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `js-mode'.")


(defconst js--font-lock-keywords
  '(js--font-lock-keywords-3 js--font-lock-keywords-1
                                   js--font-lock-keywords-2
                                   js--font-lock-keywords-3)
  "Font lock keywords for `js-mode'.  See `font-lock-keywords'.")


))



;; ;; Redefine this to properly handle commas in var statements.
;; (defun js--proper-indentation (parse-status)
;;   "Return the proper indentation for the current line."
;;   (save-excursion
;;     (back-to-indentation)
;;     (cond ((nth 4 parse-status)
;;            (js--get-c-offset 'c (nth 8 parse-status)))
;;           ((nth 8 parse-status) 0) ; inside string
;;           ((js--ctrl-statement-indentation))
;;           ((js--first-stmt-in-curly))
;;           ((js--continued-var-decl))
;;           ((eq (char-after) ?#) 0)
;;           ((save-excursion (js--beginning-of-macro)) 4)
;;           ((nth 1 parse-status)
;;            (let ((same-indent-p (looking-at
;;                                  "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
;;                  (continued-expr-p (js--continued-expression-p)))
;;              (goto-char (nth 1 parse-status))
;;              (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
;;                  (progn
;;                    (skip-syntax-backward " ")
;;                    (when (eq (char-before) ?\)) (backward-list))
;;                    (back-to-indentation)
;;                    (cond (same-indent-p
;;                           (current-column))
;;                          (continued-expr-p
;;                           (+ (current-column) (* 2 js-indent-level)
;;                              js-expr-indent-offset))
;;                          (t
;;                           (+ (current-column) js-indent-level))))
;;                (unless same-indent-p
;;                  (forward-char)
;;                  (skip-chars-forward " \t"))
;;                (current-column))))
;;
;;           ((js--continued-expression-p)
;;            (+ js-indent-level js-expr-indent-offset))
;;           (t 0))))



(provide 'js-mode-fixups)

;;; js-mode-fixups.el ends here

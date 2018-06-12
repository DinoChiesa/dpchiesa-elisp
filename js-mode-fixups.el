;;; js-mode-fixups.el --- fixups for js-mode quirks, shortcomings, bugs

;; Author: Dino Chiesa
;; Version: 0.1
;; Created: Sat, 31 Mar 2012  09:48
;; Last Updated: <2018-May-31 17:21:57>

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


;; (defun js-fixup-font-lock-extend-region ()
;;   "Extend the search region to include an entire variable decl block."
;;   ;; Avoid compiler warnings about these global variables from font-lock.el.
;;   ;; See the documentation for variable `font-lock-extend-region-functions'.
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (save-excursion
;;     (goto-char font-lock-beg)
;;     (when (re-search-forward "\\_<\\(const\\|var\\|let\\)\\_>" nil t)
;;       (setq font-lock-beg (match-beginning 0)
;;             font-lock-end (js--fixup-end-of-var-decl)))))

(defvar js--fixup-highlight-delay-time 0.07
  "Delay time before applying highlights of var decls.")

(defvar js--fixup-highlight-timer-object nil)

(defun js--fixup-end-of-var-decl ()
  "maybe limit the search by returning the end-position."
  nil)

;; (defun js--fixup-end-of-var-decl ()
;;   "maybe limit the search by returning the end-position."
;;   (let ((orig-point (point)))
;;     (message "js--fixup-end-of-var-decl %s %d" (buffer-name) (point))
;;     (save-excursion
;;       (if (re-search-forward ";" nil t)
;;           (progn
;;             (point))
;;         nil))))


(defun js--fixup-apply-highlight-on-idle (buffer matched highlight)
  "function invoked on idle, after matcher. Trying this as a way to avoid the
spin that occurs if the matcher fn calls `font-lock-apply-highlight' directly."
  (with-current-buffer buffer
    (let ((inhibit-modification-hooks t)
          (inhibit-point-motion-hooks t))
      (with-silent-modifications
        (save-match-data
          (set-match-data matched)
          ;;(message "apply-highlight %s" (prin1-to-string highlight))
          (funcall 'font-lock-apply-highlight highlight)
          )))))

(defun js--fixup-var-decl-matcher (limit)
  "See `js--variable-decl-matcher'. This is a replacement of that.

The typical practice is for font-lock-keywords to call a matcher
function like this, with a specific limit, corresponding to the
position of the end of the regex match, or the end-of-line.

The function is expected to return a single \"match\" , which
font-lock-keywords then uses to set the highlighting on the
matched text. font-lock-keywords again calls the matcher
function, with a new limit, until the matcher returns nil.

This matcher doesn't behave that way. It works via side
effects. Rather than returning match data, this function always
\"fails\" as a matcher - it returns nil, so it is called just
once for each match on var/const/let.

It is invoked with point at the beginning of the variable name
right after the var or const or let statement. It scans to find
variable names, or variable name-and-assignments, and calls
`font-lock-apply-highlight' on each one.

It cannot call `font-lock-apply-highlight' directly because that
seems to cause spins. Not always, but absolutely always in the case of
a var assignment that is not yet completed, and is outside the limit.
This seems to be because `font-lock-apply-highlight' causes
modification of a text property, which is seems to cause
font-lock to start a new fontification run.

To avoid that problem, this function calls `run-with-idle-timer'
to schedule the calls to `font-lock-apply-highlight'.
"
  ;;  (let ((start-point (point)))
  (if js--fixup-highlight-timer-object
      (progn
        (cancel-timer js--fixup-highlight-timer-object)
        (setq js--fixup-highlight-timer-object nil)
        ))

  (let ((first t)
        (matched nil))
    (save-match-data
      (condition-case nil
          (progn
            ;; (message "js--fixup-var-decl-matcher %s %d %s" (buffer-name) (point)
            ;;           (prin1-to-string limit))

            ;; When doing synchronous calls to
            ;; font-lock-apply-highlight, I need to include this line to
            ;; prevent spins.  Not necessary when calling f-l-a-h
            ;; asynchronously.
            ;;
            ;; (narrow-to-region (point-min) limit)

            (forward-comment most-positive-fixnum)

            ;; started getting a spin again!  20180531-1721
            ;; so I disabled the while loop. :<
            ;; (while (or first matched)
            (if (or first matched)
              (setq matched nil)
              (cond
               ((looking-at js--name-re)
                (setq matched (match-data))

                ;; (setq js--fixup-highlight-timer-object
                ;;       (run-with-idle-timer js--fixup-highlight-delay-time nil 'js--fixup-apply-highlight-on-idle (current-buffer) matched '(0 font-lock-variable-name-face) ))
                ;; (font-lock-apply-highlight
                ;;  '(0 font-lock-variable-name-face))
                (goto-char (match-end 0))
                (forward-comment most-positive-fixnum)
                (when (eq (char-after) ?=)
                  (forward-char)
                  (js--forward-expression)
                  (forward-comment most-positive-fixnum))

                (when (eq (char-after) ?,)
                  (forward-char)
                  (forward-comment most-positive-fixnum)
                  t)
                ))
              (setq first nil)
              )
            )

        ;; Conditions to handle
        (scan-error nil)
        (end-of-buffer nil))))

    nil)


(eval-after-load "js"
  '(progn

     ;; fix broken sorting of imenus
     (if (fboundp 'advice-add)
         (advice-add 'imenu--split-menu :around #'js--fixup-imenu-sorting))


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
                     (or
                      (looking-at "var ")
                      (looking-at "const ")
                      (or (js--backward-syntactic-ws) ;; skip back
                          (memq (char-before) '(?\; ?\{ ?\( ?\[)))))))

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
         (not (looking-at "const "))
         (not (looking-at "var "))
         (not (looking-at "let "))
         (progn
           (js--backward-syntactic-ws)
           (and (char-before)
                (= (char-before) ?,)
                (or (js--backto-beginning-of-expression) t);; skip back
                (cond
                 ((looking-at "const ")
                  ;;(+ (current-column) js-indent-level)
                  (+ (current-column) 6) ;; because var always gets +4
                  )
                 ((looking-at "var ")
                  ;;(+ (current-column) js-indent-level)
                  (+ (current-column) 4) ;; because var always gets +4
                  )
                 ((looking-at "let ")
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
                            (if (looking-at "let ")
                              (+ (current-column) 4)
                              (if (looking-at "const ")
                                  (+ (current-column) 6)
                                (current-column)))))
                         (continued-expr-p
                          (+ (current-column) (* 2 js-indent-level)
                             js-expr-indent-offset))
                         ((looking-at "var ")
                          (+ (current-column) js-indent-level 4))
                         ((looking-at "let ")
                          (+ (current-column) js-indent-level 4))
                         ((looking-at "const ")
                          (+ (current-column) js-indent-level 6))
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


(defun js--fixup-fontlock-continued-var-decl-matcher (limit)
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
                      (not (looking-at "let "))
                      (not (looking-at "const "))
                      (progn
                        (js--backward-syntactic-ws)
                        (and (char-before)
                             (= (char-before) ?,)
                             (not (= (char-before) ?=))
                             (or (js--backto-beginning-of-expression) t);; skip back
                             (cond
                              ((looking-at "var ")
                               t)
                              ((looking-at "let ")
                               t)
                              ((looking-at "const ")
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

    ;; ;; variable declarations
    ;; ,(list
    ;;   (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" js--basic-type-re)
    ;;   (list #'js--variable-decl-matcher nil nil nil))

    ;; ;; continued variable decls - dino
    ;; (js--fixup-fontlock-continued-var-decl-matcher
    ;;  (1 font-lock-variable-name-face nil t))

    ;; variable declarations - dino attempt at correct highlighting of
    ;; multiline var/const/let declarations.
    ,(list
      "\\_<\\(const\\|var\\|let\\)\\_>"
      (list #'js--fixup-var-decl-matcher '(js--fixup-end-of-var-decl) nil nil))

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


(defun js--fixup-imenu-sorting (orig-fun &rest args)
  ;; This advice will run in all buffers.  Let's may sure we
  ;; actually execute the important bits only when a JS buffer is active.
  (if (and buffer-file-name
           (string-match "\\.[Jj][Ss]$"  (file-relative-name buffer-file-name)))
      (let ((menulist (copy-sequence (car args)))
            (title (cadr args))
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
        ;; return value
        (cons title
              (nconc (nreverse keep-at-top) menulist)))
    ;; else
    (apply orig-fun args)))


(defun dino-js-beautify ()
  "replace the current buffer with the jsBeautify'd version."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "~/js/jsBeautify.js" nil t))



(provide 'js-mode-fixups)

;;; js-mode-fixups.el ends here

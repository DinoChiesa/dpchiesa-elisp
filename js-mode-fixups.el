;;; js-mode-fixups.el --- fixups for js-mode quirks, shortcomings, bugs

;; Author: Dino Chiesa
;; Version: 0.1
;; Created: Sat, 31 Mar 2012  09:48
;;


;; fixups
;; (eval-after-load "js"
;;   '(progn

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
                       (+ (current-column) js-indent-level))
                      (t
                       nil)))))))

     ;; Redefine this to properly handle commas in var statements.
     (defun js--proper-indentation (parse-status)
       "Return the proper indentation for the current line."
       (save-excursion
         (back-to-indentation)
         (cond ((nth 4 parse-status)
                (js--get-c-offset 'c (nth 8 parse-status)))
               ((nth 8 parse-status) 0) ; inside string
               ((js--ctrl-statement-indentation))
               ((js--first-stmt-in-curly))
               ((js--continued-var-decl))
               ((eq (char-after) ?#) 0)
               ((save-excursion (js--beginning-of-macro)) 4)
               ((nth 1 parse-status)
                (let ((same-indent-p (looking-at
                                      "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                      (continued-expr-p (js--continued-expression-p)))
                  (goto-char (nth 1 parse-status))
                  (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                      (progn
                        (skip-syntax-backward " ")
                        (when (eq (char-before) ?\)) (backward-list))
                        (back-to-indentation)
                        (cond (same-indent-p
                               (current-column))
                              (continued-expr-p
                               (+ (current-column) (* 2 js-indent-level)
                                  js-expr-indent-offset))
                              (t
                               (+ (current-column) js-indent-level))))
                    (unless same-indent-p
                      (forward-char)
                      (skip-chars-forward " \t"))
                    (current-column))))

               ((js--continued-expression-p)
                (+ js-indent-level js-expr-indent-offset))
               (t 0))))
;;))


     (defun js--proper-indentation (parse-status)
       "Return the proper indentation for the current line."
       (save-excursion
         (back-to-indentation)
         (cond ((nth 4 parse-status)
                (js--get-c-offset 'c (nth 8 parse-status)))
               ((nth 8 parse-status) 0) ; inside string
               ((js--ctrl-statement-indentation))
               ((js--first-stmt-in-curly))
               ((js--continued-var-decl))
               ((eq (char-after) ?#) 0)
               ((save-excursion (js--beginning-of-macro)) 4)
               ((nth 1 parse-status)
                (let ((same-indent-p (looking-at
                                      "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                      (continued-expr-p (js--continued-expression-p)))
                  (goto-char (nth 1 parse-status))
                  (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                      (progn
                        (skip-syntax-backward " ")
                        (when (eq (char-before) ?\)) (backward-list))
                        (back-to-indentation)
                        (cond (same-indent-p
                               (current-column))
                              (continued-expr-p
                               (+ (current-column) (* 2 js-indent-level)
                                  js-expr-indent-offset))
                              (t
                               (+ (current-column) js-indent-level))))
                    (unless same-indent-p
                      (forward-char)
                      (skip-chars-forward " \t"))
                    (current-column))))

               ((js--continued-expression-p)
                (+ js-indent-level js-expr-indent-offset))
               (t 0))))


(provide 'js-mode-fixups)

;;; js-mode-fixups.el ends here

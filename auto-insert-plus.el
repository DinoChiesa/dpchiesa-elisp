;;; auto-insert-plus.el --- extensions for auto-insert-mode
;;
;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : October 5, 2023
;; Version    : 1.0
;; Keywords   : template
;; URL        :
;; X-URL      :
;; Last-saved : <2023-October-05 17:50:38>


;; (auto-insert-alist
;;  '(
;;    (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
;;    (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])
;;    (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
;;    (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
;;    (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
;;    (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
;;    (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
;;    (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
;;    (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand]))
;;  )
;; )

(defvar aip/initial-dot-position nil
  "This variable defines, if not nil, the initial position of the dot.
It can be set by the @DOT@ pseudo-variable in a template." )


(defun aip/filename-remove-extension (name &optional extension)
  "Return NAME less its EXTENSION. If the extension is given as second
argument then it is an error for the extension not to be present."
  (let* ((extension (if extension (regexp-quote extension) "\\.[^.]*"))
         (regexp (concat "\\(.*\\)" extension "$")) )
    ;(message regexp)(sleep-for 10)
    (if (string-match regexp name)
        (substring name (match-beginning 1) (match-end 1))
      (error "No extension" name) ) ) )


(defvar aip/expandos-alist
  '(( "@BASEFILENAME@"  (file-name-nondirectory buffer-file-name) )
    ( "@BASEFILENAMELESSEXTENSION@"
      (aip/filename-remove-extension
       (file-name-nondirectory buffer-file-name) ) )
    ( "@FILENAME@"      buffer-file-name )
    ( "@DATE@"          (current-time-string) )
    ( "@HOST@"          (or (getenv "HOST") (getenv "COMPUTERNAME")))
    ( "@AUTHOR@"        (capitalize (or (getenv "USER") (getenv "USERNAME"))))
    ( "@COMMENT-START@" (if comment-start comment-start "") )
    ( "@COMMENT-END@"   (if comment-end comment-end "") )
    ( "@DOT@"           (setq aip/initial-dot-position (match-beginning 0))
                        "" )
    ( "@NIL@"           "" ) ;; for avoiding expansion of "last saved" in a Template

    ;; insert a file and expand its content. Things like FILENAME will
    ;; not be very useful.
    ( "@\\(INSERT\\|INSERTFILE\\)(\\(.+\\))@"
      (let ((filename
             (buffer-substring-no-properties
              (match-beginning 2)
              (match-end 2))))
        (if (file-readable-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (aip/expand-expandos)
              (buffer-substring-no-properties (point-min) (point-max)))
          (concat "The file '" filename "' is not readable"))))

    ( "@ENV(\\(.+\\))@"    (let ((varname
                                (buffer-substring-no-properties
                                 (match-beginning 1)
                                 (match-end 1))))
                           (or (getenv varname) varname)))

    ( "@@"              "@")

    ;; eg:  <ProjectGuid>{@LISP((dino-insert-uuid))@}</ProjectGuid>
    ( "@LISP(\\(.*\\))@"         (let ((sexp (match-string-no-properties 1)))
                                   (if sexp (eval (read sexp)) "")))
    )
  "An alist specifying the variables to recognize and how to replace them.
Elements look like (REGEXP LISP-CODE ...). When a variable is recognized,
using dc-variable-delimiter, it is compared to the REGEXPs (if dc-fast-
-variable-handling is false) and once one is found, the associated forms
are evaluated and the result replaces the occurrence of the variable." )


(defun aip/expand-expandos ()
  "with current file, evaluate and expand all expandos, as defined in
`aip/expandos-alist'.  The effect is to replace @KEYWORD@ by whatever
the expansion is defined as, in a new file."
  (interactive (list 0))
  (goto-char 0)
  (let ((number-of-expanded-variables 0))
    (while (search-forward "@" nil t)
      (backward-char 1)
      (let ((l aip/expandos-alist))
        (while (consp l)
          (let ((regexp (car (car l)))
                (forms (cdr (car l))) )
            (setq l (cdr l))
            ;; Search if it is a known variable
            (if (looking-at regexp)
                (let* ((the-first-match (match-data))
                       (new (eval (cons 'progn forms))) )
                  ;; restore the old match
                  (store-match-data the-first-match)
                  (replace-match new t t)
                  (setq number-of-expanded-variables
                        (+ 1 number-of-expanded-variables) )
                  (setq l nil) )
              (if (null l)
                  (forward-char 1) ) ) ) )))

    (message "Note: %s variable(s) expanded."
             number-of-expanded-variables ) ) )

(defun aip/fixup-auto-insert-alist (alist)
  "xform my-auto-insert-alist into an alist with items of the form
PATTERN . [FILENAME fn], suitable for use with auto-insert-mode."
  (mapcar (lambda (item) (cons (car item) (vector (cdr item) 'aip/expand-expandos)))
          alist))


(provide 'auto-insert-plus)

;;; auto-insert-plus.el ends here

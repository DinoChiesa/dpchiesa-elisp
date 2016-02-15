;;; dcjava.el --- utility functions for working with Java
;;
;; Copyright (C) 2014,2015,2016 Dino Chiesa
;;

(require 's) ;; magnars' long lost string library

(defvar dcjava--load-path (or load-file-name "~/elisp/dcjava.el")
  "For internal use only. ")

(defvar dcjava-cache-dir (file-name-directory dcjava--load-path))
(defvar dcjava-cache-basefilename ".dcjava.classes")
(defvar dcjava-helper-classname-alist nil)
(defvar dcjava-helper-classnames nil
  "list of classes to be able to import")

(defconst dcjava-classname-regex "\\([a-zA-Z_$][a-zA-Z\\d_$]*\\.\\)*[a-zA-Z_$][a-zA-Z\\d_$]*"
  "a regex that matches a Java classname")

;; (defconst dcjava-classname-regex "\\([a-zA-Z_$][a-zA-Z\\d_$]*\\.\\)*[a-zA-Z_$][a-zA-Z\\d_$]*"
;;   "a regex that matches a Java classname")

(defun dcjava-cache-filename ()
  (concat dcjava-cache-dir dcjava-cache-basefilename))

(defun dcjava--filter (condp lst)
     (delq nil
           (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun dcjava--is-class-name (str)
  "returns true if the string appears to be formed like a java class name"
  (string-match dcjava-classname-regex str))


(defun dcjava-reload-classlist ()
  "loads the list of known classes into memory"
  (interactive)
  (setq dcjava-helper-classname-alist nil
        dcjava-helper-classnames
        (dcjava--filter
         'dcjava--is-class-name
         (with-temp-buffer
           (insert-file-contents (dcjava-cache-filename))
           (split-string (buffer-string) "\n" t)))))



(defun dcjava--list-from-fully-qualified-classname (class)
  "given a fully-qualified java CLASS name, returns a list of two strings: the unqualified classname followed by the package name"
  (let* ((parts (split-string class "\\." t))
         (rlist (reverse parts))
         (last (car rlist)))
    (list last (mapconcat 'identity (reverse (cdr rlist)) "."))))


(defun dcjava-get-helper-classname-alist ()
  "returns the alist for the java class names"
  (or dcjava-helper-classname-alist
      (setq dcjava-helper-classname-alist
            (mapcar
             'dcjava--list-from-fully-qualified-classname
             dcjava-helper-classnames))))


(defun dcjava-auto-add-import ()
  "adds an import statement for the class or interface at point, if possible."
  (interactive)
  (let* ((symbol-name (substring-no-properties (thing-at-point 'word))
                      ;; (save-excursion
                      ;;   (beginning-of-thing 'symbol)
                      ;;   (set-mark (point))
                      ;;   (forward-word)
                      ;;   (buffer-substring-no-properties (mark) (point)))
                      )
         (matching-pair
          (assoc symbol-name (dcjava-get-helper-classname-alist))))
    (cond
     (matching-pair
      (let* ((package-name (cadr matching-pair))
             (import-statement
              (concat "import " package-name "." symbol-name ";")))

        (save-excursion
          (if (not (re-search-backward (concat "^" import-statement) nil t))
              (let ((want-extra-newline nil))
                (if (re-search-backward "^import" nil t)
                    (end-of-line)
                  (beginning-of-buffer)
                  (while (looking-at "^//")
                    (forward-line))
                  (setq want-extra-newline t))
                (newline)
                (insert import-statement)
                (if want-extra-newline (newline))
                (message import-statement))))))
     (t
      (message "did not find class %s" symbol-name)))
    ))


(defun dcjava-learn-new-import ()
  "learns a new import statement for the fully-qualified classname or import statement at point, if possible."
  (interactive)
  (let ((import-stmt
         (save-excursion
           (beginning-of-line)
           (and (looking-at (concat "import[\t ]+" dcjava-classname-regex
                                    "[\t ]*;"))
                (s-chop-suffix
                 ";"
                 (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))))
    (if import-stmt
        (let ((parts (split-string import-stmt "[\t ]+" t)))
          (if (eq (length parts) 2)
              (let ((classname (nth 1 parts)))
                (if (not (member classname dcjava-helper-classnames))
                    (progn
                      (write-region (nth 1 parts) nil (dcjava-cache-filename) 'append)
                      (dcjava-reload-classlist))
                  (message "that class is already known")))
            (message "that does not look like a java class name")))
      (message "that does not look like an import statement"))))



;; (defun dino-find-java-source-in-dir (dir classname)
;;   "find a java source file in a DIR tree, based on the CLASSNAME. This is
;; a simple wrapper on the shell find command."
;;   (s-trim-right
;;    (shell-command-to-string
;;     (concat "find " dir " -name " classname ".java"))))

(defun dcjava-find-java-source-in-dir (dir classname)
  "find a java source file in a DIR tree, based on the CLASSNAME. This is
a simple wrapper on the shell find command."
  (let ((argument (if (s-contains? "/" classname)
                      " -path \\*" " -name ")))
    (s-trim-right
     (shell-command-to-string
      (concat "find " dir argument classname ".java")))))

(defvar dcjava-wacapps-root "~/dev/wacapps/new/api_platform")

(defun dcjava-find-wacapps-java-source-for-class-at-point ()
  "find a java source file that defines the class named at point,
in the wacapps dir tree, referred to by `dcjava-wacapps-root' . This is
a wrapper on the shell find command."
  (interactive)
  (let ((filename
         (save-excursion
           (re-search-backward "[ \t]" nil t)
           (forward-char)
           (if (looking-at dcjava-classname-regex)
               (replace-regexp-in-string
                (regexp-quote ".") "/"
                (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                t t)))))
    (if (not filename)
        (setq filename (substring-no-properties (thing-at-point 'word))))
    (if filename
        (setq filename (dcjava-find-java-source-in-dir dcjava-wacapps-root filename)))
    (if (and filename
             (not (equal "" filename))
             (file-exists-p filename))
        (find-file filename)
      (message "no file"))))



(provide 'dcjava)

;;; dcjava.el ends here

;;; dcjava.el --- utility functions for working with Java
;;
;; Copyright (C) 2014-2016 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2014?
;; Modified   : February 2016
;; Version    : 1.4
;; Keywords   : apigee
;; Requires   : s.el
;; License    : New BSD
;; X-URL      : https://github.com/dpchiesa/elisp
;; Last-saved : <2016-February-19 23:20:29>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with Java code in emacs. I never got into the full development
;; environment of Java (JDEE) because it was too unweildy and fragile
;; for me, when I examined it.  Instead I jut code Java in a text
;; editor, and set some basic defaults for C-style, smarter-compile,
;; flycheck, and so on.
;;
;; This module adds a few extra things to that basic set up:
;;
;;  - `dcjava-auto-add-import' adds an import statement to the current
;;    file, if necessary, based on the short name of the class under
;;    point. When using "Document", the module will add an import for
;;    org.w3c.dom.Document.  If there are multiple Document classes,
;;    the user will geta popup choice.
;;
;;  - `dcjava-sort-import-statements' sort the import statements
;;
;;  - `dcjava-find-java-source-in-dir' finds a Java file in a dir
;;    tree based on its short name or fully-qualified name.
;;
;;
;; There are a few helper functions:
;;
;;  - `dcjava-learn-new-import' adds a class to the known list of
;;    classes that can be imported by `dcjava-auto-add-import' .
;;
;;  - `dcjava-reload-classlist' loads the list of known classes from
;;    the cache file
;;
;;
;; Sunday, 14 February 2016, 16:07
;;
;; TODO: in `dcjava-auto-add-import', when there are multiple
;; choices, present the choice intelligently. If the file already has an
;; import for jackson from codehaus, I don't want the chooser to ask me
;; if I wanna use jackson from fasterxml. It should automatically use
;; the one I am already using. Can use a simple heuristic.
;;
;;
;;; License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;; Code:
;;


(require 's) ;; magnars' long lost string library

(defvar dcjava--load-path (or load-file-name "~/elisp/dcjava.el")
  "For internal use only. ")

(defvar dcjava-cache-dir (file-name-directory dcjava--load-path))
(defvar dcjava-cache-basefilename ".dcjava.classes")
(defvar dcjava-helper-classname-alist nil)
(defvar dcjava-helper-classnames nil
  "list of classes to be able to import")
(defconst dcjava--classname-regex "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\.\\)*[a-zA-Z_$][a-zA-Z0-9_$]*"
  "a regex that matches a Java classname or package name")

(defconst dcjava--import-stmt-regex (concat "import[\t ]+" dcjava--classname-regex
                                    "[\t ]*;")
  "a regex that matches a Java import statement")

(defconst dcjava--package-stmt-regex (concat "package[\t ]+" dcjava--classname-regex
                                    "[\t ]*;")
  "a regex that matches a Java package statement")

;; (defconst dcjava-classname-regex "\\([a-zA-Z_$][a-zA-Z\\d_$]*\\.\\)*[a-zA-Z_$][a-zA-Z\\d_$]*"
;;   "a regex that matches a Java classname")

(defun dcjava-cache-filename ()
  (concat dcjava-cache-dir dcjava-cache-basefilename))

(defun dcjava--filter (condp lst)
  "filters the list LST, removing each item for which condp returns nil"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun dcjava--is-class-name (str)
  "returns true if the string appears to be formed like a java class name"
  (string-match dcjava--classname-regex str))


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
  "given a fully-qualified java classname CLASS, returns a list of two strings: the unqualified classname followed by the package name"
  (let* ((parts (split-string class "\\." t))
         (rlist (reverse parts))
         (last (car rlist)))
    (list last (mapconcat 'identity (reverse (cdr rlist)) "."))))


(defun dcjava--xform-alist (lst)
  "transform the list of to combine items that share a common classname"
  (let ((new-list ())
        item)
    (while (setq item (car lst))
      (let* ((classname (car item))
             (rest (cadr item))
             (found (assoc classname new-list)))
        (if (not found)
            (setq new-list (cons item new-list))
          (setcdr found (cons rest (cdr found)))))
      (setq lst (cdr lst)))
    new-list))


(defun dcjava-get-helper-classname-alist ()
  "returns the alist for the java class names. Computes it just-in-time if necessary."
  (or dcjava-helper-classname-alist
      (setq dcjava-helper-classname-alist
            (dcjava--xform-alist
             (mapcar
              'dcjava--list-from-fully-qualified-classname
              dcjava-helper-classnames)))))


(defun dcjava-sort-import-statements ()
  "sorts the import statements in a file."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward dcjava--import-stmt-regex nil t)
        (let ((start nil))
          (beginning-of-line)
          (setq start (point))
          (while (re-search-forward dcjava--import-stmt-regex nil t))
          (forward-line)
          (beginning-of-line)
          (sort-lines nil start (point)))
      (mesage "cannot find import statements"))))


(defun dcjava--gen-import-regex (package-name &optional symbol)
  "returns a regex that matches an import for a given java class defined by a package name and a symbol.  If the symbol is null, then the package-name is treated as a fully-qualified classname."
  (concat "^import[\t ]+"
          (regexp-quote
           (if symbol (concat package-name "." symbol)
             package-name))
          "[\t ]*;"))


(defun dcjava-add-one-import-statement (package-name &optional symbol-name)
  "add one import statement, append to the list of imports at or near beginning-of-buffer.
If the symbol is null, then the package-name is treated as a fully-qualified classname."
  (let ((import-statement
         (concat "import "
                 (if symbol-name
                     (concat package-name "." symbol-name)
                   package-name)
                 ";"))
        (import-regex (dcjava--gen-import-regex package-name symbol-name)))
    (save-excursion
      (if (re-search-backward import-regex nil t)
          (message (concat "already have " package-name "." symbol-name))
        (let ((want-extra-newline nil))
          (if (re-search-backward dcjava--import-stmt-regex nil t)
              (end-of-line)
            (beginning-of-buffer)
            (if (re-search-forward dcjava--package-stmt-regex nil t)
                (progn (forward-line) (beginning-of-line)))
            ;; naively skip-comments. this breaks if you use /*
            (while (looking-at "^//")
              (forward-line))
            (setq want-extra-newline t))
          (newline)
          (insert import-statement)
          (if want-extra-newline (newline))
          (message import-statement))))))



(defun dcjava--generate-menu (candidates)
  "Generate a menu suitable for use in `x-popup-menu' from the
list of candidates. Each item in the list of candidates is a
string, a fully-qualified class name.

The output is a list like this:

  (\"Add Import...\"
    (\"Ignored pane title\"
      (\"import a.b.c.Class;\" \"a.b.c.Class\")
      (\"import x.y.z.Class;\" \"x.y.z.Class\")))

The result of the choice is the cdr of the selected item. In this case, it
will be something like (\"x.y.z.Class\") .

"
  (let ((items (mapcar #'(lambda (elt)
                           (list (concat "import " elt ";") elt))
                       candidates)))
    ;; this works with x-popup-menu
    (setq items (cons "Ignored pane title" items))
    (list "Add import..." items)))


(defun dcjava--get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun dcjava-add-import-statement-from-choice (package-names symbol-name)
  "present a choice for an import statement to add, then add the chosen one."
  (let* ((candidates (mapcar #'(lambda (elt)
                                (concat elt "." symbol-name))
                            package-names))
         (chosen (x-popup-menu (dcjava--get-menu-position)
                               (dcjava--generate-menu candidates))))
    (when chosen ;; actually a list containing a single string (classname)
      (dcjava-add-one-import-statement (car chosen)))))


(defun dcjava-auto-add-import ()
  "adds an import statement for the class or interface at point, if possible."
  (interactive)
  (let* ((symbol-name (substring-no-properties (thing-at-point 'word)))
         (matching-pair
          (assoc symbol-name (dcjava-get-helper-classname-alist))))
    (cond
     (matching-pair
      (let* ((package-names (cdr matching-pair))
             (num-pkgs (length package-names)))
        (if (= num-pkgs 1)
            ;; add the import statement
            (dcjava-add-one-import-statement (car package-names) symbol-name)
          (dcjava-add-import-statement-from-choice package-names symbol-name))))
     (t
      (message "did not find class %s" symbol-name)))
    ))


(defun dcjava-learn-new-import ()
  "learns a new import statement for the import statement at point, if possible."
  (interactive)
  (let ((import-stmt
         (save-excursion
           (beginning-of-line)
           (and (looking-at dcjava--import-stmt-regex)
                (s-trim
                 (s-chop-suffix
                  ";"
                  (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))))
    (if import-stmt
        (let ((parts (split-string import-stmt "[\t ]+" t)))
          (if (eq (length parts) 2)
              (let ((classname (nth 1 parts)))
                (if (not (member classname dcjava-helper-classnames))
                    (progn
                      (write-region
                       (concat (nth 1 parts) "\n")
                       nil (dcjava-cache-filename) 'append)
                      (dcjava-reload-classlist))
                  (message "a class by that name is already known")))
            (message "Inconceivable! that does not look like a java class name")))
      (message "that does not look like an import statement"))))


;; ==================================================================


(defun dcjava-find-java-source-in-dir (dir classname)
  "find a java source file in a DIR tree, based on the CLASSNAME. This is
a simple wrapper on the shell find command."
  (let ((modified-classname (s-replace "." "/" classname)))
    (let ((argument (if (s-contains? "/" modified-classname)
                        " -path \\*" " -name ")))
      (s-trim-right
       (shell-command-to-string
        (concat "find " dir argument modified-classname ".java"))))))


(defvar dcjava-wacapps-root "~/dev/wacapps/new/api_platform")

(defun dcjava-find-wacapps-java-source-for-class-at-point ()
  "find a java source file that defines the class named at point,
in the wacapps dir tree, referred to by `dcjava-wacapps-root' . This is
a wrapper on the shell find command. Bug: does not handle errors properly
when not on a full java class or package name. "
  (interactive)
  (let ((filename
         (save-excursion
           (if (re-search-backward "[ \t]" (line-beginning-position) 1)
               (forward-char))
           (if (looking-at dcjava--classname-regex)
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

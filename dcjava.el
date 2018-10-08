;;; dcjava.el --- utility functions for working with Java
;;
;; Copyright (C) 2014-2016 Dino Chiesa and Apigee Corporation, 2017 Google, Inc.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2014?
;; Modified   : February 2016
;; Version    : 1.4
;; Keywords   : apigee
;; Requires   : s.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/dpchiesa/elisp
;; Last-saved : <2018-October-08 10:12:29>
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
(defvar dcjava-helper-classname-alist nil
  "the alist of short classnames related to fully-qualified type names")
(defvar dcjava-helper-classnames nil
  "list of classes to be able to import")
(defconst dcjava--classname-regex "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\.\\)*[a-zA-Z_$][a-zA-Z0-9_$]*"
  "a regex that matches a qualified or unqualified classname or package name")

(defconst dcjava--qualified-classname-regex "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\.\\)+[a-zA-Z_$][a-zA-Z0-9_$]*"
  "a regex that matches a qualified classname (with package prefix)")

(defconst dcjava--import-stmt-regex (concat "import[\t ]+" dcjava--classname-regex
                                    "[\t ]*;")
  "a regex that matches a Java import statement")

(defconst dcjava--package-stmt-regex (concat "package[\t ]+" dcjava--classname-regex
                                    "[\t ]*;")
  "a regex that matches a Java package statement")

(defconst dcjava--edge-of-symbol-regex
  "[ \t(,\\[=]"
  "A regex that matches the leading edge of a java symbol or classname")


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
        (delete-dups
         (dcjava--filter
          'dcjava--is-class-name
          (with-temp-buffer
            (insert-file-contents (dcjava-cache-filename))
            (split-string (buffer-string) "\n" t))))))


(defun dcjava--list-from-fully-qualified-classname (classname)
  "given a fully-qualified java CLASSNAME, returns a list of two strings: the unqualified classname followed by the package name"
  (let* ((parts (split-string classname "\\." t))
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
              (or dcjava-helper-classnames (dcjava-reload-classlist)))))))


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

(defun dcjava-add-one-import-statement (package-name &optional symbol-name want-learn)
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
          (if want-learn
              (dcjava-learn-new-import))
          (dcjava-sort-import-statements)
          (if want-extra-newline (newline))
          (message import-statement))))))


(defun dcjava--generate-menu (candidates &optional format heading)
  "Generate a menu suitable for use in `x-popup-menu' from the
list of candidates. Each item in the list of CANDIDATES is a
string. The FORMAT is a function that formats the displayable menu choice;
it is called once for each candidate.  The HEADING is a string used in the
result, which will appear as a menu heading.

For example, calling it with candidates ('(\"one\" \"two\")) and no additional
areguments will return a structure like this:

  (\"Select a Choice...\"
    (\"Ignored pane title\"
      (\"one\" \"one\")
      (\"two\" \"two\")))

Calling it with ('(\"Class1\" \"Class2\")
                 #'(lambda (x) (concat \"import \" x \";\"))
                 \"Add import\")

...will return output like this:

  (\"Add Import...\"
    (\"Ignored pane title\"
      (\"import a.b.c.Class;\" \"a.b.c.Class\")
      (\"import x.y.z.Class;\" \"x.y.z.Class\")))

The result of the choice is the cdr of the selected item. In this case, it
will be something like (\"x.y.z.Class\") .

"

  (let ((items (mapcar #'(lambda (elt) (list (funcall (or format 'identity) elt) elt))
                       candidates)))
    ;; this works with x-popup-menu
    (setq items (cons "Ignored pane title" items))
    (list (or heading "Select a Choice...") items)))


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
                               (dcjava--generate-menu candidates
                                                      #'(lambda (x) (concat "import " x ";"))
                                                      "Add import..."))))
    (when chosen ;; actually a list containing a single string (classname)
      (dcjava-add-one-import-statement (car chosen)))))


(defun dcjava--classname-at-point ()
  "returns the fully-qualified classname under point"
  (save-excursion
    (if (re-search-backward dcjava--edge-of-symbol-regex (line-beginning-position) 1)
        (forward-char))
    (if (looking-at dcjava--classname-regex)
        ;;(replace-regexp-in-string
        ;;(regexp-quote ".") "/"
        (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))


(defun dcjava-auto-add-import ()
  "adds an import statement for the class or interface at point, if possible.
If the class at point is fully-qualified, just adds an import for that. Otherwise,
uses a cached list to lookup the package/class to import."
  (interactive)
  (let ((classname (dcjava--classname-at-point)))
    (cond
     (classname
      (cond
       ((string-match dcjava--qualified-classname-regex classname)
        (let* ((pair (dcjava--list-from-fully-qualified-classname classname))
               (basename (car pair)))
          (dcjava-add-one-import-statement classname nil t)
          ;; unqualify the classname under point
          (save-excursion
            (if (re-search-backward dcjava--edge-of-symbol-regex (line-beginning-position) 1)
                (forward-char))
            (if (re-search-forward (regexp-quote classname))
                ;; replace the fully-qualified classname with the basename
                (replace-match basename)))))
       (t
        (let ((matching-pair (assoc classname (dcjava-get-helper-classname-alist))))
          (cond
           (matching-pair
            (let* ((package-names (cdr matching-pair))
                   (num-pkgs (length package-names)))
              (if (= num-pkgs 1)
                  ;; add the import statement
                  (dcjava-add-one-import-statement (car package-names) classname)
                (dcjava-add-import-statement-from-choice package-names classname))))
           (t
            (message "did not find class %s" classname)))
          )
        )))
     (t
      (message "did not find a classname under point"))
     )))

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
a simple wrapper on the shell find command. The return value is a list of
strings (filenames). "
  (let* ((modified-classname (s-replace "." "/" classname))
         (argument (if (s-contains? "/" modified-classname)
                       " -path \\*" " -name "))
         (result
          (s-trim-right
           (shell-command-to-string
            (concat "find " dir argument modified-classname ".java")))))
    (if (s-blank? result) nil
      (s-split "\n" result))))

(defun dcjava--is-directory (dir-name)
  "Tests to see whether a name refers to a directory"
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))

(defun dcjava-path-of-api-platform ()
  "returns a string representing the api_platform directory for the
given current working directory."
  (interactive)
  (let ((to-find "api_platform"))
    (let ((path
           (dcjava-insure-trailing-slash
            (let ((maybe-this (concat (file-name-directory default-directory) to-find)))
              (if (dcjava--is-directory maybe-this)
                  (file-name-directory default-directory)
                (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
                      r)
                  (while (and elts (not r))
                    (if (string= (car elts) to-find)
                        (setq r (reverse (cdr elts)))
                      (setq elts (cdr elts))))
                  (if r
                      (mapconcat 'identity r "/") )))))))
      (and path (file-truename path)))))

(defun dcjava-insure-trailing-slash (path)
  "Insure the given path ends with a slash. This is useful with
`default-directory'. Setting `default-directory' to a value that
does not end with a slash causes it to use the parent directory.
"
  (and path
       (if (s-ends-with? "/" path) path (concat path "/"))))



;;(defvar dcjava-wacapps-root "~/dev/wacapps/new/api_platform")
(defvar dcjava-cps-root "~/dev/wacapps/cps")


(defun dcjava-find-file-from-choice (flist)
  "present a choice for an import statement to add, then add the chosen one."
  (let ((chosen (x-popup-menu (dcjava--get-menu-position)
                              (dcjava--generate-menu flist
                                                     nil
                                                     "Open a file..."))))
    (when chosen ;; actually a list containing a single string (filename)
      (progn (find-file (car chosen))
             (message "open file %s" (car chosen))))))



(defun dcjava-find-wacapps-java-source-for-class (classname)
  "find a java source file in the source tree that defines the class named
by CLASSNAME. Prompts if no classname is provided. This fn is a wrapper on
the shell find command. "

  (interactive "P")
  ;; prompt for classname if not specified
  (if (not classname)
      (setq classname (read-string "Class to find: " nil)))

  (let* ((src-root (concat (dcjava-insure-trailing-slash (dcjava-path-of-api-platform)) "api_platform/"))
    (filenames (and classname
                    (or
                     (dcjava-find-java-source-in-dir src-root classname)
                     (dcjava-find-java-source-in-dir dcjava-cps-root classname)))))

    (if filenames
        (let ((numfiles (length filenames)))
          (if (and (eq numfiles 1))
              (if (file-exists-p (car filenames))
                  (progn (find-file (car filenames))
                         (message "open file %s" (car filenames)))
                (message "E_NOEXIST %s" classname))
            (dcjava-find-file-from-choice filenames)))
      (message "no file for class %s" classname))))


(defun dcjava-find-wacapps-java-source-for-class-at-point ()
  "find a java source file in the wacapps dir tree that defines the
class named by the `thing-at-point'. This fn is a wrapper on the shell
find command.

When invoked interactively, uses the class name at point. When
none is found, then prompts for a classname.

When multiple source files are found, prompts user with a list to
select from.
"
  (interactive)
  (let* ((classname
          (or
           (dcjava--classname-at-point)
           (let ((thing (thing-at-point 'word)))
             (if thing (substring-no-properties thing)))
           (read-string "Class to find: " nil)))
         )
    (dcjava-find-wacapps-java-source-for-class classname)))


(defun dcjava-insert-inferred-package-name ()
  "infers the package name from the directory structure, then inserts the package statement
at the top of the file."
  (interactive)
  (let ((elts (delq "" (reverse (split-string (file-name-directory default-directory) "/"))))
        (package-root-dir-names '("org" "com" "io" "net"))
        dir-stack
        inferred-package-name)
    (while (and elts (not inferred-package-name))
      (if (member (car elts) package-root-dir-names)
          (setq inferred-package-name (mapconcat 'identity (push (car elts) dir-stack) "."))
        (push (car elts) dir-stack)
        (setq elts (cdr elts))))
    (if inferred-package-name
        (save-excursion
          (beginning-of-buffer)
          ;; naively skip-comments. this breaks if you use /*
          (while (looking-at "^//")
            (forward-line))
          (newline)
          (insert (concat "package " inferred-package-name ";"))
          (newline)
          ))))


(provide 'dcjava)

;;; dcjava.el ends here

;;; dino-dired-fixups.el --- fixups for dired mode
;;
;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  10:31
;; Package-Requires: ()
;; Version: 2017.03.01
;; URL: https://github.com/DinoChiesa/dpchiesa-elisp/blob/master/dino-dired-fixups.el
;; License: Public Domain
;; Keywords: dired

;;; Commentary:

;; This module extends the basic dired to do sorting on extension and
;; size, in addition to name and timestamp. Use the s key to cycle through
;; sort modes.

;; To use it, place this in your .emacs file:
;;
;; (require 'dired)
;; (require 'dino-dired-fixups)
;;


(require 'cl)
(require 'dired)
(require 'dired-aux)
(require 'ls-lisp)

;; (defun ls-lisp-format-time (file-attr time-index now)
;;   "################")

(defvar dino-dired-switches-to-cycle '("t" "U" "S" "")
  "The one-character switches to cycle through for dired with `dired-toggle-sort'. ")
;; New toggle scheme: add/remove a trailing " -t" " -S", " -X" (maybe) or " -U".
;; Sort by:
;; -t = last mod time
;; -U = time of file creation
;; -S = size
;; -X = extension (not always available - conditionally added later)
;; default = name


(defun ls-lisp-format-file-size (f-size human-readable)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb.  Some files are larger than that.
"
  (if (or (not human-readable) (< f-size 1024))
      (format (if (floatp f-size) " %11.0f" " %11d") f-size)
    (let (post-fixes)
      (do ((f-size (/ f-size 1024.0) (/ f-size 1024.0))
           ;; kilo, mega, giga, tera, peta, exa
           (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
          ((< f-size 1024) (format " %10.0f%s"  f-size (car post-fixes)))))))

;; On MacOS, the builtin ls program does not do the -X option. (lame)
;; The MacPorts or brew versions of GNU ls does. If it exists, use it.
;; the -X is used by dired-fixups for sorting by extension.
(if (eq system-type 'darwin)
    (let ((candidate-dirs (list "/opt/local/bin" "/usr/local/opt/coreutils/bin")))
      (while candidate-dirs
        (let ((candidate (concat (file-name-as-directory (car candidate-dirs)) "gls")))
          (if (file-exists-p candidate)
              (setq ls-lisp-use-insert-directory-program t
                    dino-dired-switches-to-cycle (reverse (cons "X" (reverse dino-dired-switches-to-cycle)))
                    insert-directory-program candidate)))
        (setq candidate-dirs (cdr candidate-dirs)))))


(defun dino-dired-next-sorting-switch (old)
  "returns the next sorting switch, a one-character string, after OLD"
  (let ((found (member old dino-dired-switches-to-cycle)))
    (or (and found
             (or (cadr found) "t"))
        "t")))


(defun dino-dired-sort-cycle (&optional arg)
  "This is intended as a replacement for the `dired-sort-toggle' fn from
dired.el. Normally, dired sorts on either name or time, and you can
swap between those two (eg, toggle) with the s key. This function allows
sorting on name, size, mod time, create time, and (maybe)
extension. Cycling works the same, with the s key. So it's no
longer toggling, but rotating.

With optional ARG, do not cycle. Instead just use that arg as the
new switch. It should be one of [tUXS] . X is only legal on a
platform with gls. Invoking with an optional arg can be useful
when called from a dired-mode hook fn, to set the default /
initial sort.

"
  (interactive)
  (setq dired-actual-switches
        (let (case-fold-search)
          (cond
           ((and arg (member arg dino-dired-switches-to-cycle))
            (dino-dired-generate-new-sorting-switch-string arg)) ;; set new switch to passed-in value

           (arg
            (dino-dired-generate-new-sorting-switch-string "")) ;; illegal passed-in value, sort by name

           ;; Both of the next 2 conds rotate among the sort switches.

           ;; this case handles the situation in which the options to ls are not concatted.
           ((string-match " " dired-actual-switches)
            (let ((n 0)
                  (L (1- (length dino-dired-switches-to-cycle)))
                  result)
              (while (and (< n L) (not result))
                (let ((cur-switch (nth n dino-dired-switches-to-cycle)))

                  (if (and (not (string= cur-switch ""))
                           (string-match (concat " -" cur-switch "\\'") dired-actual-switches))
                      (setq result
                            (concat
                             (substring dired-actual-switches 0 (match-beginning 0))
                             " -" (dino-dired-next-sorting-switch cur-switch))))))
              (or result
                  (concat dired-actual-switches " -t"))))

           ;; this case handles the situation in which the options to ls are concatted.
           (t
            (let* ((switches-regex
                    (concat "[" (mapconcat 'identity dino-dired-switches-to-cycle "") "]"))
                   (old-sorting-switch
                    (if (string-match switches-regex dired-actual-switches)
                        (substring dired-actual-switches (match-beginning 0) (match-end 0))
                      "ZZ")))

              (dino-dired-generate-new-sorting-switch-string
               (dino-dired-next-sorting-switch old-sorting-switch)))))))

  (dino-dired-sort-set-modeline)
  (revert-buffer))


(defun dino-dired-generate-new-sorting-switch-string (new)
  (concat
   "-l"
   ;; strip -l and any other sorting switches
   (dired-replace-in-string
    (concat "[-l" (mapconcat 'identity dino-dired-switches-to-cycle "") "]")
    ""
    (or dired-actual-switches ""))
   new))


(defun dino-dired-sort-set-modeline ()
 "This is a redefinition of the fn from `dired.el'. This one
properly provides the modeline in dired mode, supporting the new
search modes defined in the new `dino-dired-sort-cycle'.
"
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                   "Dired by mtime")
                  ((string-match "^-[^U]*U[^U]*$" dired-actual-switches)
                   "Dired by ctime")
                  ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                   "Dired by ext")
                  ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                   "Dired by sz")
                  ((string-match "^-[^SXUt]*$" dired-actual-switches)
                   "Dired by name")
                  (t
                   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))


;; nifty utility function
(defun dino-dired-do-find (&optional arg)
  "Visit each of the marked files, or the file under the point, or when
prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list
    (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))


(defun mode-for-buffer (&optional buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer (or buffer-or-string (current-buffer))
     major-mode))

(defun dino-dired-copy-or-move-other-window (fn)
  "copy or move the marked files to another directory."
  (unless (eq major-mode 'dired-mode)
    (error "works only when current-buffer is in dired-mode"))
  (let ((other-visible-dired-buffers
         (delq nil (mapcar #'(lambda (w)
                              (let* ((b (window-buffer w))
                                     (m (mode-for-buffer b)))
                                (and (eq m 'dired-mode)
                                     (not (eq b (current-buffer)))
                                     b)))
                           (window-list)))))

    (unless (= (length other-visible-dired-buffers) 1)
      (error "Can copy only if exactly 2 dired windows are visible"))

    (let ((dst-dir (expand-file-name (with-current-buffer (car other-visible-dired-buffers)
                                       default-directory))))
      (mapc #'(lambda (f) (funcall fn f dst-dir 1))
              (dired-get-marked-files nil))
       (with-current-buffer (car other-visible-dired-buffers)
         (revert-buffer))
       (revert-buffer))))


(defun dino-dired-move-file-to-dir-in-other-window (&optional arg)
"If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then move
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit.
"
  (interactive "P")
  (dino-dired-copy-or-move-other-window #'rename-file))

(defun dino-dired-copy-file-to-dir-in-other-window (&optional arg)
"If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then copy
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit.
"
  (interactive "P")
    (dino-dired-copy-or-move-other-window #'copy-file))


;; Auto-update capability with timer

(defvar dired-file-modification-hash (make-hash-table :test 'equal))

(defun maybe-revert-dired-buffers ()
  (walk-windows
   #'(lambda (win)
       (with-selected-window win
         (when (eq major-mode 'dired-mode)
           (let ((mod (gethash default-directory dired-file-modification-hash)))
             (unless (and mod
                          (equal mod (nth 5 (file-attributes
                                             default-directory))))
               (setq mod (nth 5 (file-attributes default-directory)))
               (puthash default-directory mod dired-file-modification-hash)
               (dired-revert))))))
   'no-mini 'all-frames))

(run-with-idle-timer 1 t 'maybe-revert-dired-buffers)


;; This is a well-known fn name normally provided by dired-x, which I do
;; not use.  So I provide my own definition. When opening a file from
;; dired, it will use the command guessed here.
(defun dired-guess-shell-command (prompt files)
  "invoked by `dired-read-shell-command' to read the shell command
for a given file or set of files. This function makes an intelligent guess."
  (if (eq (length files) 1)

      (let* ((file (car files))
             (prompt (concat "! on " file " "))
             (ext (file-name-extension file))
             (initial
              (if (member ext '("png" "jpg" "gif"))
                  ;;(concat "open -a seashore " (car files))
                  (concat "open -a preview " (car files))
                "")))
        (read-shell-command prompt initial))

    (let ((prompt (concat "! on ["
                          (mapconcat 'identity files " ")
                          "] ")))

      (read-shell-command prompt))))



(defun dino-dired-kill-new-file-contents (&optional arg)
  "copies the contents of the marked file into the kill-ring"
  (interactive "P")
  (let ((filename-list (dired-get-marked-files nil arg)))
      (mapc #'(lambda (f)
               (with-temp-buffer
                 (insert-file-contents f)
                 (kill-new
                  (buffer-substring-no-properties (point-min) (point-max)))))
            filename-list)))


;; This fn redefined here to change the doc string, only.
;; The current doc string is not helpful.
(defun dired-do-touch (&optional arg)
  "Change the timestamp of the marked (or next ARG) files.
This calls touch. With prefix, accepts timestamp in [[CC]YY]MMDDhhmm[.SS] format.
When invoked interactively, you can pull the current file timestamp of the file at
point into the minibuffer, by typing M-n ."
  (interactive "P")
  (dired-do-chxxx "Timestamp" dired-touch-program 'touch arg))

;; eliminate the error message:
;; "ls does not support --dired; see `dired-use-ls-dired' for more details."
(setq dired-use-ls-dired nil)

(provide 'dino-dired-fixups)

;;; dino-dired-fixups.el ends here

;;; goflycheck.el --- a flycheck checker for the Go Language

;; Author: Dino Chiesa <dpchiesa@hotmail.com>

;;; Commentary:

;;; Code:

(require 'go-mode)


;;;###autoload
(defcustom goflycheck-cmd-line-limit 28
  "This is a limit on the searching for commands.

If positive, this is the number of lines at the top of the source
file to look in, to find the command that goflycheck will use to
compile or flycheck the current buffer.

If negative, then only in the final N lines.

If the value of this variable is zero, then goflycheck looks
everywhere in the file.

The line should appear in a comment inside the Go buffer."
  :type 'integer
  :group 'goflycheck)


(defun goflycheck-get-value-from-comments (marker-string line-limit)
  "Gets a string from the header comments in the current buffer.

This is used to extract the flycheck command and the compile
command from the comments.

It looks for the MARKER-STRING followed by a colon, and returns
the string that follows it, or returns nil if that string is not
found.  It searches in the top LINE-LIMIT lines if LINE-LIMIT is
positive.  If negative, then it searches in the final LINE-LIMIT
lines.  If zero then it searches the entire file, which may take
extra time.  This search runs every time the flycheck occurs, so
be careful uzing zero.

Example: when MARKER-STRING is \"flycheck\", LINE-LIMIT is 25,
and the following string is found on the 8th line of the buffer:

     // flycheck: go test foo.go

...then this command will return the string

     \"go test foo.go\"

It's ok to have whitespace between the marker and the following
colon.  This function is intended for internal use.  goflycheck
automatically attaches it as a flycheck hook."

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



(defun goflycheck-set-flycheck-command ()
  "Set the flycheck command for a Go module, dynamically.

This function is intended for use as a before-syntax-check-hook with
flycheck.  Use it like this:

    (add-hook 'flycheck-before-syntax-check-hook  #'goflycheck-set-flycheck-command)

Then, in your go file, specify this in the comments near the top
of the file.  (See `goflycheck-cmd-line-limit' for details.)

    // flycheck: go build one.go two.go %f

This will cause flycheck to run the given command, replacing the %f with
the source file name.

This function is intended for internal use.  The goflycheck adds this hook
automatically when flycheck loads."

  (and (eq major-mode 'go-mode)
       (let ((cmd-string
              (goflycheck-get-value-from-comments "flycheck" goflycheck-cmd-line-limit)))
         (and cmd-string
              (not (eq cmd-string ""))
              (let* ((cmd (split-string cmd-string " "))
                     (ferf (member "%f" cmd)))
                (and ferf (setcar ferf 'source))
                (put 'go :flycheck-command cmd))))))


(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker gocheck
       "A GO syntax checker. It uses \"go build\" as the tool.
If you would like to use a different compiler, see
`goflycheck-set-flycheck-command'."
       :command ("go" "build" source)
       :error-patterns
       ;; ex43.go:6:4: expected 'STRING', found newline
       ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
       :modes go-mode)
     (add-hook 'flycheck-before-syntax-check-hook  #'goflycheck-set-flycheck-command)
     (setq flycheck-log-level 0)))


(provide 'goflycheck)

;;; goflycheck.el ends here

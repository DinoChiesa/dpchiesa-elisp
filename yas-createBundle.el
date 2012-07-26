(defun dino-recompile-snippets ()
  "recompile all snippets in the snippets directory"
  (interactive)
  (let ((dir "c:/users/dino/elisp/snippets")
        (elfile "yasnippet-bundle.el"))

    (delete-file (concat dir "/" elfile))
    (yas/compile-snippets  dir elfile)))


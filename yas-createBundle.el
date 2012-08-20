(defun dino-recompile-snippets ()
  "recompile all snippets in the snippets directory"
  (interactive)
  (let ((dir "/Users/Dino/elisp/snippets")
        (elfile "yasnippet-bundle.el"))

    (delete-file (concat dir "/" elfile))
    (yas/compile-snippets  dir elfile)))

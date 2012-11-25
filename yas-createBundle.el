;; This prolly ain't what you're looking for.
;; See dino-recompile-then-reload-all-snippets ()

(defun dino-recompile-snippets ()
  "recompile all snippets in the snippets directory"
  (interactive)
  (let* ((dir "/Users/Dino/elisp/snippets")
         (elfile "yasnippet-bundle.el")
         (fqpath (concat dir "/" elfile)))

    (if (file-exists-p fqpath)
        (delete-file fqpath))
    (yas/compile-snippets  dir elfile)))

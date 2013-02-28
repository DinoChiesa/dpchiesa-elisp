;; image-dired-fixups.el
;;
;; image dired did not work in MacOS for me.
;; So I added these fixups to get it to work.


;;
;; (defun image-dired-fixup-check-convert ()
;;   "checks the existence of the ImageMagick convert program"
;;   (image-dired-fixup-is-executable-file-on-path "convert"))

(require 'image-dired)

(defun image-dired-fixup-is-executable-file-on-path (filename)
  "checks the existence of the given file on the path."
  (let ((path (shell-command-to-string (concat "which " filename))))
    (and
     (not (null path))
     (not (string= "" path)))))


;; redefine
(defun image-dired-create-thumb (original-file thumbnail-file)
  "For ORIGINAL-FILE, create thumbnail image named THUMBNAIL-FILE."
  (if (image-dired-fixup-is-executable-file-on-path
       image-dired-cmd-create-thumbnail-program)
      (let* ((width (int-to-string image-dired-thumb-width))
             (height (int-to-string image-dired-thumb-height))
             (modif-time (format "%.0f" (float-time (nth 5 (file-attributes
                                                            original-file)))))
             (thumbnail-nq8-file (replace-regexp-in-string ".png\\'" "-nq8.png"
                                                           thumbnail-file))
             (command
              (format-spec
               (if (eq 'standard image-dired-thumbnail-storage)
                   image-dired-cmd-create-standard-thumbnail-command
                 image-dired-cmd-create-thumbnail-options)
               (list
                (cons ?p image-dired-cmd-create-thumbnail-program)
                (cons ?w width)
                (cons ?h height)
                (cons ?m modif-time)
                (cons ?f original-file)
                (cons ?q thumbnail-nq8-file)
                (cons ?t thumbnail-file))))
             thumbnail-dir)
        (when (not (file-exists-p
                    (setq thumbnail-dir (file-name-directory thumbnail-file))))
          (message "Creating thumbnail directory.")
          (make-directory thumbnail-dir))
        (call-process shell-file-name nil nil nil shell-command-switch command))
    ;; else
    (and
    (message (format "Cannot find thumbnail creation program (%s)"
             image-dired-cmd-create-thumbnail-program))
    -1)))

(setenv "PATH"
  (concat
   "/opt/local/bin" ":"
   (getenv "PATH")))

(provide 'image-dired-fixups)

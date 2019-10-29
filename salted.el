;;; salted.el --- functions for opening encrypted files
;;
;; Author: Dino Chiesa
;; Created: Monday, 28 October 2019, 16:15

;; This basically works.
;; If you have "untabify" set in a before-save-hook, you need to
;; remove it, because it will interfere with the write of the ciphertext byte stream.
;;
;; Bugs
;;
;; - the buffer position is trashed on save.
;;


(defvar salted--salt-file-utility "~/dev/go/src/github.com/DinoChiesa/salted/salt_file"
  "The location of the salt_file utilty to encrypt and decrypt")

(defvar salted--salt-file-passphrase ""
  "The passphrase")

;; (defun salted-decrypt-file (passwd)
;;   "decrypt the file"
;;   (let ((coding-system-for-write 'no-conversion)
;;         (coding-system-for-read 'no-conversion))
;;     (call-process-region (point-min) (point-max) salted--salt-file-utility
;;                          t t nil "-in" buffer-file-name "-out" "-" "-passphrase" passwd "-decrypt")))

(defun salted-decrypt-buffer (passwd)
  "decrypt the region"
  (let ((coding-system-for-write 'no-conversion)
        (coding-system-for-read 'no-conversion))
    (call-process-region (point-min) (point-max) salted--salt-file-utility
                         t t nil "-in" "-" "-out" "-" "-passphrase" passwd "-decrypt")))

(defun salted-encrypt-buffer-to-file (passwd)
  "encrypt the region"
  (message "encrypting to (%s)" buffer-file-name)
  (let ((coding-system-for-write 'no-conversion)
        (coding-system-for-read 'no-conversion))
    (call-process-region (point-min) (point-max) salted--salt-file-utility
                         t t nil "-in" "-" "-out" "-" "-passphrase" passwd)))

(define-generic-mode 'salted-file-mode
  (list ?#)
  nil nil
  '(".salted\\'")
  (list (lambda ()
          (add-hook 'before-save-hook
                    (lambda ()
                      (salted-encrypt-buffer-to-file salted--salt-file-passphrase))
                    nil t)

          (add-hook 'after-save-hook
                    (lambda ()
                      ;;(salted-decrypt-file salted--salt-file-passphrase)
                      (salted-decrypt-buffer salted--salt-file-passphrase)
                      (set-buffer-modified-p nil)
                      (auto-save-mode nil))
                    nil t)

          (set (make-local-variable 'salted--salt-file-passphrase) (read-passwd "passphrase: "))
          (set-buffer-file-coding-system 'no-conversion t)
          (salted-decrypt-buffer salted--salt-file-passphrase)
          (goto-char (point-min))
          (auto-save-mode nil)
          (set-buffer-modified-p nil)))
  "Mode for salted encrypted files")

(provide 'salted)

;;; salted.el ends here

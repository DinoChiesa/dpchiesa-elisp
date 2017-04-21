;;; lorem.el --- insert lorem ipsum text at point
;;
;; Author: Dino Chiesa
;; Version: 0.1
;; License: Public Domain
;; Created: Sat, 31 Mar 2012  09:29
;;

(defvar lorem-text-list nil
  "A list of lorem ipsum sentences. Computed at runtime.")

(defvar lorem-current-source-file nil
  "name of the current file to use for lorem ipsum text")

(defvar lorem-default-source-file
  (concat
   (file-name-as-directory
    (if (eq system-type 'windows-nt)
        (getenv "USERPROFILE")
        (getenv "HOME")))
          "Documents/Lorem.txt")
  "name of the default file to use for lorem ipsum text")

(defun lorem-set-source-file (file)
  "set the current source file for subsequent lorem generation"
  (interactive "fSource file: ")
  (setq lorem-text-list nil
        lorem-current-source-file file) )

(defun lorem-string-ends-with (s ending)
  "return non-nil if string S ends with ENDING"
  (if (and (stringp s)
       (> (length s) 0))
  (let ((elength (length ending)))
    (string=
     (substring s (- 0 elength))
     ending))))

(defun lorem-ipsum ()
  "Inserts one paragraph of lorem ipsum text at point."
  (interactive)

  (if (not lorem-text-list)
      (let (beg txt pmax)
        (with-temp-buffer
          (insert-file-contents (or lorem-current-source-file lorem-default-source-file))
          (goto-char (point-min))
          (setq pmax (point-max)
                beg (point))
          (while (re-search-forward "[\.\n\?]" pmax t)
            (setq txt (buffer-substring-no-properties (if (> beg 1) (1- beg) beg) (point)))
            (if (> (length txt) 1)
                (progn
                  (while (lorem-string-ends-with txt "\n")
                    (setq txt (substring txt 0 -1)))
                  (if (not (or (lorem-string-ends-with txt ".")
                               (lorem-string-ends-with txt "?")))
                      (setq txt (concat txt ".")))
                  (if (> (length txt) 1)
                      (setq lorem-text-list (cons txt lorem-text-list)))))
            (setq beg (1+ (point)))))))

  (let ((n-sentences (+ (random 3) 3))
        (c 0)
        (len (length lorem-text-list))
        (ptext ""))
    (while (< c n-sentences)
      (setq ptext (concat ptext (nth (random len) lorem-text-list) " ")
            c (1+ c)))
    (insert ptext)))


(provide 'lorem)

;;; lorem.el ends here

;;; yaml-pretty-mode.el
;;
;; compose-region can be used to replace a region of text with a different character
;; This works from within font-lock to display leading whitespace differently.
;;
;; ps: font-lock is amazingly, enduringly inscrutable.
;;
;; Public Domain - do what you want with this.
;;

(defgroup yaml-pretty nil
  "prettify yaml buffers by displaying leading whitespace specially."
  :group 'yaml-pretty :prefix 'yaml-pretty-)

(defvar yaml-pretty-ws1 ?┊
  "the character to display for the first space in a 2-space sequence.")

(defvar yaml-pretty-ws2 ?·
  "the character to display for the second space in a 2-space sequence.")

;; ?┊  ;; just right
;; ?·  ;; good
;; ?│  ;; alternative
;; ?⸠  ;; not high enough
;; ?┃  ;; too heavy
;; ?⍿  ;; too short and narrow
;; ?|  ;; not high enough
;; see http://unicode-search.net/unicode-namesearch.pl?term=VERTICAL

(defface yaml-pretty-leading-ws-face
  '(
    (((class color) (min-colors 8))
     :foreground "grey30")
    )
  "face for leading whitespace in a yaml file."
  :group 'yaml-pretty)

(defun yaml-pretty-search-leading-ws (limit)
  "Function to search for leading whitespace, and invoke
    `compose-region' on ranges within it, to display the whitespace
    differently: display a dot and a vertical bar for the sequence.

    This fn is intended to be invoked indirectly via font-lock as a
    search/matcher. It doesn't return a positive match to font-lock.

    At the finish, all of the leading whitespace up to LIMIT should
    be composed and fontified."

  (save-match-data ;; we don't want font-lock to fontify.
    (beginning-of-line)
    (let ((found-leading-ws))
      (while
          (re-search-forward "  " limit t)
        (let ((start (match-beginning 0))
              (stop (match-end 0)))
          (setq found-leading-ws t)
          (compose-region start (1- stop) yaml-pretty-ws1)
          (compose-region (1- stop) stop yaml-pretty-ws2)
          (put-text-property start stop 'face 'yaml-pretty-leading-ws-face)
          ))
      )))


;; (defvar yaml-pretty--NOTUSED--leading-ws-anchor-regexp "^  ")
;;
;; (defvar yaml-pretty--NOTUSED--flock-keywords-first-try
;;   `((,yaml-pretty-leading-ws-anchor-regexp ;; anchor
;;      ("doesn't matter"                      ;; anchored-matcher
;;       (yaml-pretty-match-leading-ws)          ;; pre-match form
;;       (goto-char (match-end 0))             ;; post-match form
;;       (0 yaml-pretty-leading-ws-face)
;;       ))))


(defvar yaml-pretty-flock-keywords
  '((yaml-pretty-search-leading-ws . yaml-pretty-leading-ws-face)))

;;;###autoload
(define-minor-mode yaml-pretty-mode
  "minor mode"
  :group 'yaml-pretty
  :init-value nil
  :lighter nil
  :global t

  ;; Toggling the mode should clear the state variables.

  (font-lock-add-keywords nil yaml-pretty-flock-keywords)

  ;; re-fontify the entire buffer
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer))))
  )

(provide 'yaml-pretty-mode)

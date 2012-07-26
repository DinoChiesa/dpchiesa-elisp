;; to help determine the appropriate way to
;; insert comments in a html-mode or pho-mode
;; buffer.
(defun if-inside-php ()
  " "
  (interactive)
  (let (
        ;; (case-fold-search t)
        (php-start-pos
         (save-excursion
           (if (re-search-backward "<\\?php\\|<\\?PHP\\|<\\?\\|<%" nil t 1)
               (match-beginning 0)
             (point-min))))
        (html-js-start-pos
         (save-excursion
           (if (re-search-backward "<!--" nil t 1)
               (match-beginning 0)
             (point-min))))
        (php-end-pos
         (save-excursion
           (if (re-search-forward "\\?>\\|%>" nil t 1)
               (match-end 0)
             (point-max))))
        (html-js-end-pos
         (save-excursion
           (if (re-search-forward "-->" nil t 1)
               (match-end 0)
             (point-max)))))
    (if
        (and (> php-start-pos html-js-start-pos)
             (< php-end-pos html-js-end-pos))
        (setq inside-php t)
      (setq inside-php nil))))



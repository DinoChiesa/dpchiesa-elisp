
;; The first solution used a the `'display` text property, which
;; changes how the specified text is displayed, in this case it was
;; *replaced* by the triangle in the [left fringe][1].  What I
;; needed to do was to use a `'before-string` [*overlay*][2]
;; instead.  Which doesn't change the string being displayed.
;;
;;
;;   ;; put little triangles in the left fringe of the display,
;;   ;; when a TODO: is detected in the text.
;;   ;; This function is slightly broken, because it seems to
;;   ;; alter the text in some unexpected way.
;;   (defun annotate-todo ()
;;     "put fringe marker on TODO: lines in the curent buffer"
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward "TODO:" nil t)
;;         (put-text-property (+ 4 (point)) (+ 4 (point))
;;                            'display '(left-fringe right-triangle)))))

;;    (defun annotate-todo ()
;;       "put fringe marker on TODO: lines in the curent buffer"
;;       (interactive)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward "TODO:" nil t)
;;           (let ((overlay (make-overlay (- (point) 5) (point))))
;;             (overlay-put overlay 'before-string (propertize (format "A")
;;                                                             'display '(left-fringe right-triangle)))))))


;;     (defun annotate-todo ()
;;        "put fringe marker on TODO: lines in the curent buffer"
;;       (interactive)
;;       (let (lit)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward "TODO:" nil t)
;;           (progn
;;             (setq lit (csharp-in-literal))
;;             (if (or (eq lit 'c) (eq lit 'c++))
;;                 (let ((overlay (make-overlay (- (point) 5) (point))))
;;                   (overlay-put overlay 'before-string
;;                                (propertize "A"
;;                                            'display
;;                                            '(left-fringe   ;; right
;;                                              horizontal-bar
;;                                              better-fringes-important-bitmap))))))))))



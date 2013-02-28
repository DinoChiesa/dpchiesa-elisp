(defun multiline-message-box (msg)
  "display a multiline message box on Windows.

According to bug #11138, when passing a message with newlines to
`message-box' on Windows, the rendered message-box appears all on
one line.

This function can work around that problem.
"
  (flet ((ok (&optional p1 &rest args) t))

    (let ((parts (split-string msg "\n"))
          (menu-1 (make-sparse-keymap "Attention"))
          c)

      (define-key menu-1 [menu-1-ok-event]
        `(menu-item ,(purecopy "OK")
                    ok
                    :keys ""))
      (define-key menu-1 [separator-1] menu-bar-separator)

      ;; add lines in reverse order
      (setq c (length parts))
      (while (> c 0)
        (setq c (1- c))
        (define-key menu-1 (vector (intern (format "menu-1-fake-event-%d" c)))
          `(menu-item ,(purecopy (nth c parts))
                      nil
                      :keys ""
                      :enable t)))
      (x-popup-menu t menu-1))))


;;(multiline-message-box "Hello!\nI must be going!\nThis is line 3.")

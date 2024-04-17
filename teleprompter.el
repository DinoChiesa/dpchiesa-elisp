;;; teleprompter.el --- a teleprompter minor mode

;; TODO:
;; 1. make it stop scrolling at end-of-buffer.
;; 2. figure out how to get the keybindings to work. Maybe they already do work?

(defcustom telep-spl 2.7
  "seconds per line"
  :group 'telep
  :type 'float)

(defvar telep--running nil)

(defvar telep-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "SPC") 'telep-start/stop)
    (define-key km (kbd "f") 'telep-faster)
    (define-key km (kbd "s") 'telep-slower)
    (define-key km (kbd "q") 'telep-quit)
    (define-key km [remap keyboard-quit] 'telep-quit)
    km)
  "keymap for telep-mode buffers")

(defun telep--update ()
  ;; todo: check end-of-buffer here
   (scroll-up-line))

(defun telep-stop ()
  "Pause telep.
Returns t if telep was unpaused."
  (interactive)
  (prog1 telep--running
    (when telep--running
      (cancel-timer telep--running)
      (setq telep--running nil))))

(defun telep-start ()
  "Start / resume telep."
  (interactive)
  (setq telep--running
        (run-with-timer 0 telep-spl 'telep--update)))

(defun telep-start/stop ()
  "Toggle pause/unpause telep."
  (interactive)
  (or (telep-stop) (telep-start)))

(defun telep-quit ()
  "Exit telep mode."
  (interactive)
  (telep-mode -1))

(defun telep-faster ()
  "Increases speed.
Decreases the seconds-per-line parameter. See the variable
`telep-spl'."
  (interactive)
  (setq telep-spl (* telep-spl 0.85)))


(defun telep-slower ()
  "Decreases speed.
Increases the seconds-per-line parameter. See the variable
`telep-spl'."
  (interactive)
  (setq telep-spl (/ telep-spl 0.85)))


;;;###autoload
(define-minor-mode telep-mode
  "telep mode"
  :init nil
  :keymap telep-mode-map
  (cond (telep-mode
         (telep-start))
        (t
         (telep-stop)
         )))


(provide 'telep)
;;; teleprompter.el ends here

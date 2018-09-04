;;; linum-ex.el --- Display line numbers to the left of buffers

;; originally derived from linum.el, which is
;; Copyright (C) 2007, 2008  Markus Triska

;; modifications in linum-ex.el provided by: Dino Chiesa

;; Author: Markus Triska <markus.triska@gmx.at>
;; Last saved: <2018-September-04 11:16:30>
;;
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display line numbers for the current buffer. Copy linum-ex.el to your
;; load-path and add to your .emacs:

;;    (require 'linum-ex)

;; Then toggle display of line numbers with M-x linum-mode. To enable
;; line numbering in all buffers, use M-x global-linum-mode.

;; =======================================================
;;
;; Dino Chiesa Mon, 23 May 2011  12:00
;;
;; notes on changes.
;;
;; The problem with the original linum module is that it updated the
;; line numbers after every scroll and possibly every command. This
;; works for small files but not for files with 100,000+ lines. Even
;; with linum-delay turned on, linum had the effect of "Freezing" the
;; display when the user was continuously scrolling.  It also introduced
;; noticeable delays when scrolling only momentarily.
;;
;; One idea for working around that is to use `run-with-idle-timer', and
;; only update the line numbers when emacs is idle. One can set a single
;; timer, for, say 0.1s, and then when emacs goes idle for that period,
;; the line numbers will be updated.
;;
;; Seems like the perfect fit, but there's a problem: a timer created
;; via `run-with-idle-timer' gets canceled after being delayed 10 times.
;; If the after-scroll event sets up a timer via `run-with-idle-timer',
;; only in the case when no timer has been set, the timer may get
;; canceled silently, by timer.el . For more on this look at
;; `timer-max-repeats'.
;;
;; If the base delay is 0.1s, then this cancellation would happen after
;; 10 cycles, or if emacs is busy for 1s, which may occur for example
;; when the user is holding pgdn to continuously scroll through a large
;; document. If the timer gets cancelled, then the line numbers don't
;; ever get updated.
;;
;; To avoid that pitfall, this code can set `run-with-idle-timer' for
;; every scroll event, and handle the delay explicitly, right here.  The
;; way to do it is, within the after-scroll event, store the "last
;; scrolled" time, and then call `run-with-idle-timer'. There may be
;; other outstanding timer events, but we don't care.  In the function
;; that gets called when the timer fires, check to see if a reasonable
;; interval (Say 0.1s) has elapsed since the last scroll event. If so,
;; do linum-update. If not, it means scrolling is still happening, so,
;; do nothing. All this applies only if linum-delay is non-nil.
;;
;; The result is that timers fire constantly while the user is
;; continuously scrolling, but the line numbers get updated only after
;; the user stops scrolling. The user experiences no delay while
;; scrolling, but (unfortunately) gets no line numbers either. The user
;; sees updated line numbers immediately when he stops scrolling.
;;
;; =======================================================


;;; Code:

(require 'timer)

(defconst linum-version "0.991")

(defvar linum-overlays nil "Overlays used in this buffer.")
(defvar linum-available nil "Overlays available for reuse.")
(defvar linum-before-numbering-hook nil
  "Functions run in each buffer before line numbering starts.")

(mapc #'make-variable-buffer-local '(linum-overlays linum-available))

(defgroup linum nil
  "Show line numbers to the left of buffers"
  :group 'convenience)

;;;###autoload
(defcustom linum-format 'dynamic
  "Format used to display line numbers. Either a format string
like \"%7d\", 'dynamic to adapt the width as needed, or a
function that is called with a line number as its argument and
should evaluate to a string to be shown on that line. See also
`linum-before-numbering-hook'."
  :group 'linum
  :type 'sexp)

(defface linum
  '((t :inherit (shadow default)))
  "Face for displaying line numbers in the display margin."
  :group 'linum)

(defcustom linum-eager t
  "Whether line numbers should be updated after each command.
The conservative setting `nil' might miss some buffer changes,
and you have to scroll or press C-l to update the numbers."
  :group 'linum
  :type 'boolean)

(defcustom linum-delay nil
  "Delay updates to give Emacs a chance for other changes."
  :group 'linum
  :type 'boolean)

(defvar linum--delay-time 0.1
  "Delay time.  See also `linum-delay'")

(defvar linum--last-scroll nil
  "Time of last scroll event. See also `linum-delay'")

(defvar linum--last-cmd nil
  "Time of last command. See also `linum-delay'")

(defvar linum--win nil
  "Window of the last scroll event. See also `linum-delay'")


;;;###autoload
(define-minor-mode linum-mode
  "Toggle display of line numbers in the left marginal area."
  :lighter ""                           ; for desktop.el
  (if linum-mode
      (progn
        (if linum-eager
            (add-hook 'post-command-hook 'linum-post-command)
          (add-hook 'after-change-functions 'linum-after-change nil t))
        (add-hook 'window-scroll-functions 'linum-after-scroll nil t)
        ;; mistake in Emacs: window-size-change-functions cannot be local
        (add-hook 'window-size-change-functions 'linum-after-size)
        (add-hook 'change-major-mode-hook 'linum-delete-overlays nil t)
        (add-hook 'window-configuration-change-hook
                  'linum-after-config nil t)
        (set (make-local-variable 'linum--win) nil)
        (set (make-local-variable 'linum--last-scroll) nil)
        (set (make-local-variable 'linum--last-cmd) nil)
        (linum-update-current))
    (remove-hook 'post-command-hook 'linum-post-command t)
    (remove-hook 'window-size-change-functions 'linum-after-size)
    (remove-hook 'window-scroll-functions 'linum-after-scroll t)
    (remove-hook 'after-change-functions 'linum-after-change t)
    (remove-hook 'window-configuration-change-hook 'linum-after-config t)
    (remove-hook 'change-major-mode-hook 'linum-delete-overlays t)
    (linum-delete-overlays)))

;;;###autoload
(define-globalized-minor-mode global-linum-mode linum-mode linum-on)

(defun linum-on ()
  "Turn on linum (line numbering) mode."
  (interactive)
  (unless (minibufferp)
    (linum-mode 1)))

(defun linum-delete-overlays ()
  "Delete all overlays displaying line numbers for this buffer."
  (mapc #'delete-overlay linum-overlays)
  (setq linum-overlays nil)
  (dolist (w (get-buffer-window-list (current-buffer) nil t))
    (set-window-margins w 0)))

(defun linum-update-current ()
  "Update line numbers for the current buffer."
  (linum-update (current-buffer)))

(defun linum-update (buffer)
  "Update line numbers for all windows displaying BUFFER."
  (with-current-buffer buffer
    (when linum-mode
      (setq linum-available linum-overlays)
      (setq linum-overlays nil)
      (save-excursion
        (mapc #'linum-update-window
              (get-buffer-window-list buffer nil 'visible)))
      (mapc #'delete-overlay linum-available)
      (setq linum-available nil
            linum--last-cmd nil
            linum--last-scroll nil))))

(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (string= (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delete o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      (forward-line)
      (setq line (1+ line)))
    (linumex-set-window-margin-width win width)
    ;;(set-window-margins win width)
    ))

(defun linumex-set-window-margin-width (win width)
  "Sets the window margin width. When scaling text font in the window,
the linum font also scales. When the font becomes larger, in some
cases the line number will be too wide to fit in the margin. This
code updates the margin width used by linum when scaling."
  ;; When there has been no previous scaling event in the window, the variables
  ;; are not yet set.  Therefore we need to use default values.
  (let* ((stepvar (or (and (boundp 'text-scale-mode-step) text-scale-mode-step) 1.2))
         (amountvar (or (and (boundp 'text-scale-mode-amount) text-scale-mode-amount) 0))
         (xstep (or (and stepvar amountvar (expt stepvar (+ 0.8 amountvar))) 1))
         (net-width (ceiling (* xstep width))))
    ;; (message "step(%s) amount(%s) xstep(%s) net-width(%s)"
    ;;          (prin1-to-string stepvar)
    ;;          (prin1-to-string amountvar)
    ;;          (prin1-to-string xstep)
    ;;          (prin1-to-string net-width))
    (set-window-margins win net-width)))


(defun linum-after-change (beg end len)
  ;; update overlays on deletions, and after newlines are inserted
  (when (or (= beg end)
            (= end (point-max))
            ;; TODO: use string-match-p with CVS or new release
            (string-match "\n" (buffer-substring-no-properties beg end)))
    (linum-update-current)))

(defun linum--after-scroll-fired ()
  (if linum--last-scroll
      (let ((now (current-time))
            (one-moment-after-scroll (timer-relative-time linum--last-scroll linum--delay-time)))
        (if (time-less-p one-moment-after-scroll now)
            (linum-update linum--win)))))

(defun linum-after-scroll (win start)
  (if linum-delay
      (progn
        (setq linum--win (window-buffer win))
        (setq linum--last-scroll (current-time))
        (run-with-idle-timer linum--delay-time nil 'linum--after-scroll-fired))
    (linum-update (window-buffer win))))

(defun linum--post-command-fired ()
  (if linum--last-cmd
      (let ((now (current-time))
            (one-moment-after-cmd (timer-relative-time linum--last-cmd linum--delay-time)))
        (if (time-less-p one-moment-after-cmd now)
            (linum-update-current)))))

(defun linum-post-command ()
  (if linum-delay
      (progn
        (setq linum--last-cmd (current-time))
        (run-with-idle-timer linum--delay-time nil 'linum--post-command-fired))
    (linum-update-current)))

(defun linum-after-size (frame)
  (linum-after-config))

(defun linum-after-config ()
  (walk-windows (lambda (w) (linum-update (window-buffer w))) nil 'visible))


(provide 'linum-ex)
;;; linum-ex.el ends here

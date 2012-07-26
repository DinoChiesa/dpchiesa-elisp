;;; js-comint.el --- Run a javascript command interpreter in emacs.

;;; Copyright (C) 2008 Paul Huff

;;; Author: Paul Huff <paul.huff@gmail.com>
;;; Maintainer: Paul Huff <paul.huff@gmail.com>
;;; Created: 26 May 2008
;;; Version: 0.0.1
;;; Package-Requires: ()
;;; Keywords: javascript, inferior-mode, convenience


;; js-comint.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; js-comint.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;; js-comint.el let's you run an inferior javascript process in emacs,
;;; and defines a few functions for sending javascript input to it quickly.

;;  Usage:
;;  Put js-comint.el in your load path
;;  Add (require 'js-comint) to your .emacs
;;  Set jsint-js-interpreter-program  to the execution command for running your javascript REPL
;;  (setq jsint-js-interpreter-program  "/path/to/executable <args>")
;;  Do: M-x run-js
;;  Away you go.

;;  I've added the following couple of lines to my .emacs to take advantage of
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent js2-mode.

;; (add-hook 'js2-mode-hook '(lambda ()
;;                          (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;                          (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;                          (local-set-key "\C-cb" 'js-send-buffer)
;;                          (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;                          (local-set-key "\C-cl" 'js-load-file-and-go)
;;                          ))

;;  This is version 0.0.1, so I've only tested it on my own version of emacs which is currently:
;;  GNU Emacs 22.0.90.1 (i386-apple-darwin8.8.1, Carbon Version 1.6.0) of 2006-10-28
;;  Not sure if it'll work anywhere else, but it doesn't require anything apple-ish, just emacs-ish.

;; Additionally, I've only tested this with rhino.  I'm sure it'll probably work with spidermonkey,
;; though if it barfs let me know, and I'll update it.

;; I'm a newbie elisper, so please let me know if I'm a. doing things the wrong way, b.
;; making things work they way they shouldn't in the elisp world.

;;; History:
;;

;;; Code:

(require 'comint)
jsint-js-interpreter-program
jsint-interpeter-program-command

(defcustom jsint-interpeter-program-command  "/usr/bin/java org.mozilla.javascript.tools.shell.Main" "Path to the javascript interpreter")

(defgroup jsint nil
  "Run a javascript process in a buffer."
  :group 'jsint)

(defcustom jsint-mode-hook nil
  "*Hook for customizing jsint mode."
  :type 'hook
  :group 'jsint)

;;;###autoload
(defun run-js (cmd &optional dont-switch-p)
  "Run an inferior Javascript process, input and output via buffer `*js*'.
If there is a process already running in `*js*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `jsint-interpeter-program-command').

Runs the hook `jsint-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run js: " jsint-js-interpreter-program )
                       jsint-js-interpreter-program )))
  (if (not (comint-check-proc "*js*"))
      (save-excursion (let ((cmdlist (split-string cmd)))
        (set-buffer (apply 'make-comint "js" (car cmdlist)
                           nil (cdr cmdlist)))
        (jsint-mode))))
  (setq jsint-js-interpreter-program  cmd)
  (setq jsint-buffer "*js*")
  (if (not dont-switch-p)
      (pop-to-buffer "*js*")))

;;;###autoload
(defun js-send-region (start end)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (run-js jsint-js-interpreter-program  t)
  (comint-send-region jsint-buffer start end)
  (comint-send-string jsint-buffer "\n"))

;;;###autoload
(defun js-send-region-and-go (start end)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (run-js jsint-js-interpreter-program  t)
  (comint-send-region jsint-buffer start end)
  (comint-send-string jsint-buffer "\n")
  (switch-to-js jsint-buffer))

;;;###autoload
(defun js-send-last-sexp-and-go ()
  "Send the previous sexp to the inferior Js process."
  (interactive)
  (js-send-region-and-go (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun js-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (js-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun js-send-buffer ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-send-region (point-min) (point-max)))


;;;###autoload
(defun js-send-buffer-and-go ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun js-load-file (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-js jsint-js-interpreter-program  t)
    (comint-send-string jsint-buffer (concat "load(\"" filename "\")\n"))))

;;;###autoload
(defun js-load-file-and-go (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-js jsint-js-interpreter-program  t)
    (comint-send-string jsint-buffer (concat "load(\"" filename "\")\n"))
    (switch-to-js jsint-buffer)))

;;;###autoload
(defun switch-to-js (eob-p)
  "Switch to the javascript process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and jsint-buffer (get-buffer jsint-buffer))
          (js-interactively-start-process))
      (pop-to-buffer jsint-buffer)
    (error "No current process buffer.  See variable `jsint-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar jsint-buffer)

(defvar jsint-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-x\C-e" 'js-send-last-sexp)
    (define-key m "\C-cl" 'js-load-file)
    m))

;;;###autoload
(define-derived-mode jsint-mode comint-mode "Inferior Javascript"
  "Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{jsint-mode-map}

A javascript process can be fired up with M-x run-js.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
jsint-mode-hook (in that order).

You can send text to the inferior Javascript process from othber buffers containing
Javascript source.
    switch-to-js switches the current buffer to the Javascript process buffer.
    js-send-region sends the current region to the Javascript process.


"
(use-local-map jsint-mode-map))

(provide 'jsint)

;;; js-comint.el ends here

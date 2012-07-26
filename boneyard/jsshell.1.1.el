;;; jsshell.el --- Run a javascript command interpreter in emacs on Windows.
;;
;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : 26 March 2012
;; Modified   : March 2012
;; Version    : 0.1.1
;; Keywords   : jScript javascript shell ms-windows
;; Copyright  : (c) 2012 Dino Chiesa
;; License    : New  BSD
;; X-URL      : http://www.emacswiki.org/emacs/JSShell
;; Last-saved : <2012-March-27 13:50:11>
;;

;;; Commentary:
;;
;; This module was philosophically inspired by Paul Huff's js-comint.el .
;;
;; The code here, though, is not related. It has been partially derived
;; from powershell.el, which is Copyright (C) 2008-2011 Dino Chiesa
;;
;; The idea behind js-comint.el was attractive to me, but I did not
;; prefer the design used in that module. So I basically copied the
;; structure for jsshell.el from powershell.el, and renamed the
;; important bits.  This newly created module has an external dependency
;; on jsshell.js, and on Windows and cscript.exe.  It also has some of
;; the utility functions from the original js-comint.el - same functions
;; but different implementations.
;;
;;  Usage:
;;  Put jsshell.el in your load path
;;  Add (require 'jsshell) to your .emacs
;;  M-x jsshell
;;
;; Consider adding these key mappnigs:
;;
;;    (local-set-key "\C-x\C-e" 'jsshell-send-last-sexp)
;;    (local-set-key "\C-\M-x"  'jsshell-send-last-sexp-and-pop)
;;    (local-set-key "\C-cb"    'jsshell-send-buffer)
;;    (local-set-key "\C-c\C-b" 'jsshell-send-buffer-and-pop)
;;    (local-set-key "\C-cl"    'jsshell-load-file-and-pop)
;;    (local-set-key "\C-c\C-e" 'jsshell-send-region)
;;


;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2010, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;

;;; Bugs:

;; Literal strings typed into the shell need extra escaping.  This is
;; particularly a problem with regex strings. Eg, a \d in a JS regex gets
;; transformed into just a d when run through the jsshell.  I think this
;; is not a problem when loading files with `jsshell-load-file'. The
;; problem may not be worth fixing.
;;

;;; Code:

(require 'shell)

(defcustom jsshell-location-of-cscript-exe
  ;; eg, "c:\\windows\\system32\\cscript.exe"
  (concat (getenv "WINDIR") "\\system32\\cscript.exe")
  "Path to the CSCRIPT.exe . You probably don't need to set this."
  :group 'jsshell)

(defcustom jsshell-location-of-jsshell-js
  "c:\\dev\\js\\jsshell.js"
  "Path to the javascript REPL program"
  :group 'jsshell)

(defcustom jsshell-profile
  (list "c:\\dev\\js\\json2.js")
  "List of JS modules to load every time a jsshell starts."
  :group 'jsshell)

(defgroup jsshell nil
  "Run a javascript process in a buffer."
  :group 'jsshell)

(defcustom jsshell-log-level 0
  "The current log level for jsshell internal operations.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG. "
  :group 'jsshell)

(defvar jsshell--prompt-regex  "js> $"
  "Regexp for JSShell prompt. For internal use by jsshell.el")

(defvar jsshell--awaiting-command-prompt nil
  "For internal use only. ")

(defvar jsshell--file-load-queue  nil
  "For internal use only. ")


(defun jsshell-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `jsshell-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level jsshell-log-level)
      (let* ((msg (apply 'format text args)))
        (message "JSShell: %s" msg))))


;;;###autoload
(defun jsshell (&optional buffer)
  "Run an inferior Javascript shell, with I/O through tne named
BUFFER (which defaults to `*JSShell*').

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a new shell.

If BUFFER exists and the shell process is running, just switch to BUFFER.

See the help for `shell' for more details.  \(Type
\\[describe-mode] in the shell buffer for a list of commands.)

NB: Literal strings typed into the shell need extra escaping.
This is espcially a problem with regex strings. eg, a \d in a JS
regex gets transformed into just a d when run through the
jsshell.  I think this is not a problem when loading files with
`jsshell-load-file'. The problem may not be worth fixing.

"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*JSShell*")))))

  (setq buffer (get-buffer-create (or buffer "*JSShell*")))

  (let ((proc (get-buffer-process buffer)))
    (if (not proc)
        (progn
          (jsshell-log 1 "Javascript shell starting up...in buffer %s"
                       (buffer-name buffer))
          (let ((explicit-shell-file-name jsshell-location-of-cscript-exe))
            (setq explicit-cscript.exe-args (list jsshell-location-of-jsshell-js))
            (shell buffer))

          (setq proc (get-buffer-process buffer))

          (set (make-local-variable 'jsshell--awaiting-command-prompt) nil)
          (set (make-local-variable 'shell-dirtrack-mode) nil)
          (set (make-local-variable 'comint-prompt-read-only) t)
          (set (make-local-variable 'comint-input-sender-no-newline) nil)
          (set (make-local-variable 'jsshell--files-to-load) nil)
          (set (make-local-variable 'jsshell--file-load-queue) nil)

          ;; fixup output hooks
          (make-local-variable 'comint-output-filter-functions)

          (add-hook 'comint-output-filter-functions
                    'jsshell--prompt-seeking-output-filter)

          (remove-hook 'comint-output-filter-functions
                       'comint-watch-for-password-prompt)

          (add-hook 'comint-preoutput-filter-functions
                    'jsshell--preoutput-filter-for-prompt)

          ;; hook the kill-buffer action so we can kill the inferior process
          (add-hook 'kill-buffer-hook 'jsshell-delete-process)

          (accept-process-output proc)

          ;; profile
          (setq jsshell--input-recd-msg "")

          ;; record the list of files to be loaded...
          (mapc (lambda (filename)
                  (setq jsshell--file-load-queue
                        (cons filename jsshell--file-load-queue)))
                jsshell-profile)

          ;; not sure why lisp is ornery like this...
          (setq jsshell--file-load-queue (reverse jsshell--file-load-queue))

          ;; kick off the loading
          (comint-send-input t t)

           ;; (mapc (lambda (filename) (jsshell-load-file filename buffer))
           ;;       jsshell-profile)

          )
      ;; pop to buffer?

      ))

  ;; return the buffer created
  buffer)



(defun jsshell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))


(defvar jsshell--multiline-note-regex
      (concat
       ;;"^"
       (regexp-quote
        "Multi-line input. Use two consecutive blank lines to eval.")
       "\\(\x0A\\|\x0D\\)*$" )

    "Regexp for multiline note from JSShell. For internal use by jsshell.el")


(defvar jsshell--input-recd-msg ""
  "not documented.")


(defun jsshell--preoutput-filter-for-prompt (string)
  "Filter the 'Enter two consecutive blank lines...' from
the output, in some cases."
  (let ((old-msg jsshell--input-recd-msg))
    (if (and jsshell--awaiting-command-prompt
             (string-match jsshell--multiline-note-regex string))
        (progn
          (jsshell-log 4 "preoutput: seeking prompt and got multiline msg...(%s)" string)
          (setq jsshell--awaiting-command-prompt t
                jsshell--input-recd-msg "")
          (if (string= old-msg "")
              "\ninput received.\n"
            old-msg))

      string)))


(defun jsshell--prompt-seeking-output-filter (string)
  "This function is intended for use only internally to the jsshell
package.

It gets installed as a comint output filter upon initialization
of a jsshell. Its purpose is to negotiate the I/O protocol the
shell uses to manage multi-line input.

Normally, the Javascript REPL evals each line of input
independently. In some cases, such as with functions that are
defined over multiple lines, the user of the REPL wants to defer
eval until after all the input is ready.

For accepting multiline input, the Javascript REPL applies this
convention: one empty line signals the beginning of a multi-line
input block, and 2 empty lines signal the end of the block. This
multi-line input is what is used to load an entire JS file into
the shell, for example.

This filter function manages that protocol, keeping in mind the
state of this module, specifically whether there are additional
files waiting to be loaded.  When expecting a prompt after
loading a multi-line block, this filter send a nudge to the
shell (essentially a newline). When not expecting a prompt, and
there are files to be loaded, this filter loads the next file.

The nudging happens only when sending chunks of data via
`jsshell-send-region' or `jsshell-load-file', which are typically
bound to keyboard sequences.

When the shell is in actual interactive use - that is to say when
a person is typing input directly into the shell buffer - this
function does not get called. The human is expected to enter the
necessary double newlines at the appropriate times.

"
  (jsshell-log 3 "output filter (waiting? %s) (%s)"
               (prin1-to-string jsshell--awaiting-command-prompt)
               string)
  (cond

   (jsshell--awaiting-command-prompt
    (if (string= string "")
        (progn
          (jsshell-log 3 "empty output string...")
          ;;(comint-send-string (current-buffer) "")
          )
      (jsshell-log 3 "seek prompt")
      (let ((current (current-buffer)))
        (if (string-match jsshell--prompt-regex string)
            (progn
              (setq jsshell--awaiting-command-prompt nil
                    jsshell--input-recd-msg "")

              (jsshell-log 3 "the waiting is over")

              ;; more files to load?
              (if jsshell--file-load-queue
                  (progn
                    (jsshell-log 3 "next file")
                    ;;(comint-send-string current "") ;; newline?  two newlines? shit!
                    (jsshell--nudge)
                    ))
              )

          ;;(comint-simple-send (get-buffer-process current) "\n")
          ;;(comint-send-string current "\n\n")
          (let ((proc (get-buffer-process current)))
            (if proc
                (progn
                  (jsshell-log 3 "no joy...")
                  (jsshell--nudge)
                  )))))))


   ;; not waiting. load a file, if available on the queue
   (jsshell--file-load-queue
    (let ((thisfile (car jsshell--file-load-queue)))
      (jsshell-log 3 "dequeue a file (%s)" thisfile)
      (setq jsshell--file-load-queue (cdr jsshell--file-load-queue))
      (jsshell-really-load-file thisfile (current-buffer))
      ))

   (t
    (jsshell-log 3 "not waiting, no files to load"))))




(defun jsshell--nudge (&optional newline-count)
  "send a nudge to the shell in the current buffer"
  (if (and newline-count (> newline-count 0))
      (comint-send-string (current-buffer) (make-string newline-count ?\n)))
  (comint-send-input newline-count t))



(defun jsshell--squish-jscode (string)
  "Collapse double-newlines to single newlines.
Also strip newlines from the end of the code.
This is necessary because a double-newline tells the
JS REPL to stop parsing and evaluate. This is not
what we want."
  (while (string-match "\n\n" string)
    (setq string (replace-match "\n" nil nil string)))
  (while (string-match "^\n" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "\n$" string)
    (setq string (replace-match "" nil nil string)))
  string)


(defun jsshell--enter-multiline-mode (buffer)
  "tell the shell to expect multiple lines of content.
Terminated by two successive newlines. "
  ;; in the shell buffer, set the local var that tells
  ;; this module to seek the prompt.
  (with-current-buffer buffer
    (jsshell-log 1 "enter multiline input mode: %s (%s)"
                           (buffer-name buffer)
                           jsshell--awaiting-command-prompt)
    (setq jsshell--awaiting-command-prompt t))

  ;;(comint-simple-send (get-buffer-process current) "\n")
  (comint-send-string buffer "\n")
  ;; (with-current-buffer buffer
  ;;   (jsshell--nudge))

  ;; (with-current-buffer buffer
  ;;   (comint-send-input nil t))
  )



;;;###autoload
(defun jsshell-send-region (start end &optional buffer)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (let ((buffer (jsshell buffer))
        (jscode (jsshell--squish-jscode
                 (buffer-substring-no-properties start end))))

    (jsshell--enter-multiline-mode buffer)

    (comint-simple-send (get-buffer-process buffer)
                        jscode)

    ;; (comint-send-string buffer jscode)
    ;; (comint-send-string buffer "\n")
    ;; (comint-send-input nil t)

    ))


;;;###autoload
(defun jsshell-send-region-and-pop (start end &optional buffer)
  "Send the contents of the current region to the inferior
Javascript shell."
  (interactive "r")
  (let ((buffer (jsshell buffer)))
    (jsshell-send-region start end buffer)
    (jsshell-switch-to-js buffer)))

;;;###autoload
(defun jsshell-send-last-sexp-and-pop ()
  "Send the previous sexp to the inferior Js process, and pop to the buffer."
  (interactive)
  (jsshell-send-region-and-pop (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun jsshell-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (jsshell-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun jsshell-send-buffer ()
  "Send the entire contents of the current buffer to
the inferior Javascript shell."
  (interactive)
  (jsshell-send-region (point-min) (point-max)))

;;;###autoload
(defun jsshell-send-buffer-and-pop ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (jsshell-send-region-and-pop (point-min) (point-max)))

;;;###autoload
(defun jsshell-load-file (filename &optional buffer)
  "Enqueue the file named FILENAME to be loaded into the JSSHell
interpreter designated by BUFFER, or *JSShell* if no buffer supplied."
  (interactive "fjs file to load: ")
  (if (file-readable-p filename)
      (let ((buffer (jsshell buffer)))
        (with-current-buffer buffer
          (setq jsshell--file-load-queue
                (cons filename jsshell--file-load-queue))
          (comint-send-input t t)))
    (error "That file is not readable.")))


(defun jsshell-really-load-file (filename &optional buffer)
  "Load the file named FILENAME into the JSSHell interpreter designated by
BUFFER, or *JSShell* if no buffer supplied."
  (jsshell-log 2 "really load file (%s)" filename)
  (let ((jscode
         (if (file-readable-p filename)
             (jsshell--squish-jscode
              (with-temp-buffer
                (insert-file-contents filename)
                (buffer-substring-no-properties (point-min) (point-max))))
           ";"))

        (buffer (jsshell buffer)))

    (with-current-buffer buffer
      (jsshell--enter-multiline-mode buffer)
      (setq jsshell--input-recd-msg
            (format
             (if (file-readable-p filename)
                 "loading %s"
               "the file %s does not exist.") filename))
      (comint-simple-send (get-buffer-process buffer) jscode)
      (jsshell--nudge)
      (sleep-for 0.32)
      )))




;;;###autoload
(defun jsshell-load-file-and-pop (filename &optional buffer)
  "Load the file named FILENAME into the JSSHell interpreter designated by
BUFFER, or *JSShell* if no buffer supplied. The pop to that buffer."
  (interactive "fjs file to load: ")
  (let ((buffer (jsshell buffer)))
    (jsshell-load-file filename buffer)
    (jsshell-switch-to-js buffer)))


;;;###autoload
(defun jsshell-switch-to-js (&optional buffer move-to-eob)
  "Switch to the javascript process buffer.
With non-nil MOVE-TO-EOB argument, position cursor at end of buffer."
  (interactive "P")
  (let ((buffer (jsshell buffer)))
    (if (not buffer)
        (error "No current process buffer.  See variable `jsshell-buffer'")
      (jsshell-log 2 "Switch to buffer '%s'" (buffer-name buffer))
      (pop-to-buffer buffer)
      (when move-to-eob
        (push-mark)
        (goto-char (point-max))))))



(setq jsshell-profile
   (list "c:\\dev\\js\\json2.js"
         "c:\\dev\\js\\stringExtensions.js"
         "c:\\dev\\js\\arrayExtensions.js"
         ))

(provide 'jsshell)

;;; jsshell.el ends here

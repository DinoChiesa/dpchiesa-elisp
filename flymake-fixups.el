;;; flymake-fixups.el --- fixups for various bugs, shortcomings and problems in flymake

;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  09:56
;; Version: 0.1
;;

(defvar flymake-last-check-complete-time nil)

(eval-after-load "flymake"
  '(progn
     ;; (list-colors-display) ;; <-- to see a display of emacs colors and names
     (set-face-background 'flymake-errline "firebrick")
     (set-face-background 'flymake-warnline "DarkOrange4")

     (setq flymake-gui-warnings-enabled nil)
     (setq flymake-no-changes-timeout 0.695)
     (setq flymake-min-recheck-interval 3.695)

     (set (make-local-variable 'flymake-last-check-complete-time) nil)

     ;; patched version of the timer event, and the process sentinel, to
     ;; prevent flymake from running too often.
     (defun flymake-on-timer-event (buffer)
       "Start a syntax check for buffer BUFFER if necessary.
This patched version of the method uses a new variable,
called `flymake-last-check-complete-time', to track whether
it is appropriate to run flymake again."
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (not flymake-is-running)
                      flymake-last-change-time
                      (> (- (flymake-float-time) flymake-last-change-time)
                         flymake-no-changes-timeout)
                      (or (not flymake-last-check-complete-time)
                          (> (- (flymake-float-time) flymake-last-check-complete-time)
                             flymake-min-recheck-interval)))

             (setq flymake-last-change-time nil)
             (flymake-log 3 "starting syntax check after interval has elapsed")
             (flymake-start-syntax-check)))))



     ;; fixup to track `flymake-last-check-complete-time'
     (defun flymake-process-sentinel (process event)
       "Sentinel for syntax check buffers."
       (when (memq (process-status process) '(signal exit))
         (let* ((exit-status       (process-exit-status process))
                (command           (process-command process))
                (source-buffer     (process-buffer process))
                (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

           (flymake-log 2 "process %d exited with code %d"
                        (process-id process) exit-status)
           (condition-case err
               (progn
                 (flymake-log 3 "cleaning up using %s" cleanup-f)
                 (when (buffer-live-p source-buffer)
                   (with-current-buffer source-buffer
                     (funcall cleanup-f)))

                 (delete-process process)
                 (setq flymake-processes (delq process flymake-processes))

                 (when (buffer-live-p source-buffer)
                   (with-current-buffer source-buffer

                     (flymake-parse-residual)
                     (flymake-post-syntax-check exit-status command)
                     (setq flymake-last-check-complete-time (flymake-float-time)
                           flymake-is-running nil))))
             (error
              (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                     source-buffer (error-message-string err))))
                (flymake-log 0 err-str)
                (with-current-buffer source-buffer
                  (setq flymake-last-check-complete-time (flymake-float-time)
                        flymake-is-running nil))))))))
     ;; -------------------------------------------------------


     ;; advice to set flag to allow emacs exit without query on any
     ;; active flymake processes.
     (defadvice flymake-start-syntax-check-process
       (after
        cheeso-advice-flymake-start-syntax-check-1
        (cmd args dir)
        activate compile)
       (set-process-query-on-exit-flag ad-return-value nil))


     ;; advice to forcibly replace the get-make-cmdline with a version
     ;; that uses nmake.exe, on Windows.
     (defadvice flymake-get-make-cmdline
       (after
        cheeso-advice-flymake-get-make-cmdline
        (source base-dir)
        activate compile)
       (if (eq system-type 'windows-nt)
           (progn
             (setq ad-return-value
                   (list nmake.exe
                         (list (concat "CHK_SOURCES=" source)
                               "SYNTAX_CHECK_MODE=1"
                               "check-syntax")))
             (flymake-log 3 "cheeso-advice-flymake-get-make-cmdline: %s"
                          (prin1-to-string ad-return-value)))))

     ))

(provide 'flymake-fixups)

;;; flymake-fixups.el ends here

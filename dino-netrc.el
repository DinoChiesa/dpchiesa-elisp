;;; dino-netrc.el --- fixed netrc parsing.

;; Copyright (C) 1989-1996, 1998, 2000-2013 Free Software Foundation,
;; Inc.

;; Derived from ange-ftp.el
;;
;; The netrc.el module provided with emacs does not handle netrc files
;; that use newlines as separators within records. This module fixes that.
;; It's SO SIMPLE!
;;
;; To use:
;; within a program call:
;;
;;   (dino-netrc-find "api.fiddlesticks.com")
;;
;; ...to get a list of (machine username password) for the
;; api.fiddlesticks.com host.
;;

;;; Code:

(defun dino-netrc-parse-token (token limit)
  (if (search-forward token limit t)
      (let (beg)
        (skip-chars-forward ", \t\r\n" limit)
        (if (eq (following-char) ?\")   ;quoted token value
            (progn (forward-char 1)
                   (setq beg (point))
                   (skip-chars-forward "^\"" limit)
                   (forward-char 1)
                   (buffer-substring beg (1- (point))))
          (setq beg (point))
          (skip-chars-forward "^, \t\r\n" limit)
          (buffer-substring beg (point))))))

(defun dino-netrc-parse-group ()
  (let ((start (point))
        (end (save-excursion
               (if (looking-at "machine\\>")
                   ;; Skip `machine' and the machine name that follows.
                   (progn
                     (skip-chars-forward "^ \t\r\n")
                     (skip-chars-forward " \t\r\n")
                     (skip-chars-forward "^ \t\r\n"))
                 ;; Skip `default'.
                 (skip-chars-forward "^ \t\r\n"))
               ;; Find start of the next `machine' or `default'
               ;; or the end of the buffer.
               (if (re-search-forward "machine\\>\\|default\\>" nil t)
                   (match-beginning 0)
                 (point-max))))
        machine login password account
        tuple)
    (setq machine  (dino-netrc-parse-token "machine"  end)
          login    (dino-netrc-parse-token "login"    end)
          password (dino-netrc-parse-token "password" end)
          account  (dino-netrc-parse-token "account"  end))
    (goto-char end)
    (and
     machine login
     (list machine login password))))


;; (defun dino-netrc-is-remote-host ()
;;   "return a list of (REMOTE-HOST SSH-PREFIX) if the `buffer-file-name'
;; represents a remote (ssh) file.  the SSH-PREFIX will be of the form
;; \"/ssh:remote-host:\"  Else return nil. "
;;   (let ((remote-host
;;          (save-match-data ; is usually a good idea
;;            (and
;;             (string-match "^/ssh:\\([^:]+\\):" dir)
;;             (match-string 1 dir)
;;             ))))
;;     (and remote-host
;;          (list remote-host
;;                (concat "/ssh:" remote-host ":")  ))))


(defun dino-netrc-parse ()
  "parse the netrc file. This should be cached! "
  (let ((file (expand-file-name "~/.netrc"))
        (default-directory "~/")
        (element nil)
        (parse-alist nil))
    (with-current-buffer (generate-new-buffer "*.netrc*")
      (insert-file-contents file)
      ;; (setq buffer-file-name file)
      ;; (setq default-directory (file-name-directory file))
      ;; (normal-mode t)
      ;; (run-hooks 'find-file-hook)
      (setq buffer-file-name nil);; not clear why
      (goto-char (point-min))
      (while (search-forward-regexp "^[ \t]*#.*$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (skip-chars-forward " \t\r\n")
      (while (not (eobp))
        (and (setq element (dino-netrc-parse-group))
             (add-to-list 'parse-alist element)))
      (kill-buffer (current-buffer)))
    parse-alist))

(defun dino-netrc-basic-auth-header (username &optional password)
  "produce an HTTP Basic Auth header value for a given USERNAME and PASSWORD.
Optionally, pass a list of (username password) as the first argument.
Or, pass in a string machine-name as the first argument , and it will look up the
pair in .netrc.

A typical use might be:
  (dino-netrc-basic-auth-header (cdr (dino-netrc-find machine-name)))

or
  (dino-netrc-basic-auth-header machine-name)
"
  (if (not password)
      (if (listp username)
          (setq password (cadr username)
                username (car username))
        (let ((result (cdr (dino-netrc-find username))))
          (setq username (car result)
                password (cadr result)))))

  (concat "Basic "
          (base64-encode-string
           (concat username ":" password))))

(defun dino-netrc-basic-auth-creds (machine-name)
  "retrieve a (USERNAME . PASSWORD) from .netrc for the given MACHINE-NAME.

A typical use might be:
  (dino-netrc-basic-auth \"api.enterprise.apigee.com\")
"
        (cdr (dino-netrc-find machine-name)))


(defun dino-netrc-find (machine)
  "parse .netrc for the given machine, return cons of (username . login)"
  (let ((tuples (dino-netrc-parse)))
      (assoc machine tuples)))

(provide 'dino-netrc)

;;; dino-netrc.el ends here

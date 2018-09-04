;;; gh-graphql.el --- An interactive HTTP client for Emacs, for connecting to github graphQL
;;
;; Public domain.

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a tool to use within emacs to explore and Github's graphQL service.
;; It is derived from pavel kurnosov's restcient.el
;; It runs queries from a plain-text query sheet, displays
;; results as a pretty-printed JSON.

;; To use, include this in your .emacs :
;; (require 'gh-graphql)
;; (setq gh-graphql-auth-token "my-user-token-for-github")
;;


;; Example file
;;
;; # -*- gh-graphql -*-
;; #
;; # URLs for querying Github via the GraphQL interface
;; # Yes, this runs in emacs.
;;
;; # It looks like this in curl:
;; #
;; # curl -H "Authorization: bearer $ghtoken" -X POST -d " \
;; #  { \
;; #    \"query\": \"query { viewer { login }}\" \
;; #  } \
;; # "
;;
;; #
;; # The gh-graphql.el module removes newlines and multiple
;; # spaces, and escapes the double-quotes, before wrapping the result in a
;; # JSON wrapper and then sending it off via HTTP.
;; #
;;
;; POST
;;
;; query {
;;   repository(owner:"octocat", name:"Hello-World") {
;;     issues(last:20, states:CLOSED) {
;;       edges {
;;         node {
;;           title
;;           url
;;           labels(first:5) {
;;             edges {
;;               node {
;;                 name
;;               }
;;             }
;;           }
;;         }
;;       }
;;     }
;;   }
;; }
;;


;;; Code:
;;
(require 'url)
(require 'json)
(require 's)

(defgroup gh-graphql nil
  "An interactive HTTP client in Emacs for the Github GraphQL API."
  :group 'tools)

(defcustom gh-graphql-log-request t
  "Log gh-graphql requests to *Messages*."
  :group 'gh-graphql
  :type 'boolean)

(defcustom gh-graphql-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time."
  :group 'gh-graphql
  :type 'boolean)

(defcustom gh-graphql-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :group 'gh-graphql
  :type 'string)

(defcustom gh-graphql-inhibit-cookies nil
  "Inhibit gh-graphql from sending cookies implicitly."
  :group 'gh-graphql
  :type 'boolean)

(defconst gh-graphql-api-endpoint "https://api.github.com/graphql")

(defvar gh-graphql-within-call nil)

(defvar gh-graphql-request-time-start nil)
(defvar gh-graphql-request-time-end nil)

(defvar gh-graphql-response-loaded-hook nil
  "Hook run after response buffer created and data loaded.")

(defvar gh-graphql-http-do-hook nil
  "Hook to run before making request.")

(defvar gh-graphql-auth-token nil
  "user token for github.")

(defvar gh-graphql--settings-file-base-name "gh-graphql.dat")

;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around gh-graphql-fix)
  (if gh-graphql-within-call
      (setq success t ad-return-value t)
    ad-do-it))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around gh-graphql-fix-2)
  (if gh-graphql-within-call
      (setq success t)
    ad-do-it))
(ad-activate 'url-cache-extract)

(defadvice url-http-user-agent-string (around gh-graphql-fix-3)
  (if gh-graphql-within-call
      (setq ad-return-value nil)
    ad-do-it))
(ad-activate 'url-http-user-agent-string)

(defun gh-graphql-restore-header-variables ()
  (url-set-mime-charset-string)
  (setq url-mime-language-string nil)
  (setq url-mime-encoding-string nil)
  (setq url-mime-accept-string nil)
  (setq url-personal-mail-address nil))

(defun gh-graphql--path-to-settings-file ()
  "a function rturning the path to the cache file for gh-graphql.el"
  (gh-graphql--join-path-elements user-emacs-directory gh-graphql--settings-file-base-name))

(defun gh-graphql--join-path-elements (root &rest dirs)
  "Joins a series of directories together, inserting slashes as necessary,
like Python's os.path.join."
  (if (not dirs)
      root
    (apply 'edge--join-path-elements
           (let ((first (car dirs)))
             (if (s-suffix? "/" root)
                 (concat root
                         (if (s-prefix? "/" first)
                             (substring first 1 (length first))
                           first))
               (if (s-prefix? "/" first)
                   (concat root first)
                 (concat root "/" (car dirs)))))
           (cdr dirs))))

(defun gh-graphql--restore-token ()
  "function expected to be called on initial module load, that reads the token for Github from the cache file."
  (if (not gh-graphql-auth-token)
      (let ((dat-file-path (gh-graphql--path-to-settings-file)))
        (if (file-exists-p dat-file-path)
            (with-temp-buffer
              (insert-file-contents dat-file-path)
              (save-excursion
                (goto-char (point-min))
                (let ((settings-data (read (current-buffer))))
                  (dolist (one-setting settings-data)
                    (let ((setting-name (car one-setting)))
                      (if (and
                           (string= "gh-graphql-auth-token" setting-name)
                           (cadr one-setting))
                          (set (intern setting-name) (cadr one-setting))))))))))))

(defun gh-graphql-save-auth-token ()
  "save `gh-graphql-auth-token' to the cache file."
  (if gh-graphql-auth-token
      (let ((dat-file-path (gh-graphql--path-to-settings-file))
            (alist-to-write (list
             (list "gh-graphql-auth-token" gh-graphql-auth-token))))
    (with-temp-file dat-file-path
      (goto-char (point-min))
      (erase-buffer)
      (insert ";; settings for gh-graphql.el")
      (newline)
      (insert (concat ";; Stored: " (current-time-string)))
      (newline)
      (let ((print-length nil)) ;; to avoid truncating
        (pp alist-to-write (current-buffer)))))))

(defun gh-graphql-y-or-n (raw-prompt &optional default-yes)
  "displays PROMPT in the minibuffer, prompts for a y or n,
    returns t or nil accordingly. If neither Y or N is entered, then
    if DEFAULT-YES, returns t, else nil."
  (let* ((options-string (if default-yes "Y or n" "y or N"))
         (prompt (concat raw-prompt "(" options-string ")? "))
         (cursor-in-echo-area t)
         (key (read-key (propertize prompt 'face 'minibuffer-prompt)))
         (isyes (or (eq key ?y) (eq key ?Y)))
         (isno (or (eq key ?n) (eq key ?N))))
    (if (not (or isyes isno))
        default-yes
      isyes)))

(defun gh-graphql--authorization-header ()
  "returns the authorization header for the GraphQL Github API."
  (let ((token
         (or gh-graphql-auth-token
             (read-string "Github user token: " nil))))
    (if (and token (not gh-graphql-auth-token))
        (progn
          (setq gh-graphql-auth-token token)
          (if (gh-graphql-y-or-n "Save for next time ")
                (gh-graphql-save-auth-token))))
    (cons "Authorization" (concat "Bearer " token))))


(defun gh-graphql--default-headers ()
  "returns the default HTTP header for the Github GraphQL API."
  (list
   '( "user-agent" . "gh-graphql.el")
   (gh-graphql--authorization-header)))


(defun gh-graphql-http-do (method url headers entity &rest handle-args)
  "Send ENTITY and HEADERS to URL as a METHOD request."
  (if gh-graphql-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity))
  (let ((url-request-method method)
        (url (replace-regexp-in-string "#" "%23" (s-trim url)))
        (url-request-extra-headers '())
        (url-request-data (encode-coding-string entity 'utf-8)))

    (gh-graphql-restore-header-variables)

    (dolist (header headers)
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (cdr header))
          (setq url-request-extra-headers (cons header url-request-extra-headers)))
        ))

    (setq gh-graphql-within-call t)
    (setq gh-graphql-request-time-start (current-time))
    (run-hooks 'gh-graphql-http-do-hook)
    (url-retrieve url 'gh-graphql-http-handle-response
                  (append (list method url (if gh-graphql-same-buffer-response
                            gh-graphql-same-buffer-response-name
                          (format "*HTTP %s %s*" method url))) handle-args) nil gh-graphql-inhibit-cookies)))

(defvar gh-graphql-content-type-regexp "^Content-[Tt]ype: \\(\\w+\\)/\\(?:[^\\+\r\n]*\\+\\)*\\([^;\r\n]+\\)")

(defun gh-graphql-prettify-response (method url)
  (save-excursion
    (let ((start (point)) (guessed-mode) (end-of-headers))
      (while (and (not (looking-at "^\\s-*$"))
                  (eq (progn
                        (when (looking-at gh-graphql-content-type-regexp)
                          (setq guessed-mode
                                (cdr (assoc-string (concat
                                                    (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                                    "/"
                                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2))
                                                    )
                                                   '(("text/xml" . xml-mode)
                                                     ("application/xml" . xml-mode)
                                                     ("application/json" . js-mode)
                                                     ("application/x-yaml" . yaml-mode)
                                                     ("image/png" . image-mode)
                                                     ("image/jpeg" . image-mode)
                                                     ("image/jpg" . image-mode)
                                                     ("image/gif" . image-mode)
                                                     ("text/html" . html-mode))))))
                        (forward-line)) 0)))
      (setq end-of-headers (point))
      (while (and (looking-at "^\\s-*$")
                  (eq (forward-line) 0)))
      (unless guessed-mode
        (setq guessed-mode
              (or (assoc-default nil
                                 ;; magic mode matches
                                 '(("<\\?xml " . xml-mode)
                                   ("{\\s-*\"" . js-mode))
                                 (lambda (re _dummy)
                                   (looking-at re))) 'js-mode)))
      (let ((headers (buffer-substring-no-properties start end-of-headers)))
        (when guessed-mode
          (delete-region start (point))
          (unless (eq guessed-mode 'image-mode)
            (apply guessed-mode '())
            (if (fboundp 'font-lock-flush)
                (font-lock-flush)
              (with-no-warnings
                (font-lock-fontify-buffer))))

          (cond
           ((eq guessed-mode 'xml-mode)
            (goto-char (point-min))
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char) (insert "\n"))
            (indent-region (point-min) (point-max)))

           ((eq guessed-mode 'image-mode)
            (let* ((img (buffer-string)))
              (delete-region (point-min) (point-max))
              (fundamental-mode)
              (insert-image (create-image img nil t))))

           ((eq guessed-mode 'js-mode)
            (let ((json-special-chars (remq (assoc ?/ json-special-chars) json-special-chars)))
              (ignore-errors (json-pretty-print-buffer)))
            (gh-graphql-prettify-json-unicode)))

          (goto-char (point-max))
          (or (eq (point) (point-min)) (insert "\n"))
          (let ((hstart (point)))
            (insert method " " url "\n" headers)
            (insert (format "Request duration: %fs\n" (float-time (time-subtract gh-graphql-request-time-end gh-graphql-request-time-start))))
            (unless (eq guessed-mode 'image-mode)
              (comment-region hstart (point))
              (indent-region hstart (point)))))))))

(defun gh-graphql-prettify-json-unicode ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\[Uu]\\([0-9a-fA-F]\\{4\\}\\)" nil t)
      (replace-match (char-to-string (decode-char 'ucs (string-to-number (match-string 1) 16))) t nil))))

(defun gh-graphql-http-handle-response (status method url bufname raw stay-in-window)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (setq gh-graphql-within-call nil)
  (setq gh-graphql-request-time-end (current-time))
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
    (gh-graphql-restore-header-variables)
    (when (buffer-live-p (current-buffer))
      (with-current-buffer (gh-graphql-decode-response
                            (current-buffer)
                            bufname
                            gh-graphql-same-buffer-response)
        (unless raw
          (gh-graphql-prettify-response method url))
        (buffer-enable-undo)
        (run-hooks 'gh-graphql-response-loaded-hook)
        (if stay-in-window
            (display-buffer (current-buffer) t)
          (switch-to-buffer-other-window (current-buffer)))))))

(defun gh-graphql-decode-response (raw-http-response-buffer target-buffer-name same-name)
  "Decode the HTTP response using the charset (encoding) specified in the Content-Type header. If no charset is specified, default to UTF-8."
  (let* ((charset-regexp "Content-Type.*charset=\\([-A-Za-z0-9]+\\)")
         (image? (save-excursion
                   (search-forward-regexp "Content-Type.*[Ii]mage" nil t)))
         (encoding (if (save-excursion
                         (search-forward-regexp charset-regexp nil t))
                       (intern (downcase (match-string 1)))
                     'utf-8)))
    (if image?
        ;; Dont' attempt to decode. Instead, just switch to the raw HTTP response buffer and
        ;; rename it to target-buffer-name.
        (with-current-buffer raw-http-response-buffer
          ;; We have to kill the target buffer if it exists, or `rename-buffer'
          ;; will raise an error.
          (when (get-buffer target-buffer-name)
            (kill-buffer target-buffer-name))
          (rename-buffer target-buffer-name)
          raw-http-response-buffer)
      ;; Else, switch to the new, empty buffer that will contain the decoded HTTP
      ;; response. Set its encoding, copy the content from the unencoded
      ;; HTTP response buffer and decode.
      (let ((decoded-http-response-buffer
             (get-buffer-create
              (if same-name target-buffer-name (generate-new-buffer-name target-buffer-name)))))
        (with-current-buffer decoded-http-response-buffer
          (setq buffer-file-coding-system encoding)
          (save-excursion
            (erase-buffer)
            (insert-buffer-substring raw-http-response-buffer))
          (kill-buffer raw-http-response-buffer)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) encoding)
            (error
             (message (concat "Error when trying to decode http response with encoding: "
                              (symbol-name encoding)))))
          decoded-http-response-buffer)))))

(defconst gh-graphql-method-regexp
  "^\\(POST\\)[[:space:]]*$")

(defconst gh-graphql-header-regexp
  "^\\([^ :]+\\): \\(.*\\)$")

(defconst gh-graphql-var-regexp
  "^\\(:[^: ]+\\)\\s-+\\(:?\\)=\\s-+\\(.+\\)$")

(defconst gh-graphql-evar-regexp
  "^\\(:[^: ]+\\)\\s-+:=\\s-+\\(.+\\)$")

(defun gh-graphql-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^#")
        (if (re-search-forward "^[^#]" (point-max) t)
            (point-at-bol) (point-max))
      (if (re-search-backward "^#" (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun gh-graphql-current-max ()
  (save-excursion
    (if (re-search-forward "^#" (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (point-max))))

(defun gh-graphql-replace-all-in-string (replacements s)
  (if replacements
      (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                                (lambda (key) (cdr (assoc key replacements)))
                                s nil t)
    s))

;; (defun gh-graphql-remove-newlines-from-string (s)
;;   (replace-regexp-in-string "\n" "" s nil t))

(defun gh-graphql-reformat-whitespace-in-string (s)
  (replace-regexp-in-string " +" " "
                            (replace-regexp-in-string "\n" "" s nil t)
                            nil t))

(defun gh-graphql-wrap-in-query (s)
  (if (string-match "^ *{ \"query\"" s)
      s
  (concat "{ \"query\" : \""
          (replace-regexp-in-string "\"" "\\\"" s nil t)
          "\" }")))


(defun gh-graphql-replace-all-in-header (replacements header)
  (cons (car header)
        (gh-graphql-replace-all-in-string replacements (cdr header))))

(defun gh-graphql-replace-all-in-headers (replacements headers)
  (mapcar (apply-partially 'gh-graphql-replace-all-in-header replacements) headers))

(defun gh-graphql-find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp gh-graphql-var-regexp bound t)
        (let ((name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (should-eval (> (length (match-string 2)) 0))
              (value (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
          (setq vars (cons (cons name (if should-eval (gh-graphql-eval-var value) value)) vars))))
      vars)))

;; this function can be used to refer to a var, in an eval var decl,
;; eg, :basicauth := (base64-encode-string (concat (gh-graphql-var ":username") ":" (gh-graphql-var ":pwd"))
(defun gh-graphql-var (name)
  (cdr (assoc name vars)))

(defun gh-graphql-eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun gh-graphql--trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun gh-graphql-http-parse-current-and-do (func &rest args)
  (save-excursion
    (goto-char (gh-graphql-current-min))
    (when (re-search-forward gh-graphql-method-regexp (point-max) t)
      (let ((url gh-graphql-api-endpoint)
            (method (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
            (headers (gh-graphql--default-headers)))
        (forward-line)
        (while (re-search-forward gh-graphql-header-regexp (point-at-eol) t)
          (setq headers (cons (cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                              headers))
          (forward-line))
        (when (looking-at "^\\s-*$")
          (forward-line))
        (let* ((cmax (gh-graphql-current-max))
               (entity (gh-graphql--trim-right (buffer-substring-no-properties (min (point) cmax) cmax)))
               (vars (gh-graphql-find-vars-before-point))
               (url (gh-graphql-replace-all-in-string vars url))
               (headers (gh-graphql-replace-all-in-headers vars headers))
               (entity (gh-graphql-replace-all-in-string vars entity))
               (entity (gh-graphql-reformat-whitespace-in-string entity))
               (entity (gh-graphql-wrap-in-query entity))
               )
          (apply func method url headers entity args))))))

(defun gh-graphql-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the clipboard."
  (interactive)
  (gh-graphql-http-parse-current-and-do
   '(lambda (method url headers entity)
      (kill-new (format "curl -i %s -X %s \\\n '%s'%s"
                        (mapconcat (lambda (header) (format "-H '%s: %s' \\\n" (car header) (cdr header))) headers " ")
                        method
                        url
                        (if (> (string-width entity) 0)
                            (format "\\\n -d '%s'" entity) "")))
      (message "curl command copied to clipboard."))))

;;;###autoload
(defun gh-graphql-http-send-current (&optional raw stay-in-window)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (gh-graphql-http-parse-current-and-do 'gh-graphql-http-do raw stay-in-window))

;;;###autoload
(defun gh-graphql-http-send-current-raw ()
  "Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)."
  (interactive)
  (gh-graphql-http-send-current t))

;;;###autoload
(defun gh-graphql-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (gh-graphql-http-send-current nil t))

(defun gh-graphql-jump-next ()
  "Jump to next request in buffer."
  (interactive)
  (let ((last-min nil))
    (while (not (eq last-min (goto-char (gh-graphql-current-min))))
      (goto-char (gh-graphql-current-min))
      (setq last-min (point))))
  (goto-char (+ (gh-graphql-current-max) 1))
  (goto-char (gh-graphql-current-min)))

(defun gh-graphql-jump-prev ()
  "Jump to previous request in buffer."
  (interactive)
  (let* ((current-min (gh-graphql-current-min))
         (end-of-entity
          (save-excursion
            (progn (goto-char (gh-graphql-current-min))
                   (while (and (or (looking-at "^\s*\\(#.*\\)?$")
                                   (eq (point) current-min))
                               (not (eq (point) (point-min))))
                     (forward-line -1)
                     (beginning-of-line))
                   (point)))))
    (unless (eq (point-min) end-of-entity)
      (goto-char end-of-entity)
      (goto-char (gh-graphql-current-min)))))

(defun gh-graphql-mark-current ()
  "Mark current request."
  (interactive)
  (goto-char (gh-graphql-current-min))
  (set-mark-command nil)
  (goto-char (gh-graphql-current-max))
  (backward-char 1)
  (setq deactivate-mark nil))

(defvar gh-graphql-mode-keywords
  (list
   ;; (list gh-graphql-method-regexp '(1 font-lock-keyword-face) '(2 font-lock-function-name-face))
   (list gh-graphql-method-regexp '(1 font-lock-keyword-face) )
   (list gh-graphql-header-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
   (list gh-graphql-evar-regexp '(1 font-lock-preprocessor-face) '(2 font-lock-function-name-face))
   (list gh-graphql-var-regexp '(1 font-lock-preprocessor-face) '(3 font-lock-string-face))
   ))

(defvar gh-graphql-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode gh-graphql-mode fundamental-mode "Github GraphQL Client"
  "Turn on gh-graphql mode."
  (local-set-key (kbd "C-c C-c") 'gh-graphql-http-send-current)
  (local-set-key (kbd "C-c C-r") 'gh-graphql-http-send-current-raw)
  (local-set-key (kbd "C-c C-v") 'gh-graphql-http-send-current-stay-in-window)
  (local-set-key (kbd "C-c C-n") 'gh-graphql-jump-next)
  (local-set-key (kbd "C-c C-p") 'gh-graphql-jump-prev)
  (local-set-key (kbd "C-c C-.") 'gh-graphql-mark-current)
  (local-set-key (kbd "C-c C-u") 'gh-graphql-copy-curl-command)
  (gh-graphql--restore-token)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(gh-graphql-mode-keywords)))


(provide 'gh-graphql)
;;; gh-graphql.el ends here

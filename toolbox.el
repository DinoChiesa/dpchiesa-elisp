;;
;; various utility functions that may be useful elsewhere
;;
;; Wed, 28 Mar 2012  11:14
;;


(defvar flyjs-reg-exe
  (concat (getenv "windir") "\\system32\\reg.exe"))

(when (not (fboundp 'string/trim-trailing-newlines))
  (defun string/trim-trailing-newlines (string)
    (while (string-match "\\(.*\\)\\(\n\\|\r\\)$" string)
        (setq string (substring string 0 -1))) ;; remove newline
      string))

(when (not (fboundp 'string/ends-with))
  (defun string/ends-with (s ending)
    "return non-nil if string S ends with ENDING"
    (let ((elength (length ending)))
      (string= (substring s (- 0 elength)) ending))))


(when (not (fboundp 'string/starts-with))
  (defun string/starts-with (s arg)
    "returns t if string S starts with ARG.  Else nil."
    (cond ((>= (length s) (length arg))
           (string-equal (substring s 0 (length arg)) arg))
          (t nil))))

(when (not (fboundp 'string/last-index-of))
  (defun string/last-index-of (s c)
    "Returns the index of the last occurrence of character C in string S.
Returns nil if not found.

"
    (let ((i (1- (length s)))
          ix c2)
      (while (and (>= i 0) (not ix))
        (setq c2 (aref s i))
        (if (= c c2)
            (setq ix i))
        (decf i))
      ix)))



    (defun get-matching-files-sorted-by-time (spec)
      "Get a list of filenames, with names matching SPEC. The files
    are ordered according to the last-modified time, with the most
    recently-modified file first.
    "
      ;; in lieu of flet
      (let ((by-mtime (lambda (a b)
                       (let ((a-mtime (nth 5 (file-attributes a)))
                             (b-mtime (nth 5 (file-attributes b))))
                         (or (> (nth 0 a-mtime) (nth 0 b-mtime))
                             (and
                              (eq (nth 0 a-mtime) (nth 0 b-mtime))
                              (> (nth 1 a-mtime) (nth 1 b-mtime))))))))

      (let ((allfiles (file-expand-wildcards spec)))
        (sort allfiles 'by-mtime))))



(defun smart-compile-any (predicate sequence)
  "Return true if PREDICATE is true of any element of SEQUENCE.
Otherwise nil.

If non-nil, the actual value will be a list, the car of which is
the first element in the sequence to return a non-nil result from
PREDICATE.

"
  (while (and sequence (not (funcall predicate (car sequence))))
    (setf sequence (cdr sequence)))
  sequence)


    (defun my-any (predicate sequence)
      "Return true if PREDICATE is true of any element of SEQUENCE.
    If so, return the true (non-nil) value returned by PREDICATE.
    (fn PREDICATE SEQ...)"
      (let (result)
        (mapc '(lambda (elt)
                 (or result
                     (setq result (funcall predicate elt))))
              sequence)
        result))

    (defun file-contents-as-string (filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-substring-no-properties (point-min) (point-max))))


;; used in flymake-for-jslint.
;;

;;
;; used in jsshell.el to produce a bundle file
;;

(defun jsshell-produce-bundle (&optional jsshell-el bundle-el jsshell-js)
  "Produce a new .el file, which contains all the jsshell.el
function and also embeds the jsshell.js source as a string. The
resulting .el file will then be suitable for a one-file
distribution of JSShell.

JSShell depends on two pieces: jsshell.el and jsshell.js. Rather
than distributing and installing two distinct files, the bundle
embeds the .js file into the .el file, for a one-file
distribution option. This function produces that one file.

Most people will never need to use this function. It's useful only
after modifying either the original jsshell.el or the jsshell.js file,
when you want to produce a new distributable bundle. In other words, it's
useful for the developer of jsshell.el.

"
  (let ((jsshell-el (or jsshell-el
                        (concat (file-name-directory jsshell--load-path) "jsshell.el")
                        "jsshell.el")))
    (let ((bundle-el  (or bundle-el
                          (concat (file-name-directory jsshell-el) "jsshell-bundle.el")))
          (jsshell-js (or jsshell-js
                          (and jsshell-js-tmpf
                               (file-readable-p jsshell-js-tmpf)
                               jsshell-js-tmpf)
                          jsshell-location-of-jsshell-js ;; orig dev wkstation
                          (concat (file-name-directory jsshell-el) "jsshell.js"))))
      (with-temp-file bundle-el
        (insert (concat
                 ";;; "
                 (file-name-nondirectory bundle-el)
                 " -- JSShell generated bundle\n"))
        (insert (concat ";;\n;; generated " (current-time-string) "\n;;\n\n"))
        (insert-file-contents jsshell-el)
        (goto-char (point-max))
        (insert "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
        (insert ";; this is the embedded Javascript code for the JS REPL\n\n")
        (goto-char (point-max))
        (insert (concat "(setq jsshell-js-src "
                        (pp-to-string (jsshell--minimized-js-contents jsshell-js))
                        ")\n"
                        "\n(provide '"
                        (file-name-sans-extension (file-name-nondirectory bundle-el))
                        ")\n"
                        "\n;;; "
                        (file-name-nondirectory bundle-el)
                        " ends\n"))))))



;;
;; used in jsshell.el to squish javascript
;;
(defun jsshell--minimized-js-contents (file)
  "returns the minimized JS version of the contents of
the specified FILE."
  (and (file-readable-p file)
       (let ((re-pairs '(("[\s\t]*//.*$" "") ;; javascript comments
                         ("^\n" "")          ;; eliminate leading newlines
                         ("\n[\s\t]+" "\n")  ;; collapse whitespace
                         ("\n\n" "\n")       ;; collapse newlines
                         ("\n}" "}")
                         ("{\n" "{")
                         ("}\n" "}")
                         (";\n" ";")         ;; replace semi-newline with semi
                         ("\n$" ""))))       ;; trailing newline
         (with-temp-buffer
           (insert-file-contents file)
           (mapc (lambda (pair)
                   (goto-char (point-min))
                   (while (re-search-forward (car pair) nil t)
                     (replace-match (cadr pair) nil nil)))
                 re-pairs)
           (buffer-substring-no-properties (point-min) (point-max))))))



(x-popup-menu (thesaurus-get-menu-position)
              (list "Title here\nSecond line"
                    (list "breakout1"
                          '("OK?" . 0)
                          '("Not OK?" . 1))
                    (list "breakout #2"
                          '("Yes" . 10)
                          '("No" . 20)
                          '("Maybe" . 30))))


(if (eq system-type 'darwin)
  ; something for OS X if true
  ; optional something if not
)

(if (eq system-type 'windows-nt)
  ; something for OS X if true
  ; optional something if not
)



    (defvar bundlify-powershell-exe
      (concat
       (getenv "windir")
       "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))


    (defun wget-via-powershell (url &optional ext)
      "get the contents of a URL into a file via powershell. The EXT
            is the extension to use on the destination file. It should begin
            with a dot.
            "
      (let (rris)
        (fset 'rris 'replace-regexp-in-string)
        (let* ((ext (or ext ".txt"))
               (url-tmpf (make-temp-file "emacs-wget-" nil ext))
               (ps-cmd (concat
                        "(new-object System.Net.WebClient).DownloadFile("
                        (rris (char-to-string 34)
                              (char-to-string 39)
                              (pp-to-string url))
                        ","
                        (rris "/"
                              "\\\\"
                              (rris (char-to-string 34)
                                    (char-to-string 39)
                                    (pp-to-string url-tmpf)))
                        ")"))
               (shell-command
                (format "%s -Command %s"
                        bundlify-powershell-exe
                        (concat "\"& {" ps-cmd "}\""))))

          (shell-command-on-region (point) (point)
                                   shell-command
                                   nil nil nil)
          ;;(message "downloaded to %s" url-tmpf)
          url-tmpf)))



;; (defun jscomp-fixup-json2-script ()
;;   "Edit the json2.js script. The script as it arrives does
;; not work with eval(), which is what we use in a cscript.exe to
;; include the module. "
;;   (let ((filename jscomp-json2-script-location))
;;     (with-temp-buffer
;;       (insert-file-contents filename)
;;       (when (re-search-forward "var JSON;" nil t)
;;         (goto-char (line-beginning-position))
;;         (insert "// ")
;;         (forward-line 1)
;;         (insert "// ")
;;         (forward-line 1)
;;         (insert "// ")
;;         (forward-line 1)
;;         (insert "// "))
;;       (when (re-search-forward "^(function" nil t)
;;         (delete-region (line-beginning-position) (line-end-position))
;;         (goto-char (line-end-position))
;;         (insert "\n(function (scope) {\n    var JSON={};\n"))
;;       (when (re-search-forward "^}())" nil t)
;;         (delete-region (line-beginning-position) (line-end-position))
;;         (goto-char (line-end-position))
;;         (insert "\n    scope.JSON = JSON;\n\n}(this));"))
;;       (write-region (point-min)
;;                     (point-max)
;;                     filename))))


(defun msgbox-via-powershell (format-string &rest args)
  "display a message box via powershell and Windows Forms.
    "
  (let (rris)
    (fset 'rris 'replace-regexp-in-string)
    (let ((msg (format format-string args)))
      (let ((powershell-exe  (concat
                              (getenv "windir")
                              "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))
            (ps-cmd
             (concat "[void][System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms');"
                     "[Windows.Forms.MessageBox]::Show("
                     (mapconcat '(lambda (elt)
                                   (rris (char-to-string 34)
                                         (char-to-string 39)
                                         (pp-to-string
                                          (rris (char-to-string 34)
                                                "'+[char]0x0022+'"
                                                (rris (char-to-string 39)
                                                      "'+[char]0x0027+'"
                                                      elt)
                                                ))))
                                (split-string msg "\n" nil)
                                "+[char]0x000D+")
                     ",'Message from Emacs',"
                     "[Windows.Forms.MessageBoxButtons]::OK,"
                     "[Windows.Forms.MessageBoxIcon]::Information)"))

            shell-command)

        (setq shell-command
              (format "%s -Command %s"
                      powershell-exe
                      (concat "\"& {" ps-cmd "}\"")))

        (shell-command-on-region (point) (point)
                                 shell-command
                                 nil nil nil)))))




(defadvice message-box (around
                        cheeso-advice-message-box-1
                        (format-string &rest args)
                        activate compile)
  "Replace message-box on Windows"
 (if (eq system-type 'windows-nt)
     (msgbox-via-powershell format-string args)
   ad-do-it))



(msgbox-via-powershell "Hello!\nI must be \"going\"!")
(message-box "Hello!\nI must be \"going\"!")



(defun multiline-message-box (msg)
  "display a multiline message box on Windows.

        According to bug #11138, when passing a message with newlines to
        `message-box' on Windows, the rendered message-box appears all on
        one line.

        This function can work around that problem.
        "

  (let ((ok (lambda (&optional p1 &rest args) t)))

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


(multiline-message-box "Hello!\nI must be going!\nThis is line 3.")





(defun break-into-lines (string)
  "break  a string into a set of strings, using the
newline char as a delimiter."

  (split-string string "\n" nil))

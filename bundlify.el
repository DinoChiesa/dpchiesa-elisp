;;; bundlify.el --- fiddling with downloads and dynamic modules
;;
;; Sometimes you want a module that depends on some external file or
;; resource.  For example, the flymake-for-js-for-wsh stuff depends on
;; jslint.js or jshint.js , which are large external resources. Wouldn't
;; it be nice to be able to download this content at runtime, and then use it?
;;
;; idea:
;;
;; flymake-for-jshint-for-wsh,... upon startup...
;;
;;  1. looks for a cache file to find the doctored jshint.js script
;;
;;  2. if it finds it, then commands are known thereafter.
;;
;;  3. if not found, then downloads jshint.js, adding the WSH
;;      boilerplate to it, saves it into a JS file, caches the location.
;;
;;
;; This allows flymake-for-jslint-for-wsh to be a one-file distribution.
;;

(require 'url)

(defvar jshint-src  "https://raw.github.com/jshint/jshint/master/jshint.js"
  "Source URL for JSHint")

(defvar wget-src  "https://cheesoexamples.svn.codeplex.com/svn/wget/wget.cs"
  "Source URL for wget.cs")

(defvar bundlify-wget-proggy "c:\\dev\\wget\\wget.exe")

(defun util--minimize-js (js-src)
  "returns the minimized version of the given JS source.
"
  (let ((re-pairs '(("[\s\t]*//.*$" "")      ;; javascript comments
                    ("^\n" "")               ;; eliminate leading newlines
                    ("\n[\s\t]+" "\n")       ;; collapse whitespace after newline
                    ("\n\n" "\n")            ;; collapse newlines
                    ("[\s\t]+:[\s\t]+" ":")  ;; collapse whitespace around colon
                    ("[\s\t]=[\s\t]" "=")    ;; collapse whitespace around equals
                    (",[\s\t]+" ",")         ;; collapse whitespace after comma
                    ("\n}" "}")
                    ("{\n" "{")
                    ("}\n" "}")
                    (";\n" ";")              ;; replace semi-newline with semi
                    ("\\(\r\\|\n\\)$" "")))) ;; trailing NL or CR

         (with-temp-buffer
           (insert js-src)
           (mapc (lambda (pair)
                   (goto-char (point-min))
                   (while (re-search-forward (car pair) nil t)
                     (replace-match (cadr pair) nil nil)))
                 re-pairs)
           (buffer-substring-no-properties (point-min) (point-max)))))


(defun bundlify-file-contents (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun bundlify-string-starts-with (s arg)
  "returns t if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))


(defun bundlify-dump-http-headers ()
  "In the buffer created by `url-retrieve-synchronously',
there are HTTP headers. This fn removes them. It deletes each
line until finding a blank line, which in normal HTTP signals the
end of the headers and the beginning of the message content.
"
  (while (/= (point) (line-end-position))
    (delete-region (point) (line-end-position))
    (delete-char 1))
  (delete-char 1))

(defvar bundlify-powershell-exe
  (concat
   (getenv "windir")
   "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))


(message "%s"
(pp-to-string bundlify-powershell-exe))
(message "%s" bundlify-powershell-exe)

;;(new-object System.Net.WebClient).DownloadFile('http://www.xyz.net/file.txt','C:\tmp\file.txt')


(defvar bundlify-download-file-src "(function(globalScope) {'use strict';var adStreamType={binary: 1,\ntext:2},\nsaveOptions={createNotExist:1,\noverwrite:2},\nopenOptions={read:1,\nwrite:2,\nappend:2},\nopenFormat={unicode: -1,\nascii:0,\nuseDefault: -2};function writeToFile (filename, content) {var fso=new ActiveXObject (\"Scripting.FileSystemObject\"),\ns=fso.openTextFile (filename, openOptions.write, openFormat.ascii);s.write(content);s.Close();}function downloadFile(url, filename) {var stream, xhr, msg;try {stream=new ActiveXObject('ADODB.Stream');}catch(e) {writeToFile(filename, \"Cannot create ADODB.Stream\");WScript.StdErr.Echo('not supported');return;}xhr=new ActiveXObject('MSXML2.XMLHTTP');xhr.open('GET', url, false);xhr.send();if (xhr.status == 200) {stream.Open();stream.Type=adStreamType.text;stream.Write(xhr.ResponseBody);stream.SaveToFile (filename, saveOptions.overwrite);stream.Close();}else {msg=\"unable to download that URL.\\nstatus code:\" +\nxhr.status;writeToFile(filename, msg);WScript.StdErr.Echo(msg);}}globalScope.Wget={download:downloadFile };}(this));")


(defun bundlify-get-url-contents-via-powershell (url)
  "get the contents of a URL via powershell. The EXT is the extension
to use on the destination file. It should begin with a dot.
"
  (let ((localfile (bundlify-wget-via-powershell url)))
    (bundlify-file-contents localfile)))


(defun bundlify-wget-via-powershell (url &optional ext)
  "get the contents of a URL into a file via powershell. The EXT
is the extension to use on the destination file. It should begin
with a dot.
"
  (let* ((ext (or ext ".txt"))
         (url-tmpf (make-temp-file "emacs-bundlify-" nil ext))
         (ps-cmd (concat
                  "(new-object System.Net.WebClient).DownloadFile("
                  (replace-regexp-in-string (char-to-string 34)
                                            (char-to-string 39)
                                            (pp-to-string url))
                  ","
                  (replace-regexp-in-string "/"
                                            "\\\\"
                  (replace-regexp-in-string (char-to-string 34)
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
    url-tmpf))




(bundlify-get-url-contents-via-powershell jshint-src)

(bundlify-get-url-contents-via-powershell  wget-src)



(defun bundlify-get-url-contents-via-wget-exe (url)
  "get contents of the given URL."
  (let ((buf (generate-new-buffer "*bundlify*"))
        c)
    (with-current-buffer buf
      (call-process
       bundlify-wget-proggy ;; PROGRAM
       nil                  ;; INFILE
       t                    ;; BUFFER
       nil                  ;; DISPLAY
       "-q" url "-")        ;; ARGS
      (setq c (buffer-substring-no-properties (point-min) (point-max))))
    ;;(kill-buffer buf)
    c))







(defun xx-bundlify-get-url-contents (url)
  "get contents of the given URL. On Windows, `url-retrieve-synchronously' works
only for non-HTTPS URLs. (shakes head sadly)
"
  (if (not (bundlify-string-starts-with source-f "http://"))
      (error "On Windows, `url-retrieve-synchronously' works only for non-HTTPS URLs.")
    (let ((buf (url-retrieve-synchronously url))
          c)
      (with-current-buffer buf
        (rename-buffer "*bundlify*"  t)
        (bundlify-dump-http-headers)
        (setq c (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer buf)
      c)))




(defun bundlify-produce-bundle (orig-el varname source-f &optional bundle-el minimizer-fn)
  "Produce a new .el file.  It will contains all the code from
the ORIG-EL file and also embeds a setq statement for variable
VARNAME containing the text content from file SOURCE-F as a
string. If SOURCE-F is a URL, then the content is taken from that
URL.

The resulting .el file, stored in BUNDLE-EL, will then be
suitable for a one-file distribution of something.  If BUNDLE-EL
is nil, then the bundle name will be derived from the name of the
original .el.  This may overwrite existing bundle files.

The MINIMIZER-FN, if non-nil, is invoked on the string contents
of the source file, before encoding it as a string int he setq
statement.  This is handy for embedding any source code, such as
elisp or javascript, that can be squished easily.

This fn is usable for any elisp module that depends on an external
source file.

Most people will never need to use this function. It's useful primarily
for developers of elisp modules.

"
  (if (not (file-readable-p orig-el))
      (error (format "The original el file, '%s', is not readable"
                     (prin1-to-string orig-el))))

  (let ((file-contents
         (if (or (bundlify-string-starts-with source-f "https://")
                 (bundlify-string-starts-with source-f "http://"))
             (bundlify-get-url-contents-via-powershell source-f)
           (if (not (file-readable-p source-f))
               (error (format "The source file to be embedded, '%s', is not readable"
                              (prin1-to-string source-f)))
             (bundlify-file-contents source-f)))))

    (let* ((name-sans-extension  (file-name-sans-extension (file-name-nondirectory orig-el)))
           (bundle-el  (or bundle-el
                           (concat (file-name-directory orig-el)
                                   name-sans-extension
                                   "-bundle.el"))))

      (with-temp-file bundle-el
        (insert (concat
                 ";;; "
                 (file-name-nondirectory bundle-el)
                 " --- generated bundle of "
                 (file-name-nondirectory orig-el)
                 " and "
                 (file-name-nondirectory source-f)
                 "\n"))
        (insert (concat ";;\n;; generated " (current-time-string) "\n;;\n\n"))
        (insert-file-contents orig-el)
        (goto-char (point-max))
        (insert "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
        (insert ";; this is the embedded source code\n\n")
        (goto-char (point-max))
        (insert (concat "(setq "
                        varname
                        " "
                        (pp-to-string
                          (or
                          (and minimizer-fn
                               (funcall minimizer-fn file-contents))
                          file-contents))
                        ")\n"
                        "\n(provide '"
                        (file-name-sans-extension (file-name-nondirectory bundle-el))
                        ")\n"
                        "\n;;; "
                        (file-name-nondirectory bundle-el)
                        " ends here\n"))))))


;;(setq bundlify-wget-proggy "c:\\dev\\wget\\wget.exe")


(bundlify-produce-bundle
 "~/flymake-for-jslint-for-wsh.el"
 "flyjs-jshint-for-wsh-src"
 jshint-src
 "~/flyjs-wsh-bundle.el"
 'util--minimize-js)


(bundlify-produce-bundle
 "~/flyjs-wsh-bundle.el"
 "flyjs-jshint-wsh-boilerplate-src"
 "~/jshint-boilerplate.js"
 "~/flyjs-wsh-b2.el"
 'util--minimize-js)


(bundlify-produce-bundle
 "~/b.el"
 "download-file-src"
 "~/jshint-boilerplate.js"
 "~/b-bundle.el"
 'util--minimize-js)



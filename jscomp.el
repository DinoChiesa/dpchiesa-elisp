;;; jscomp.el --- completion for js-mode

;; Author     : Dino Chiesa
;; Version    : 2012.4.24
;; Keywords   : javascript js completion languages Windows
;; URL        : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=jscomp.el
;; License    : New BSD
;; Last-saved : <2012-April-25 16:09:27>
;; Package-Requires: ()
;;

;;; Commentary:

;; This module provides logic to do completion on Javascript objects
;; in js-mode buffers. It requires Windows, WSH, and CScript.exe.

;; It depends on an external Javascript parser called 'Esprima'.
;; See http://esprima.org .

;; It works by parsing the javascript code of the current buffer, with
;; esprima, then using the return value to build and run another bit of
;; javascript which includes self-interrogation logic, running *that*
;; and using the result to build a popup menu of choices.

;; It is limited to completing on properties or methods on objects - the
;; things "after the dot".


;;; License:
;;
;; Licensed under the Simplified BSD License.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, is permitted provided that the following conditions
;; are met: Redistributions of source code must retain the above
;; copyright notice, this list of conditions and the following
;; disclaimer.  Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided with
;; the distribution.
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
;; ------------------------------------------------------------------

;;; Design Notes:
;;
;; The way it works: it depends on three distinct files that are
;; more-or-less boilerplate. The first two contain esprima and json2,
;; and are just regular Javascript. When either of these does not exist,
;; this module downloads the content and puts it into a file with a
;; well-known name in the temp directory.  The third file is a .wsf
;; file, known as the parser-driver. It includes, via script elements,
;; the esprima and json2 libraries, and adds a bit of boilerplate logic
;; that calls esprima.parse on the file named on the command line, and
;; formats the result of that parse as json with the JSON.stringify(),
;; and emits it to stdout.
;;
;; When the user requests completion, this module saves all content from
;; the currently-being-edited file, except for the current line , into a
;; temporary file, and parses it with the above parser driver.
;;
;; The result of that is just a parse tree. This module then converts
;; that into an elisp s-expression with the builtin json.el module.
;; Then, this module walks the parse tree, and emits another temporary
;; JS file that includes self-interrogation logic on synthetic variables
;; of the same type referencedin the original source module.  This thing
;; just prints out type information.  This module then collects that
;; information and builds a popup menu of choices for completion, with
;; that information.
;;
;; Easy peasy.



;;; Code:
(require 'json)
(load "w32-fns") ;; it does not provide!

;; (defcustom jscomp-try-use-chakra t
;;   "Whether to try to use the Chakra engine on Windows.")

;; (defvar jscomp-reg-exe
;;   (concat (getenv "windir") "\\system32\\reg.exe"))

(defvar jscomp-cscript-exe
  (concat (getenv "windir") "\\system32\\cscript.exe"))

(defvar jscomp-esprima-src-url
  "https://raw.github.com/ariya/esprima/master/esprima.js"
  "Source URL for Esprima")

(defvar jscomp-json2-src-url
  "https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
  "Source URL for json2")

(defvar jscomp-random-id nil
  "a random id")

;; =======================================================
;;
;; Chakra does not work with .wsf  !!
;;
;; I tried specifying Chakra as the language name in a
;; script tag within a .wsf, but ... when the script generated
;; an error, cscript.exe actually crashed. Not good!
;;
;; Too bad.
;;
;; So, all the work that goes into choosing Chakra or not, in order to
;; take advantage of the performance potential, is moot.
;;
;; The reason I use .wsf instead of just a flat .js file is to allow
;; simple inclusion of distinct .js modules, specifically esprima and
;; json2.
;;
;; =======================================================

;; (defvar jscomp-wsh-script-lang-name nil
;;   "This is the name of the language that will be requested on the
;; cscript.exe command line when running the jslint program.
;;
;; `jscomp.el' tries to infer the name by examining the
;; registry, the first time you use the module.  It selects Chakra
;; if it is available and selects Jscript otherwise.
;;
;; You can force this module to always use JScript by setting
;; `jscomp-try-use-chakra' to nil, before calling require on this
;; module.  You can also override what the module selects, by
;; setting this variable manually in your .emacs.  Take care to set
;; it to only JScript, Javascript, or a legal Javascript-compatible
;; language string that can be understood by cscript.exe's //E option.")
;;
;; (defun jscomp-reg-read (regpath)
;;   "Read a value from the Windows registry. This is used to
;; determine if the faster Chakra engine is available."
;;   (let (tokens last-token)
;;     (setq reg-value (shell-command-to-string
;;                      (concat jscomp-reg-exe " query " regpath))
;;           tokens (split-string reg-value nil t)
;;           last-token (nth (1- (length tokens)) tokens))
;;     (and (not (string= last-token "value.")) last-token)))

;; (defun jscomp-wsh-script-lang-name ()
;;   "sets and retrieves the language name used on the CScript.exe
;; command line."
;;   (or jscomp-wsh-script-lang-name
;;     (setq jscomp-wsh-script-lang-name
;;         (if (and jscomp-try-use-chakra
;;                  (string=
;;                   (upcase
;;                    (jscomp-reg-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\CLSID"))
;;                   "{16D51579-A30B-4C8B-A276-0FF4DC41E755}"))
;;             "Chakra" "JScript"))))


(defun jscomp-random-id ()
  "a random id, one per instance of emacs."
  (or jscomp-random-id
      (setq jscomp-random-id (format "%d" (random)))))

(defun jscomp-wget-via-powershell (url target-f)
  "Get the contents of a URL into a file named TARGET-F via
Windows Powershell. Since this module depends on Windows and
CScript.exe already, there's no concern with introducing a
further dependency on Powershell.

The reason we don't use the `url.el' package is because it
appears to be flaky on Windows when using SSL/TLS, and the
esprima is available in raw form on github which requires
https. Rather than figure out why `url.el' chokes so badly, just
work around the problem by using Powershell which is known good.

"
  ;; Need to handle quoting carefully. Replace 34 with 39 (single quote)
  ;; and take care to do the right thing with slashes in pathnames.
  (flet ((dequote (s)
                  (replace-regexp-in-string (char-to-string 34)
                                            (char-to-string 39)
                                            (pp-to-string s))))
    (let* ((ps-cmd (concat
                    "(new-object System.Net.WebClient).DownloadFile("
                    (dequote url)
                    ","
                    (replace-regexp-in-string
                     "/" "\\\\" (dequote target-f))
                    ")"))
           (shell-command
            (format "%s -Command %s"
                    flyjs-powershell-exe
                    (concat "\"& {" ps-cmd "}\""))))

      (shell-command-on-region (point) (point)
                               shell-command
                               nil nil nil))))


(defvar jscomp-esprima-script-location nil
  "internal use only")

(defvar jscomp-json2-script-location nil
  "internal use only")

(defvar jscomp-parser-driver-script-location nil
  "internal use only")


(defun jscomp-get-matching-files-by-time (spec)
  "Get a list of filenames, with names matching SPEC. The files
are ordered according to the last-modified time, with the most
recently-modified file first.
"
  (flet ((by-mtime (a b)
                   (let ((a-mtime (nth 5 (file-attributes a)))
                         (b-mtime (nth 5 (file-attributes b))))
                     (or (> (nth 0 a-mtime) (nth 0 b-mtime))
                         (and
                          (eq (nth 0 a-mtime) (nth 0 b-mtime))
                          (> (nth 1 a-mtime) (nth 1 b-mtime)))))))

  (let ((allfiles (file-expand-wildcards spec)))
    (sort allfiles 'by-mtime))))


(defun jscomp-js-fname (x &optional extension)
  "Generate a temp name to contain a .js file."
  (let ((ext (or extension ".js")))
  (concat
   (file-name-as-directory temporary-file-directory)
   "emacs." x ext)))


(defun jscomp-js-script-location (prefix &optional force-new extension)
  "gets the location of a JS script file, ."
  (let (flist
        loc
        (ext (or extension ".js")))
    (setq loc
          (if (or force-new
                  (not (setq flist (jscomp-get-matching-files-by-time
                                    (jscomp-js-fname (concat prefix ".*") ext)))))
              (jscomp-js-fname (format-time-string (concat prefix ".%Y%b%d")) ext)
            (nth 0 flist)))
    loc))


(defun jscomp-esprima-script-location (&optional force-new)
  "gets the location of the esprima parser script."
  (setq jscomp-esprima-script-location
        (jscomp-js-script-location "esprima" force-new)))

(defun jscomp-json2-script-location (&optional force-new)
  "gets the location of the esprima parser script."
  (setq jscomp-json2-script-location
        (jscomp-js-script-location "json2" force-new)))

(defun jscomp-parser-driver-script-location (&optional force-new)
  "gets the location of the parse driver script."
  (setq jscomp-parser-driver-script-location
        (jscomp-js-script-location "jscomp" force-new ".wsf")))


(defun jscomp-delete-any-stale-parser-driver-scripts ()
    ;; Delete any jscomp files that may exist, which may reference older
    ;; copies of the esprima library, which may no longer exist at this
    ;; time.
    (mapc '(lambda (x) (if (file-exists-p x) (delete-file x)))
          (jscomp-get-matching-files-by-time
           (jscomp-js-fname "jscomp.*" ".wsf"))))


(defun jscomp-download-script (url filename)
  "Download a script from the given URL, and save it
to the given FILENAME.

This fn is used to download Esprima and json2 from github.

"
  (if (file-exists-p filename)
        (delete-file filename))
  (jscomp-wget-via-powershell url filename)
  (jscomp-delete-any-stale-parser-driver-scripts))


(defun jscomp-parser-driver-wsf-boilerplate ()
  "Returns boilerplate code for a wsf to allow parsing of
a named js script file.

The way it works: when a user asks for completion, jscomp
executes cscript.exe, loading a javascript source file containing
this boilerplate code. This script, at runtime, reads all the
text of a file that contains the source from the buffer being
edited, and parses it with Esprima. It then emits the result of
that parse to stdout.
"
  (let ((esprima-f (jscomp-esprima-script-location))
        (json2-f (jscomp-json2-script-location)))
    (concat "<package>\n  <job id='parse'>
    <script language='javascript' src='"
            (file-name-nondirectory esprima-f)
            "'></script>
    <script language='javascript' src='"
            (file-name-nondirectory json2-f)
            "'></script>
    <script language='javascript'>
      (function() {
          var filename = null, text, p;
          function readAllText(filename) {
              var fso = new ActiveXObject('Scripting.FileSystemObject'),
                  fs = fso.openTextFile(filename),
                  fileData = fs.readAll();
              fs.Close();
              return fileData;
          }
          if (WScript.Arguments.length > 0) {
              filename = WScript.Arguments(0);
              text = readAllText(filename);
          } else {
              text = WScript.StdIn.ReadAll();
          }
          p = esprima.parse(text, {loc:true});
          WScript.Echo(JSON.stringify(p, null, 2));
      }());
    </script>
  </job>
</package>")))


(defun jscomp-get-parsable-js (begin end)
  "Return js code that can be inspected.

This gets all text from the current buffer except the current line.

This is used to support completion in a js-mode buffer.

"
  (concat
   (buffer-substring-no-properties (point-min) begin)
   ;; add an equivalent number of spaces
   (make-string (- end begin) 32)
   (buffer-substring-no-properties end (point-max))))


(defun jscomp-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(when (not (fboundp 'string/starts-with))
  (defun string/starts-with (s arg)
    "returns t if string S starts with ARG.  Else nil."
    (cond ((>= (length s) (length arg))
           (string-equal (substring s 0 (length arg)) arg))
          (t nil))))

(when (not (fboundp 'string/replace-char))
(defun string/replace-char (s c1 c2)
  "Replace all occurrences of char C1 in string with char C2.
Return the modified string."
  (let ((string-len  (length s))
        (ix 0))
    (while (< ix string-len)
      (if (eq (aref s ix) c1)
          (aset s ix c2))
      (incf ix)))
  s))


(defun jscomp-generate-menu (candidates stub)
  "Generate a menu suitable for use in `x-popup-menu' from the
list of CANDIDATES. Each item in the list of candidates is a
list, like (NAME TYPE) or, if TYPE is \"function\", then
sometimes (NAME TYPE ARGSTRING).

If STUB is non-nil, then include a candidate only if the NAME
begins with STUB.

"
    (list "Complete with..."
          (cons "Ignored pane title"
                (mapcar '(lambda (elt)
                           (if stub
                               (and (string/starts-with (nth 0 elt) stub)
                                    (cons (nth 0 elt) elt))
                             (cons (nth 0 elt) elt)))

                        candidates))))


(defun jscomp-type-inspector-boilerplate-1 ()
  "Returns boilerplate javascript code to allow type
introspection of the type of a variable.

The way it works: when a user asks for completion, jscomp
produces a javascript source file containing this boilerplate
code, followed by all code in the buffer being edited, plus some
trailer boilerplate code. The plan is to evaluate all of that
javascript, and get a string that describes the properties of the
thing being completed.

This works if the js-mode buffer is curly-brace complete, that is
to say, if it can be compiled. It also assumes that evaluating
the code doesn't emit anything to the StdOut.

It works in simple cases, not sure about more complex ones.

Some notes:

The boilerplate uses a random number to uniquify the variable
names, to avoid possibility of collision.

The properties of an object are enumerated with a for..in
loop. This doesn't work for built-in members, like those on the
String or Array or RegExp objects. For that reason this logic
special-cases String.  Not so much with RegExp.

"
  (let ((v (concat "emacsJscompHelper_" (jscomp-random-id)))
        (s (concat "stringProps_" (jscomp-random-id)))
        (a (concat "arrayProps_" (jscomp-random-id)))
        (json2-f (jscomp-json2-script-location)))
  (concat "<package>\n  <job id='inspect'>
    <script language='javascript' src='"
            (file-name-nondirectory json2-f)
            "'></script>
    <script language='javascript'>
    (function() {
      function say(x) {WScript.Echo(x);}
      var " a
      "= [
       ['length', null],
       ['join', '${1:separator}'],
       ['indexOf', '${1:item}'],
       ['lastIndexOf', '${1:item}'],
       ['pop'],
       ['push', '${1:item}'],
       ['reverse'],
       ['shift'],
       ['slice'],
       ['sort', '${1:fn}'],
       ['splice'],
       ['toString'],
       ['unshift'],
       ['valueOf']], "
      s
      "= [
     ['charAt', '${1:ix}'],
     ['charCodeAt', '${1:ix}'],
     ['concat', '${1:str2 ${2:, str3}}'],
     ['indexOf', '${1:str}'],
     ['lastIndexOf','${1:str}'],
     ['match','${1:re}'],
     ['replace','${1:re}, ${2:newString}'],
     ['search','${1:re}'],
     ['slice','${1:begin ${2:, optEnd}}'],
     ['split','${1:separator ${2:, optLimit}}'],
     ['substr','${1:start ${2:, optLength}}'],
     ['substring','${1:from ${2:, optionalTo}}'],
     ['length', null],
     ['toLowerCase'],
     ['toUpperCase']], " v
      "= {
        getType : function(x) {
          var t = typeof x;
          if (t === 'object') {
            if (x) {
              if (x instanceof Array) {
                t = 'array';
              }
            }
            else {
              t = 'null';
            }
          }
          return t;
        },
        argString : function(fn) {
            var re1 = new RegExp('[\\n\\t]','g'),
                fnStr = fn.toString().replace(re1,' '),
                re2 = new RegExp('\\\\(.+?\\\\)'),
                argStr = fnStr.match(re2),
                s = argStr.toString();
            return s.substring(1, s.length - 1);
        },
        emitPropListAsSexp: function(id) {
          var i, p=" v ".getPropList(id), L=p.length;
          WScript.StdOut.Write('(list ');
          for(i=0;i<L;i++){
            WScript.StdOut.Write('\\'(\"' + p[i].name + '\" \"' + p[i].type + '\" ');
            if (typeof p[i].args != 'undefined') {
              WScript.StdOut.Write('\"' + p[i].args + '\" ');
            }
            WScript.StdOut.WriteLine(')');
          }
          WScript.StdOut.WriteLine(')');
        },
        getPropList: function(identifier) {
            var r=[], m=identifier, t, n, e, typ = " v ".getType(m);
            if (typ == 'string') {
                e = " s ";
            }
            else if (typ == 'array') {
                e = " a ";
            }
            if (e !== null) {
                for(t = 0;t<e.length;t++) {
                    n = {name:e[t][0], type:'function'};
                    if (typeof e[t][1] != 'undefined') {
                      if (e[t][1] === null) {
                        n.type = 'number';
                      } else {
                        n.args = e[t][1];
                      }
                    }
                    r.push(n);
                }
            }
            else {
                for(var p in m){
                    t = typeof p;
                    if (isNaN(parseInt(p, 10))) {
                        t = 'index';
                    }
                    else {
                        t =  " v ".getType(m.p);
                    }
                    n = {name:p, type:t};
                    if (t=='function') { n.args= " v ".argString(m.p);}
                    r.push(n);
                }
            }
            return r;
        }
     };")))


(defun jscomp-type-inspector-boilerplate-2 (identifier)
  "Returns a string containing Javascript code that, when
invoked, returns the properties of the object in the JS Shell
referred to as IDENTIFIER.

"
  (concat "emacsJscompHelper_" (jscomp-random-id)
          ".emitPropListAsSexp(" identifier ");}());
    </script>
  </job>
</package>"))


(defun jscomp-within-p (loc b)
  "return t if b lies within location LOC."
  (let ((end (cdr (assq 'line (assq 'end loc))))
        (start (cdr (assq 'line (assq 'start loc)))))
    (and (>= b start) (<= b end))))

(defun jscomp-synthetic-expression (type-string)
  "Return a sythetic expression for the type described by the
given TYPE-STRING, which can be ThisExpression, ArrayExpression,
and so on.
"
  (cond
   ((string= type-string "ThisExpression")
    "this")
   ((string= type-string "ArrayExpression")
    "[]")
   (t
    (concat "'" type-string "'"))))

(defun jscomp-get-vars-and-types (elt location &optional args)
  "For the given parse tree element ELT, get a list of synthetic,
typed variable declarations for the given LOCATION.

If the element is a function, then args is a vector containing
the list of arguments.

"
  (let ((ty (cdr (assq 'type elt)))
        (loc (assq 'loc elt))
        (next nil)
        arg x next e
        (r ""))

    (cond
     ((string= ty "VariableDeclaration")
      (let ((vars (cdr (assq 'declarations elt))) ;; vector
            (i 0)
            name init)
        (while (< i (length vars))
          (setq x (elt vars i)
                i (1+ i)
                name (cdr (assq 'name (assq 'id x)))
                init (assq 'init x))
          (when init
            (setq r (concat r "\nvar " name "= "
                            (jscomp-synthetic-expression (cdr (assq 'type init)))
                             ";\n"))))
        r))

     (t
      (when (jscomp-within-p loc location)
        (cond
         ((or (string= ty "BlockStatement")
              (string= ty "Program"))

          (let ((body (cdr (assq 'body elt))) ;; vector of blocks or statements
                (i 0))
            (while (< i (length body))
              (setq e (jscomp-get-vars-and-types (elt body i) location)
                    i (1+ i))
              (when e (setq r (concat r e))))))

         ((string= ty "ExpressionStatement")
          (setq next (cdr (assq 'expression elt))))

         ((string= ty "CallExpression")
          (setq x (cdr (assq 'callee elt))
                next (assq 'body x)
                e (jscomp-get-vars-and-types x
                                             location
                                             (cdr (assq 'arguments elt))))
          (when e (setq r (concat r e))))

         ((string= ty "FunctionExpression")
          (let ((params (cdr (assq 'params elt)))
                (j 0))
            (while (< j (length params))
              (setq e (elt params j)
                    arg (elt args j)
                    j (1+ j)
                    r (concat r "var " (cdr (assq 'name e))
                              "= "
                            (jscomp-synthetic-expression (cdr (assq 'type arg)))
                              ";\n"))))))

        (when next
          (setq e (jscomp-get-vars-and-types next location))
          (when e (setq r (concat r e))))

        r)))))


(defun jscomp-figure-choices (parse-tree identifier begin end stub)
  "Return an s-expression indicating the available completion
choices given the PARSE-TREE, IDENTIFIER, BEGIN and END, and STUB.

The parse-tree is the output of esprima. Given the identifier and
the location, this logic looks into the parse tree and finds the
type of the thing that the user is doing completion on. This fn
then builds some js code that inspects the type of that thing,
and returns an s-expression representing that result.

"
  (let ((temp-wsf (jscomp-js-fname (format "jsci-%d" (random))  ".wsf"))
        r)

    (with-temp-file temp-wsf
      (insert (jscomp-type-inspector-boilerplate-1))
      ;; insert synthetic var decl with the same type
      (insert (jscomp-get-vars-and-types parse-tree (line-number-at-pos begin)))
      ;;(insert (concat "\n\nvar " identifier ";\n"))

      (insert (jscomp-type-inspector-boilerplate-2 identifier)))

    (setq r (shell-command-to-string
             (concat jscomp-cscript-exe " //Job:inspect " temp-wsf)))

    (delete-file temp-wsf)
    (eval (read r))))



(defun jscomp-choose-completion (identifier begin end stub)
  "Present possible completions.

IDENTIFIER is the var being completed.

BEGIN is the beginning of the thing being matched.

END is the end.

STUB is the partial, if any, of a property name to be matched.

"

  (let* ((parse-tree (jscomp-exec-parse
                      (jscomp-get-parsable-js begin end)))
         (choices (jscomp-figure-choices parse-tree identifier begin end stub))
         (choice
          (x-popup-menu (jscomp-get-menu-position)
                        (jscomp-generate-menu choices stub))))
    (when choice
      (when stub
        (backward-delete-char (length stub)))

      ;; Possibly do a ya-snippet expansion on the arglist
      ;; of the thing, if it is a function.
      (cond
       ((string= (nth 1 choice) "function")
        (insert (nth 0 choice))
        (let ((arglist (nth 2 choice)))
          (if (stringp arglist)
              (if (fboundp 'yas/expand-snippet)
                  (yas/expand-snippet
                   (concat  "("
                            (if (string-match ":" arglist)
                                arglist
                              (let ((c 0))
                                (mapconcat '(lambda (elt)
                                              (setq c (1+ c))
                                              (concat "${" (number-to-string c)
                                                      ":" elt "}"))
                                           (split-string (nth 2 choice) ", " t)
                                           ", ")))
                            ")"))
                (insert "()")
                (backward-char 1))
            (insert "()")
            (backward-char 1))))

       ((string= (nth 1 choice) "index")
        (backward-delete-char 1)
        (insert (concat "[" (nth 0 choice) "]")))

       (t
        (insert (nth 0 choice)))))))


(defun jscomp-ensure-prerequisites ()
  "Ensure Esprima and json2 are available."
  (mapc '(lambda
           (tag)
           (let ((url (eval (intern (format "jscomp-%s-src-url" tag))))
                 (fname (funcall (intern (format "jscomp-%s-script-location" tag)))))

             ;; ensure the script exists; if not, download it.
             (if (or (not (file-exists-p fname))
                     (not (file-readable-p fname)))
                 (jscomp-download-script url fname))

             (if (or (not (file-exists-p fname))
                     (not (file-readable-p fname)))
                 (error (concat "Trouble downloading the " tag " script.")))))

        '("esprima" "json2")))


(defun jscomp-exec-parse (str)
  "Execute a boilerplate parser javascript script, via
cscript.exe. The boilerplate script invokes the esprima parser
on another javascript source file, specified as the first
argument. The source file gets STR as its contents. The parser
emits the result to stdout.

"
  (jscomp-ensure-prerequisites)

  (let ((driver (jscomp-parser-driver-script-location))
        (temp-f (jscomp-js-fname (format "jscompparser-%d" (random))))
        r)

    (if (not (file-exists-p driver))
        (with-temp-file driver
          (insert (jscomp-parser-driver-wsf-boilerplate))))

    (if (file-exists-p temp-f) (delete-file temp-f))
    (with-temp-file temp-f (insert str))
    (setq r
          (shell-command-to-string
           (concat jscomp-cscript-exe
             " //Job:parse "
             driver
             " "
             temp-f)))

    (delete-file temp-f)
    (json-read-from-string r)))


(defun jscomp-complete ()
  "Do completion on the thing at point in the js-mode buffer.

The normal way to do these completions is to
present a dropdown.

You probably want to bind this fn to a keystroke in js-mode.

Returns t if successful.
"
  (interactive)
  (when (and (buffer-file-name)
             (save-excursion
               (beginning-of-line)
               (looking-at "[ \t]*\\([_A-Za-z][]\\[_0-9A-Za-z\\.()]*\\)\\.\\([_A-Za-z][_0-9A-Za-z]*\\)?$")))
    (let ((begin (match-beginning 0))
          (end (match-end 0))
          (identifier (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1)))
          (stub (and (match-beginning 2)
                     (buffer-substring-no-properties
                      (match-beginning 2)
                      (match-end 2)))))
      (jscomp-choose-completion identifier begin end stub))
    t))


(provide 'jscomp)

;;; jscomp.el ends here

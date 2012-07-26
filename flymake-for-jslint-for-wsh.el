;;; flymake-for-jslint-for-wsh.el --- use flymake with js code, on Windows
;;
;; Author     : Dino Chiesa
;; Version    : 1.3.0
;; Keywords   : javascript js jslint flymake languages
;; URL        : http://code.google.com/p/jslint-for-wsh/
;; X-URL      : http://code.google.com/p/jslint-for-wsh/
;; Last-saved : <2012-March-28 21:14:20>
;; Package-Requires: ((flymake "0.3"))
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2010-2011, Dino Chiesa
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
;; Started from
;; http://www.emacswiki.org/emacs-en/FlymakeJavaScript
;;
;; .. and modified for use on Windows.
;;
;; To use this module, you need something like this in your .emacs file:
;;
;;   (defun my-javascript-mode-fn ()
;;     (require 'flymake)
;;     (require 'flymake-for-jslint-for-wsh)
;;     (setq flyjs-jslintwsh-location "c:\\users\\me\\bin\\jslint-for-wsh.js")
;;     (flymake-mode-on)
;;      ...
;;      )
;;
;;   ;; need only one of the following, whichever is right for you
;;   (add-hook 'javascript-mode-hook 'my-javascript-mode-fn)
;;   (add-hook 'espresso-mode-hook   'my-javascript-mode-fn)
;;   (add-hook 'js2-mode-hook        'my-javascript-mode-fn)


;;
;; Bugs:
;;
;;    - Not integrated with the flymake-for-jslint.el which is used on
;;      Linux. Really there shouldn't be separate .el files for Windows
;;      and Linux.  This should be combined with the Linux-oriented
;;      flymake-for-jslint.js ; it will then be one module generalized
;;      to support both Linux and windows.
;;
;;

(require 'flymake)

(defcustom flyjs-try-use-chakra t
  "Whether to try to use the Chakra engine on Windows.

It is possible to use the \"Chakra\" engine from IE9, from a
cscript.exe command. The benefit is, Javascript programs run
faster.

When this variable is non-nil, flymake-for-jslint-for-wsh will
look for the Chakra Javascript engine, and will use it if it is
available. If this variable is nil, or if the Chakra engine is
not available, then flymake-for-jslint-for-wsh will use the
default JScript engine (not at version 5.8) when running the
jslint program.

In most cases you should just leave this variable as t.

To make it possible to use Chakra, you will need at least IE9
installed on your computer, and you will need to modify your
registry to expose the Chakra engine to cscript.exe.

To do the registry mod, save the following into a .reg file, and
run it:

    Windows Registry Editor Version 5.00

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\CLSID\\{16d51579-a30b-4c8b-a276-0ff4dc41e755}\\ProgID]
    @=\"Chakra\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Wow6432Node\\CLSID\\{16d51579-a30b-4c8b-a276-0ff4dc41e755}\\ProgID]
    @=\"Chakra\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra]
    @=\"JScript Language\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\CLSID]
    @=\"{16d51579-a30b-4c8b-a276-0ff4dc41e755}\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\OLEScript]

")

(defvar flyjs--language-name nil
  "This is the name of the language that will be requested on the
cscript.exe command line when running the jslint program.

flymake-for-javascript-for-wsh tries to infer the name by
examining the registry, the first time you use
flymake-for-javascript-for-wsh.  It selects Chakra if it is
available and selects Jscript otherwise.

You can force this module to always use JScript by setting
`flyjs-try-use-chakra' to nil, before calling require on this
module.  You can also override what the module selects, by
setting this variable manually in your .emacs.  Take care to set
it to only JScript, Javascript, or a legal Javascript-compatible
language string that can be understood by cscript.exe's //E option. ")



(defun flyjs--reg-read (regpath)
  "read a path in the Windows registry"
  (let ((reg.exe (concat (getenv "windir") "\\system32\\reg.exe"))
        tokens last-token)

    (setq reg-value (shell-command-to-string
                     (concat reg.exe " query " regpath))
          tokens (split-string reg-value nil t)
          last-token (nth (1- (length tokens)) tokens))

    (and (not (string= last-token "value.")) last-token)))



;; (defun flyjs--reg-read (regpath)
;;   "read a path in the Windows registry"
;;   (let ((temp-f (make-temp-file "regread_" nil ".js"))
;;         (js-code "var WSHShell, value, regpath = '';try{ if (WScript.Arguments.length > 0){ regpath = WScript.Arguments(0); WSHShell = WScript.CreateObject('WScript.Shell'); value = WSHShell.RegRead(regpath); WScript.Echo(value); }}catch (e1){ WScript.Echo('error reading registry: ' + e1);}")
;;         reg-value)
;;
;;   (with-temp-file temp-f
;;     (insert js-code))
;;
;;   (setq reg-value
;;         (shell-command-to-string
;;          (concat temp-f " " regpath)))
;;
;;   (delete-file temp-f)
;;   reg-value ))


(defcustom flyjs-jslintwsh-location "\\bin\\jslint-for-wsh.js"
  "Location of the jslint.js script.

The jslint script specified here must be modified from the
standard jslint available from jslint.com, in order to accept
command-line arguments, and to run within WSH.

The latest official version of jslint-for-wsh.js can be obtained from
http://code.google.com/p/jslint-for-wsh/

You can also use jshint, which can be configured to be much less
strict than jslint. It also must be modified to accept
command-line arguments

If you'd like to perform the modifications yourself, follow these
instructions.  Download jslint.js or jshint.js .  Append to the
end of the file, this stanza:

    (function () {
        'use strict';
        var filename = 'stdin', content = '', fso, fs, i, e, line, linter, label,
            options = {
                wsh        : true,  // WScript is allowed (JSHINT)
                windows    : true,  // WScript is allowed (JSLINT)
                white      : true,  // true: 'sloppy' whitespace is ok
                plusplus   : true,  // true == ok to use ++
                properties : false, // do not barf on any undeclared properties
                passfail   : false, // do not stop after first error
                radix      : true   // do not puke on parseInt() with no radix
            };

        if (WScript.Arguments.length > 0) {
            filename = WScript.Arguments(0);
            fso = new ActiveXObject('Scripting.FileSystemObject');
            //var file = fso.GetFile(filename);
            fs = fso.OpenTextFile(filename, 1);
            content = fs.ReadAll();
            fs.Close();
            fso = null;
            fs = null;
        } else {
            content = WScript.StdIn.ReadAll();
        }

        if (typeof JSHINT === 'function') {
            linter = JSHINT;
            label = 'JSHINT';
        }
        else if (typeof JSLINT === 'function') {
            linter = JSLINT;
            label = 'JSLINT';
        }
        else {
            throw 'no lint available.';
        }

        if (!linter(content, options)) {
            WScript.StdErr.WriteLine(label);
            for (i = 0; i < linter.errors.length; i++) {
                // sample error msg:
                //  sprintf.js(53,42) JSLINT: Use the array literal notation [].
                e = linter.errors[i];
                if (e !== null) {
                    line = (typeof e.line === 'undefined') ? '0' : e.line;
                    WScript.StdErr.WriteLine(filename + '(' + line + ',' + e.character +
                                             ') ' + label + ': ' + e.reason);
                    WScript.StdErr.WriteLine('    ' + (e.evidence || '').replace(/^\\s*(\\S*(\\s+\\S+)*)\\s*$/, '$1'));
                }
            }
        }
    }());

This change does two things:

1. allows you to specify the file to lint-check, on the command
   line, rather than as stdin. Stdin still works if no file is
   specified at all.

2. emits the error messages in a format that is more similar to
   most C/C++ compilers.

The first change allows you to invoke jslint.js from the command
line, or from within emacs with M-x compile. The second change
allows you to interpet error messages with M-x next-error.
"
  :type 'string :group 'flyjs
  )

;; consider several options for the .js extension  on the flymake thing
(defvar flyjs--js-keys '("\\.js\\'"  "\\.js\\$" ".+\\.js\\'" ".+\\.js\\$"))


(defun flyjs-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (file-without-path (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name)))
         (cscript-exe (concat (getenv "windir")
                              "\\system32\\cscript.exe"))
         (jslint-loc (expand-file-name flyjs-jslintwsh-location)))
    (if (not (file-exists-p jslint-loc))
        (error "Please set flyjs-jslintwsh-location to an actual location.")
      (list cscript-exe (list jslint-loc file-without-path
                              (concat "//E:" flyjs--language-name))))))



(defun flyjs-install ()
  "installs flymake-for-jslint-for-wsh logic into flymake. This
fn does several things:

  - select the Javascript engine to use with cscript.exe. (See
    `flyjs-try-use-chakra')

  - modify `flymake-err-line-patterns' to include a pattern for
    JSLINT or JSHINT

  - modify `flymake-allowed-file-name-masks' to make sure the
    jslint-for-wsh command is invoked by flymake.

"
  (setq flyjs--language-name
        (if (and flyjs-try-use-chakra
            (string=
             (upcase
              (flyjs--reg-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\CLSID\\"))
             "{16D51579-A30B-4C8B-A276-0FF4DC41E755}"))
            "Chakra" "JScript")

        flymake-err-line-patterns
        (cons '("^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\| JSHINT\\): \\(.+\\)$"
                1 2 3 5)
              flymake-err-line-patterns))

  ;; look for various keys for javascript files
  (let ((keys flyjs--js-keys)
        key
        found
        jsentry)
    (while (and keys (not found))
      (setq key (car keys)
            jsentry (assoc key flymake-allowed-file-name-masks))
      (if jsentry
          (progn
            (setcdr jsentry '(flyjs-init
                              flymake-simple-cleanup
                              flymake-get-real-file-name))
            (setq found t)))
      (setq keys (cdr keys)))
    (if (not found)
        (add-to-list
         'flymake-allowed-file-name-masks
         (list (car flyjs--js-keys)
               'flyjs-init 'flymake-simple-cleanup 'flymake-get-real-file-name)))))

(flyjs-install)

(provide 'flymake-for-jslint-for-wsh)

;;; flymake-for-jslint-for-wsh.el ends here


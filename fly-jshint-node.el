;;; fly-jshint-node.el --- use flymake with jshint on js code, using Node.js
;;
;; Author     : Dino Chiesa
;; Version    : 2012.8.1
;; Keywords   : javascript js jslint jshint flymake languages jscript
;; URL        :
;; License    : New BSD
;; Last-saved : <2013-April-16 23:25:02>
;; Package-Requires: ((flymake "0.3"))
;;

;;; Commentary:

;; This module performs flymake on-the-fly syntax checking of Javascript
;; source files, using JSLINT or JSHINT, via Node.js

;; This module started from
;; http://www.emacswiki.org/emacs-en/FlymakeJavaScript
;; and got modifications for my purposes.

;; Usage: Add something like this in your .emacs file:
;;
;;   (defun my-javascript-mode-fn ()
;;     (require 'fly-jshint-wsh)
;;     (flymake-mode 1)
;;      ...
;;      )
;;
;; By default, this module uses JSHINT. If you prefer jslint, then
;;
;;   (defun my-javascript-mode-fn ()
;;     (require 'fly-jshint-node)
;;     (setq flyjs-checker 'jslint) ;; jshint is the default
;;     (flymake-mode 1)
;;      ...
;;      )

;; You then need to put your custom hook onto the mode hook list.
;; You need only one of the following, whichever is right for you.

;;   (add-hook 'javascript-mode-hook 'my-javascript-mode-fn)
;;   (add-hook 'espresso-mode-hook   'my-javascript-mode-fn)
;;   (add-hook 'js2-mode-hook        'my-javascript-mode-fn)
;;   (add-hook 'js-mode-hook         'my-javascript-mode-fn)

;; There are multiple versions of the flymake for JSLINT/JSHINT
;; capability for windows. This is the latest, as of Thu, 29 Mar 2012.
;; They all are based on the idea of running JSHINT or JSLINT from a
;; command line, via CScript.exe.  They all can use either JSHINT or
;; JSLINT, but in either case need some boilerplate logic appended to
;; the standard JSLINT or JSHINT distribution, in order to function
;; properly.

;; In prior versions, there was a requirement for the user to manually
;; download JSLINT or JSHINT, then perform the modifications and set the
;; location of the modified verion of the script.  This version of the
;; module has been updated to do that for you. It uses Powershell - a
;; feature of Windows since Windows XPSP2 - to download the bare JSLINT
;; or JSHINT script, then it appends the necessary boilerplate, and
;; saves the resulting script in the user's temporary directory. It then
;; runs the script from there to do syntax checks. This simplifies
;; installation, but it does require an internet connection, the first
;; time through.

;; Pre-requisites to use this module include: Node.js.
;; Also, on the
;; first run only, curl is
;; required, in order to download the JSLINT or JSHINT script.
;; xxxx

;; With this feature, the first time you open a .js file, emacs will
;; download the necessary script. This can take several seconds, so you
;; will see a delay in opening the buffer. This is a one-time delay.
;;

;;; Revisions:
;;
;; 2012.8.1  2012-August-01 Dino Chiesa  INITIAL RELEASE
;;
;;    Put a timestamp on the downloaded-and-modified jslint or jshint
;;    scripts.  This makes it possible to periodically check for updates
;;    of the jshint/jslint script.
;;


;;; Copyright
;;
;; Copyright (c) 2010-2012, Dino Chiesa
;; All rights reserved.
;;

;;; License
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


;;; Bugs:
;;
;;  - Not integrated with the flymake-for-jslint.el which is used on
;;    Linux. Really there shouldn't be separate .el files for Windows
;;    and Linux.  This should be combined with the Linux-oriented
;;    flymake-for-jslint.js ; it will then be one module generalized
;;    to support both Linux and windows.
;;
;;  - Does not check for updates of JSLINT or JSHINT. It should, and then
;;    should download only as necessary, or on demand.
;;


(require 'flymake)

;;(setq flymake-log-level 3)

(defgroup flyjs nil
  "Run flymake on Javascript code, with JSLINT or JSHINT, via Node.js")

(defcustom flyjs-checker 'jshint
  "The syntax checker to use for Javascript. Legal values are
'jslint and 'jshint.  Use this in your emacs:

   (require 'fly-jshint-node)
   (setq flyjs-checker 'jslint)

Or you can customize this variable.
"
  :group 'flyjs)

;; https://raw.github.com/jshint/jshint/master/jshint.js
(defvar flyjs-jshint-src "https://raw.github.com/jshint/jshint/master/src/stable/jshint.js"
  "Source URL for JSHint")

(defvar flyjs-jsBeautify "~/dev/js/jsBeautify.js"
  "source location for node-compatible jsBeautify.js")

(defvar flyjs-jslint-src "https://raw.github.com/douglascrockford/JSLint/master/jslint.js"
  "Source URL for JSLint")

(defvar flyjs-script-location nil
  "Filename for the jslint.js or jshint.js script. The file gets
downloaded as necessary when `fly-jshint-node.el' runs. The value
of this variable is set dynamically - this module looks in the temp
directory for the latest file named like \"emacs.flyjs.js[lh]int.DATE.js\".
")

(defvar flyjs-curl-exe "curl"
  "the path of the curl program, for retrieving jslint or jshint.")

(defvar flyjs-node-exe "/usr/local/bin/node"
  "the path of the node program, for running js scripts from the shell.")

;; consider several options for the .js extension  on the flymake list
(defvar flyjs--js-keys '("\\.js\\'"  "\\.js\\$" ".+\\.js\\'" ".+\\.js\\$"))


(defvar flyjs-node-boilerplate "var label, linter;if (typeof exports.JSHINT === 'function') {linter = exports.JSHINT;label = 'JSHINT';}else if (typeof exports.JSLINT === 'function') {linter = exports.JSLINT;label = 'JSLINT';}else {throw 'no lint tool found.';}var fs = require('fs'),path = require('path'),files = [],options = {plusplus:false, quotmark:false, onevar:true, forin:true,white:false, undef:true},perfectFiles = [];var walkFiles = function walkFiles(file, fileHandler){var wf = function walkFiles(file){fs.stat(file,function(error,stats){if(error) throw error;if(stats.isDirectory()){var dir=file;fs.readdir(dir,function(err,files){files.forEach(function(file){wf(path.join(dir,file));});});return;}if(stats.isFile()){fileHandler(file);}});};wf(file);},lintOneFile = function(fname) {if(path.extname(fname) !== '.js'){return;}fs.readFile(fname,'utf-8',function(error2,filetext){var errors, out;if(error2){console.log('Error: ' + error2);return;}if (linter(filetext.toString(),options)){perfectFiles.push(fname);console.log('0 errors');}else {out = linter.data();errors = out.errors;Object.keys(errors).forEach(function(key){var e = errors[key], line;if (e !== null) {line = (typeof e.line === 'undefined') ? '0' : e.line;console.log(fname + '(' + line + ',' + e.character + ') ' + label + ': ' + e.reason);if (typeof e.evidence != 'undefined') {console.log('    ' + (e.evidence || '').replace(/^\\s*(\\S*(\\s+\\S+)*)\\s*$/, '$1'));}}});console.log(errors.length + ((errors.length == 1) ? ' error' : ' errors'));}});};(function(args){args.forEach(function(option){if(option.match(/^--/)){var selected = option.match(/^--(no-)?([\d\w\-]+)$/);if(selected){options[selected[2]] = (selected[1] === 'no-') ? false : true;}}else{files.push(option);}});})(process.argv.slice(2));console.log(label + ' global options: ' + require('util').inspect(options));files.forEach(function(file){walkFiles(file,lintOneFile);});process.on('exit',function() { });")




(defun flyjs-get-matching-files-by-time (spec)
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

(defun flyjs-script-location (&optional force-new)
  "gets the location of the checker script."
  (if (and (not (eq flyjs-checker 'jslint))
           (not (eq flyjs-checker 'jshint)))
      (setq flyjs-checker 'jshint))

  (flet ((js-fname (x)
                   (concat
                    (file-name-as-directory temporary-file-directory)
                    "emacs.flyjs."
                    (symbol-name flyjs-checker)
                    "." x ".js")))
    (let (flist)
      (setq flyjs-script-location
            (if (or force-new
                    (not (setq flist (flyjs-get-matching-files-by-time
                                      (js-fname "*")))))
                (js-fname (format-time-string "%Y%b%d"))
              (nth 0 flist)))))

  (if (and force-new
      (file-exists-p flyjs-script-location))
      (delete-file flyjs-script-location))

  flyjs-script-location)


(defun flyjs-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

This won't always work; it will fail if the source module
refers to relative paths.
"
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                prefix "-"
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "-"))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


(defun flyjs-choose-script-url ()
  "Choose which URL to download from, to get the script.
User may set the preference in `flyjs-checker'. Legal values
are 'jshint and 'jslint . If `flyjs-checker' is a non-standard
value, it defaults to 'jshint.
"
  (cond
   ((eq flyjs-checker 'jslint)
    flyjs-jslint-src)
   ((eq flyjs-checker 'jshint)
    flyjs-jshint-src)
   (t
    (setq flyjs-checker 'jshint)
    flyjs-jshint-src)))


(defun flyjs-wget-via-curl (url target-f)
  "Get the contents of a URL into a file named TARGET-F via
curl.

The reason we don't use the `url.el' package is because it can be
flaky when using SSL/TLS, and both jshint and jslint are
available in raw form on github which requires https. Rather than
figure out the problem with `url.el', I'm just going to work
around the problem by using curl which is known good.

"
    (with-temp-file target-f
        (call-process flyjs-curl-exe nil t nil "-s" url)))


(defun flyjs-download-script ()
  "Download the jslint or jshint script from the intertubes,
and then modify it to insert the WSH integration glue.
"
  (let ((loc (flyjs-script-location t))
        (url (flyjs-choose-script-url) )
        (bplt-loc (make-temp-file "node-boilerplate-" nil ".js")))

    (let ((bplt
           (if (file-exists-p flyjs-jsBeautify)
               (progn
                 (with-temp-buffer
                   (insert "\n//-- BEGIN nodejs boilerplate -- \n")
                   (insert flyjs-node-boilerplate)
                   (insert "\n//-- END nodejs boilerplate --\n")
                   (write-region (point-min) (point-max) bplt-loc))
                 (shell-command-to-string (concat flyjs-node-exe " " flyjs-jsBeautify " " bplt-loc)))
             flyjs-node-boilerplate)))

      (delete-file bplt-loc)

      (message (format "downloading %s to %s" url loc))
      (flyjs-wget-via-curl url loc)

      (with-temp-buffer
        (insert-file-contents loc)
        (goto-char (point-max))
        ;; augment with node boilerplate
        (insert "\n")
        (insert bplt)
        (insert "\n")
        (write-region (point-min) (point-max) loc)))))



(defun flyjshint-init ()
  "Called each time a flymake check occurs."
  (let ((work-file (flymake-init-create-temp-buffer-copy
                    'flyjs-create-temp-intemp)))
    ;; download the script file as necessary
    (if (or (not (file-exists-p (flyjs-script-location)))
            (not (file-readable-p flyjs-script-location)))
        (flyjs-download-script))

    (if (or (not (file-exists-p flyjs-script-location))
            (not (file-readable-p flyjs-script-location)))
        (error "Trouble downloading the jslint or jshint script.")
      (list flyjs-node-exe (list
                            flyjs-script-location
                            work-file)))))


(defun flyjs-install (&optional force-download)
  "installs fly-jshint-wsh logic into flymake. This needs to be done
just once, per instance of emacs. It is done automatically when
your .emacs file includes this statement:

    (require 'fly-jshint-node)

This fn does several things:

  - If FORCE-DOWNLOAD is non-nil, download jshint.js or
    jslint.js, and combine it with some JS boilerplate (command-line
    handling) and save it into script cache location.

  - modify `flymake-err-line-patterns' to include a pattern for
    JSLINT or JSHINT

  - modify `flymake-allowed-file-name-masks' to make sure the
    jslint-for-node command is invoked by flymake.

If the script does not exist (either jshint or jslint) in the
well-known place this module expects it, and FORCE-DOWNLOAD is
nil, then this module will download the script at the time
flymake first runs. As a result, there may be a delay when
opening the JS file.

If you want to avoid that delay, you can invoke this method
explicitly, at some point after the require statement, like so:

    (require 'fly-jshint-node)
    (flyjs-install t)

You can precede it with a setq to explicitly specify which
checker script to use, like so:

    (require 'fly-jshint-node)
       ....
    (setq flyjs-checker 'jslint) ;; jshint is the default
    (flyjs-install t) ;; force download of jslint
    (flymake-mode 1)

I recomment you do not download the jslint or jshint
script every time you start emacs. This module will re-use a
previous download of the script, if it is available. This is
good, because it saves time. It can be bad, because you
won't get the latest checker script when updates are made
available on github. It is likely good enough, though.

This is the typical way people will use `fly-jshint-node.el'.

    (require 'fly-jshint-node)
    (flymake-mode 1)

"
  (if force-download (flyjs-download-script))
  (setq
        flymake-err-line-patterns
        (cons '("^[ \t]*\\([-A-Za-z.0-9_:/ ]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\| JSHINT\\): \\(.+\\)$"
                1 2 3 5)
              flymake-err-line-patterns))

  ;; fixup the various keys for javascript files in the flymake alists
  (let ((keys flyjs--js-keys)
        key
        found
        jsentry)
    (while (and keys (not found))
      (setq key (car keys)
            jsentry (assoc key flymake-allowed-file-name-masks))
      (if jsentry
          (progn
            (setcdr jsentry '(flyjshint-init
                              flymake-simple-cleanup
                              flymake-get-real-file-name))
            (setq found t)))
      (setq keys (cdr keys)))
    (if (not found)
        (add-to-list
         'flymake-allowed-file-name-masks
         (list (car flyjs--js-keys)
               'flyjshint-init 'flymake-simple-cleanup ;;'flymake-get-real-file-name
               )))))

(flyjs-install)

(provide 'fly-jshint-node)

;;; fly-jshint-node.el ends here

;;; b-bundle.el --- generated bundle of b.el and jshint-boilerplate.js
;;
;; generated Thu Mar 29 15:05:14 2012
;;

;;; b.el --- voof!

;;; b.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the embedded source code

(setq download-file-src "(function () {'use strict';var filename='stdin', content='', fso, fs, i, e, line, linter, label,\noptions={curly:false,\nwsh:true,\nwhite:false,\nplusplus:false,\npassfail:false};if (WScript.Arguments.length > 0) {filename=WScript.Arguments(0);fso=new ActiveXObject('Scripting.FileSystemObject');fs=fso.OpenTextFile(filename, 1);content=fs.ReadAll();fs.Close();fso=null;fs=null;} else {content=WScript.StdIn.ReadAll();}if (typeof JSHINT === 'function') {linter=JSHINT;label='JSHINT';}else if (typeof JSLINT === 'function') {linter=JSLINT;label='JSLINT';}else {throw 'no lint tool found.';}if (!linter(content, options)) {WScript.StdErr.WriteLine(label);for (i=0; i < linter.errors.length; i++) {e=linter.errors[i];if (e !== null) {line=(typeof e.line === 'undefined') ? '0':e.line;WScript.StdErr.WriteLine(filename + '(' + line + ',' + e.character +\n') ' + label + ': ' + e.reason);WScript.StdErr.WriteLine('    ' + (e.evidence || '').replace(/^\\s*(\\S*(\\s+\\S+)*)\\s*$/, '$1'));}}}}());")

(provide 'b-bundle)

;;; b-bundle.el ends here

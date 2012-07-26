;;; ps-tricks.el --- powershell tricks in elisp
;;
;; Author: Dino Chiesa
;; Created: Wed, 11 Apr 2012  20:44
;; Package-Requires: (Windows)
;; URL: ?
;; Version: 2012.4.11
;; Keywords: w32 powershell
;; License: New BSD

;;; Commentary:

;; This module provides functions that interface between emacs and
;; powershell to do various things.

;;; Revisions:

;; 2012.4.11  2012-Apr-11  Dino Chiesa
;;    initial version.


;;; License
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2012, Dino Chiesa
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

(defvar pst-datepicker-src
  "# pickDate.ps1
#
# pick a date, print the choice.
#
# Wed, 11 Apr 2012  20:38
#
 [void] [System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms')
 [void] [System.Reflection.Assembly]::LoadWithPartialName('System.Drawing')

$c = New-Object System.Windows.Forms.MonthCalendar
$c.ShowTodayCircle = $False
$c.MaxSelectionCount = 1

$f = New-Object Windows.Forms.Form
$f.Text = 'Select a Date'
$f.Size = New-Object Drawing.Size(190,190)
$f.StartPosition = 'CenterScreen'
$f.KeyPreview = $True
$f.Controls.Add($c)
$f.Topmost = $True
$f.Add_KeyDown({
  if ($_.KeyCode -eq 'Enter') { $d = $c.SelectionStart; $f.Close(); }
  if ($_.KeyCode -eq 'Escape') { $f.Close(); }
})
$f.Add_Shown({$f.Activate()})
 [void] $f.ShowDialog()

if ($d) { Write-Host $d.ToString('yyyy/MM/dd'); }
"
)


(when (not (fboundp 'string/trim-trailing-newlines))
  (defun string/trim-trailing-newlines (string)
    (while (string-match "\\(.*\\)\\(\n\\|\r\\)$" string)
        (setq string (substring string 0 -1))) ;; remove newline
      string))


;; (defun pst-datepicker-script-location ()
;;   "return the location of the datepicker script.
;; It may or may not exist."
;;   (concat
;;    (file-name-as-directory temporary-file-directory)
;;    "emacs.datepicker."
;;     (format-time-string "%Y%b%d")
;;    ".ps1"))


(defun pst-minimize-ps-contents (script)
  "returns the minimized PS version of the contents of
the provided string SCRIPT"
  (let ((re-pairs '(("[\s\t]*#.*$" "") ;; eliminate comments
                    ("\n\n" "\n")      ;; collapse newlines
                    ("\n}" "}")
                    ("{\n" "{")
                    ("\n[\s\t]+" ";")  ;; collapse whitespace
                    (";\n" ";")        ;; replace semi-newline with semi
                    ("^\n" "")         ;; eliminate leading newline
                    ("\n$" "")         ;; eliminate trailing newline
                    ("\n" ";")         ;; replace newline with semi
                    (";;+" ";")        ;; reduce multi semi
                    ("^;" ""))))       ;; eliminate leading semi
    (with-temp-buffer
      (insert script)
      (mapc (lambda (pair)
              (goto-char (point-min))
              (while (re-search-forward (car pair) nil t)
                (replace-match (cadr pair) nil nil)))
            re-pairs)
      (buffer-substring-no-properties (point-min) (point-max)))))


;; (defun pst-fill-datepicker-script ()
;;   "fill the datepicker script."
;;   (let ((filename (pst-datepicker-script-location)))
;;     (when (not (file-exists-p filename))
;;       (with-temp-file filename
;;         (insert pst-datepicker-src)))
;;     filename))


(defun pst-path-of-powershell-exe ()
  "get location of powershell exe."
  (concat
   (or (getenv "windir") "c:\\windows")
   "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))


;; (defun pst-pick-date-fail ()
;;   "Pick a date. This doesn't work because powershell
;; scripts are not approved for execution by default."
;;   (let ((cmd (format "%s %s"
;;                     (pst-path-of-powershell-exe)
;;                     (pst-fill-datepicker-script))))
;;   (shell-command-to-string cmd)))


(defun pst-pick-date ()
  "Pick a date."
  (let ((cmd (format "%s -Command %s"
                    (pst-path-of-powershell-exe)
                    (concat "\"& {"
                            (pst-minimize-ps-contents pst-datepicker-src)
                            "}\""))))
    (string/trim-trailing-newlines (shell-command-to-string cmd))))

(defun pst-run-script (ps-src)
  "Pick a date."
  (let ((cmd (format "%s -Command %s"
                    (pst-path-of-powershell-exe)
                    (concat "\"& {"
                            (pst-minimize-ps-contents ps-src)
                            "}\""))))
    (string/trim-trailing-newlines (shell-command-to-string cmd))))


(provide 'ps-tricks)

;;; ps-tricks.el ends here
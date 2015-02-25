;;; dpreso.el --- make an HTML5 preso out of a text file.
;;
;; Copyright (C) 2014 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : June 2014
;; Version    : 0.8
;; Keywords   :
;; Requires   :
;; License    : New BSD
;; X-URL      : https://github.com/DinoChiesa/dpchiesa-elisp/blob/master/dpreso.el
;; Last-saved : <2015-February-13 19:12:27>
;;
;;; Commentary:
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

(defgroup dpreso nil
  "Tool for producing HTML5 presentations with text.")

(defcustom dpreso-filesystem-dir "/Users/dino/dev/html/dpreso/"
  "The filesystem directory in which to generate HTML files that hold the
presentations. Should end in a slash."
  :group 'dpreso)

(defcustom dpreso-url-base "http://localhost:80/html/dpreso/"
  "The base URL at which generated presentations will be available.
Should end in a slash"
  :group 'dpreso)

(defconst dpreso-re-topic "^\\([A-Za-z0-9]\\).+$")
(defconst dpreso-re-lineitem "^\\* \\([A-Za-z0-9]\\).+$")
(defconst dpreso-re-subitem "^\\*\\* \\([A-Za-z0-9]\\).+$")

(defconst dpreso-template-alist
  (list
   '("page-head"
     "<!DOCTYPE html>
<html>
  <head>
    <title>@@TITLE@@</title>
    <link rel='stylesheet' type='text/css' media='screen'
          href='http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css'>
    <style type='text/css'>
     body { margin: 0 24px; }
     li { font-size: 18px; }
     div.panel { font-size: 24px; }
    </style>
  </head>

  <!-- ======================================================= -->

  <body>

    <div id='MainMenu'>
")

   '("page-foot"
     "    </div>
   <script type='text/javascript'
           src='//ajax.googleapis.com/ajax/libs/jquery/2.0.2/jquery.min.js'></script>

   <script type='text/javascript' src='//netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min.js'></script>

  </body>

</html>
")
   '("topic-head"
     "<div class='list-group panel'>
        <a href='#topic@@TID@@' class='list-group-item list-group-item-success strong'
           data-toggle='collapse'
           data-parent='#MainMenu'>@@TOPIC@@<i class='fa fa-caret-down'></i>
        </a>
    <div class='collapse' id='topic@@TID@@'>
    <h1>@@TOPIC@@</h1>
    <ul>
")
   '("topic-foot"
     "</ul></div></div>
")

))



(defun dpreso-make-presentation ()
  "Make an HTML5 \"presentation\" that uses bootstrap and jquery accordion
from the text file in the current buffer. The text should be constructed this
way:

  Slide Title 1
  * bullet point 1
  * bullet point 2
  * bullet point 3

  Title of Slide #2
  * Can include hyperlinks <a href='http://example.com'>Thing here</a>
  * Can include sub-bullets too
  ** Prepend two asterisks to do that
  ** There must be a space after the asterisk(s)

  Rules
  * You can have as many slides as you want
  * and as many bullets per slide
  * But currently, only 2-level lists are supported.

"
  (interactive)
  (let (prompt
        bufname
        (suggested-bufname (concat (buffer-name) ".htm")))

    (setq suggested-bufname
          (replace-regexp-in-string "\\.txt" "" suggested-bufname t t))

    (setq prompt (format "buffer name (%s) ? " suggested-bufname))
    (setq bufname (read-from-minibuffer prompt nil nil nil nil nil))

    (if (string= bufname "")
        (setq bufname suggested-bufname))

    (let ((ix 0)
          (buf (generate-new-buffer bufname))
          ;;(buf (find-file-noselect "oof.htm"))
          )
      (save-excursion
        (widen)
        (goto-char (point-min))
        (with-current-buffer buf
          (let ((p-head (cadr (assoc "page-head" dpreso-template-alist))))
            (setq p-head
                  (replace-regexp-in-string "@@TITLE@@" bufname p-head t t))
            (insert p-head)))

        (while (re-search-forward dpreso-re-topic nil t)
          (let ((topic (match-string 0 nil))
                (t-head (cadr (assoc "topic-head" dpreso-template-alist))))
            (setq t-head
                  (replace-regexp-in-string "@@TOPIC@@" topic t-head t t))
            (setq t-head
                  (replace-regexp-in-string "@@TID@@" (number-to-string ix) t-head t t))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert t-head)))

          (forward-line)
          ;;(move-end-of-line 1)
          (move-beginning-of-line 1)

          (let ((in-sublist nil))
            (while (or
                    (looking-at dpreso-re-lineitem)
                    (looking-at dpreso-re-subitem))

              (let ((item (match-string 0))
                    is-sublist)
                (with-current-buffer buf
                  (goto-char (point-max))
                  (setq is-sublist (string-prefix-p "**" item))
                  (if (and is-sublist  (not in-sublist))
                      (insert "    <ul>\n"))
                  (if (and in-sublist  (not is-sublist))
                      (insert "\n    </ul>\n"))

                  (setq in-sublist is-sublist)
                  (insert (concat "    <li>" (substring item 2) "</li>\n")))
                (forward-line)
                ;;(move-end-of-line 1)
                ;;(setq search-limit (point))
                (move-beginning-of-line 1))))

          (setq ix (1+ ix))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert (cadr (assoc "topic-foot" dpreso-template-alist))))))

      (let ((new-fname
             (concat dpreso-filesystem-dir (buffer-name buf))))

        (with-current-buffer buf
          (goto-char (point-max))
          (insert (cadr (assoc "page-foot" dpreso-template-alist)))
          (set-visited-file-name new-fname t t)
          (save-buffer))

        (call-process "open" nil t t
                      (concat dpreso-url-base (buffer-name buf)))))))


(provide 'dpreso)

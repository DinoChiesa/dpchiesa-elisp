;;; dpreso.el --- make an HTML5 preso out of a text file.
;;
;; Copyright (C) 2014 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : June 2014
;; Version    : 1.2
;; Keywords   :
;; Requires   : s.el
;; License    : New BSD
;; X-URL      : https://github.com/dpchiesa/elisp
;; Last-saved : <2014-June-23 22:04:16>
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

;;(require 's) ;; magnars' long lost string library


(defconst dpreso-re-topic "^\\([A-Za-z0-9]\\).+$")
(defconst dpreso-re-lineitem "^\\* \\([A-Za-z0-9]\\).+$")
(defconst dpreso-re-subitem "^\\*\\* \\([A-Za-z0-9]\\).+$")

(defconst dpreso-template-alist
  (list
   '("page-head"
     "<!DOCTYPE html>
<html>
  <head>
    <title>Three Things</title>
    <link rel='stylesheet' type='text/css' media='screen'
          href='http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css'>
    <style type='text/css'>
     body {
       margin: 0 24px;
     }
     li {
       font-size: 18px;
     }
     div.panel {
       font-size: 24px;
     }
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
  "Make an HTML5 preso that uses bootstrap and jquery accordion from
the text file in the current buffer. "
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
          (insert (cadr (assoc "page-head" dpreso-template-alist))))

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
             (concat "/Users/dino/dev/html/dpreso/"
                     (buffer-name buf))))

        (with-current-buffer buf
          (goto-char (point-max))
          (insert (cadr (assoc "page-foot" dpreso-template-alist)))
          (set-visited-file-name new-fname t t)
          (save-buffer))

        (call-process "open" nil t t
                      (concat "http://localhost:8080/html/dpreso/"
                              (buffer-name buf)))))))



(provide 'dpreso)

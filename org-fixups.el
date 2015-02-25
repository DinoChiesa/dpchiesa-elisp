;;; org-fixups.el --- fixups for org mode and its brethren
;;
;; Copyright (C) 2013,2014 Dino Chiesa
;;
;; Author     : Dino Chiesa
;;
;;; Commentary:
;;
;;; Code:
;;

(require 'org)

(if (fboundp 'org-reveal-template)
    ;; re-define this function to allow omitting the title slide,
    ;; if :reveal-title-slide-template is nil.
    (defun org-reveal-template (contents info)
      "Return complete document string after HTML conversion.
contents is the transcoded contents string.
info is a plist holding export options."
      (concat
       (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html>\n<html%s>\n<head>\n"
               (if-format " lang=\"%s\"" (plist-get info :language)))
       "<meta charset=\"utf-8\"/>\n"
       (if-format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
       (if-format "<meta name=\"author\" content=\"%s\"/>\n" (plist-get info :author))
       (if-format "<meta name=\"description\" content=\"%s\"/>\n" (plist-get info :description))
       (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (plist-get info :keywords))
       (org-reveal-stylesheets info)
       (org-reveal-mathjax-scripts info)
       (org-reveal--build-pre/postamble 'head-preamble info)
       "</head>\n<body>\n"
       (org-reveal--build-pre/postamble 'preamble info)
       "  <div class=\"reveal\">\n    <div class=\"slides\">\n"
       (let ((templ (plist-get info :reveal-title-slide-template)))
         (if (and templ (not (string= templ "nil")))
             (concat "      <section>\n"
                     (format-spec (plist-get info :reveal-title-slide-template) (org-html-format-spec info))
                     "      </section>\n")
           ""))
       contents
       "    </div>\n  </div>\n"
       (org-reveal--build-pre/postamble 'postamble info)
       (org-reveal-scripts info)
       "</body>
</html>\n")))



(defvar org-fixups/after-export-reveal-file nil
  "Hook called after an org file is exported to reveal. Each
fn on the hook is called with the fully-qualified filename of
the file produced during export.

You can add a hook like this:

  (add-hook 'org-fixups/after-export-reveal-file
      (lambda (filename)
        (message \"the file was exported to %s\" expanded-filename)))

or, like this:

  (add-hook 'org-fixups/after-export-reveal-file
            'my-custom-function-that-takes-one-arg)

")

(defun org-fixups/post-process-exported-reveal-file (filename)
  "Post-process the file that was just exported to HTML."
  (let ((expanded-filename (expand-file-name filename)))
    (run-hook-with-args 'org-fixups/after-export-reveal-file expanded-filename)))

(if (fboundp 'org-reveal-export-to-html)
    ;; allow post-processing of the file after export
    (defun org-reveal-export-to-html
      (&optional async subtreep visible-only body-only ext-plist)
      "Export current buffer to a reveal.js HTML file."
      (interactive)
      (let* ((extension (concat "." org-html-extension))
             (file (org-export-output-file-name extension subtreep)))
        (org-export-to-file 'reveal file
          async subtreep visible-only body-only ext-plist
          'org-fixups/post-process-exported-reveal-file))))


(provide 'org-fixups)

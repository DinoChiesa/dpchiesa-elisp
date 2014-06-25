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

;; fixup to eliminate title slide optionally

(if (fboundp 'org-reveal-template) nil t)


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
</html>\n"))


(provide 'org-fixups)

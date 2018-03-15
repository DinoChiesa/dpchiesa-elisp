;;; mvn-pom-mode.el --- A major mode for pom files

;; 


;;; Code:

(require 'nxml-mode)
(require 'url)
(require 's)    ;; magnars' string lib
(require 'dash) ;; magnars' functional lib

(defvar mvn-pom-base-url "https://search.maven.org/solrsearch")

(defun mvn-pom--get-search-url (term &optional output-format)
  "creates the search URL for mvn for a generic search term."
  (format "%s/select?q=%s&rows=20&wt=%s" mvn-pom-base-url term (or output-format "xml")))

(defun mvn-pom--get-search-version-url (group artifact &optional output-format)
  "creates the search URL for mvn to search for versions of a specific group+artifact."
  (format "%s/select?q=g:\"%s\"+AND+a:\"%s\"&rows=20&core=gav&wt=%s" mvn-pom-base-url group artifact (or output-format "xml")))

;; curl -i "http://search.maven.org/solrsearch/select?q=g:%22com.auth0%22+AND+a:%22java-jwt%22&rows=20&core=gav&wt=xml"


(defun mvn-pom--generate-menu (candidates &optional format heading)
  "Generate a menu suitable for use in `x-popup-menu' from the
list of candidates. Each item in the list of CANDIDATES is a
string. The FORMAT is a function that formats the displayable menu choice;
it is called once for each candidate.  The HEADING is a string used in the
result, which will appear as a menu heading.

For example, calling it with candidates ('(\"one\" \"two\")) and no additional
areguments will return a structure like this:

  (\"Select a Choice...\"
    (\"Ignored pane title\"
      (\"one\" \"one\")
      (\"two\" \"two\")))

Calling it with ('(\"Class1\" \"Class2\")
                 #'(lambda (x) (concat \"import \" x \";\"))
                 \"Add dependency\")

...will return output like this:

  (\"Add dependency...\"
    (\"Ignored pane title\"
      (\"import a.b.c.Class;\" \"a.b.c.Class\")
      (\"import x.y.z.Class;\" \"x.y.z.Class\")))

The `x-popup-menu' will display the cadr of each of the choices.

The result of the user choice is the cdr of the selected item. In this case, it
will be something like (\"x.y.z.Class\") .

"

  (let ((items (mapcar #'(lambda (elt) (list (funcall (or format 'identity) elt) elt))
                       candidates)))
    (setq items (cons "Ignored pane title" items))
    (list (or heading "Select a Choice...") items)))



(defun mvn-pom--get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


;; (defun avg-damp (n)
;;   (lexical-let ((n n))
;;     (lambda(x) (/ x n))))

(defun mvn-pom--element-has-name (n)
  "returns a function that can be used as a predicate in the dash library.
The lambda returns true when the element has a name attribute with the given value."
  (lexical-let ((n n))
    (lambda (elt)
      (let ((attrs (xml-node-attributes elt)))
        (equal n (cdr (assq 'name attrs)))))))


(defun mvn-pom--parse-search-results-get-str-with-name (n)
  "returns a function that can be used in a mapcar against a document.
The lambda returns the first str child that has a name attribute with value N, a string."
  (lexical-let ((n n))
    (lambda (doc)
      (car (xml-node-children
            (-first
             (mvn-pom--element-has-name n)
             (xml-get-children doc 'str)))))))


(defun mvn-pom--search-result-documents (url)
  "returns the parsed docs list from the mvn search response"
  (let* ((buffer (url-retrieve-synchronously url t t))
         (root (with-current-buffer buffer
                 (xml-parse-region (point-min) (point-max))))
         (response (car root))
         (result (car (xml-get-children response 'result))))
    (xml-get-children result 'doc)))


(defun mvn-pom--search-asset (term)
  "searches for an asset on mvn repo. Returns a list of ID results. For example,
searching for \"java-jwt\" returns

(\"com.auth0:java-jwt\" \"de.notizwerk:java-jwt\" \"nl.open:java-jwt-nodependencies\")

"
  (let ((url (mvn-pom--get-search-url term "xml")))
    (mapcar
      (mvn-pom--parse-search-results-get-str-with-name "id")
      (mvn-pom--search-result-documents url))))



(defun mvn-pom-moveto-dependency-insertion-point ()
  "Move the point to the plce to insert a new dependency stanza.
"
  (interactive)
  (goto-char (point-min))
  (nxml-down-element)
  (let ((found-eof))
    (while (and
            (not (looking-back "/dependencies\s*>" (line-beginning-position)))
            (not found-eof))
      (condition-case nil
          (nxml-forward-element)
        (error (setq found-eof t))))
    (if found-eof nil
      (nxml-move-tag-backwards (line-beginning-position))
      t)))


;; (defun maven-pom-add-dependency* (search-term &optional scope-flag)
;;   (interactive "MSearch: \nP")
;;   (let* ((gids (maven-pom-search-completing-groupIds search-term))
;;          (vs (maven-pom-search-completing-versions gids)))
;;     (maven-pom-insert-dependency-xml vs scope-flag)))



(defun mvn-pom--present-selection (documents)
  "present a choice for a dependency stanza to add (groupId and artifactId)."
  (let* ((candidates (mapcar 'identity documents))
         (chosen (x-popup-menu (mvn-pom--get-menu-position)
                               (mvn-pom--generate-menu candidates nil "Select artifact..."))))

    (when chosen ;; actually a list containing a single string (classname)
      (let* ((choice-pair (s-split ":" (car chosen)))
             (group-id (car choice-pair))
             (artifact-id (cadr choice-pair))
             (url (mvn-pom--get-search-version-url group-id artifact-id))
             (documents (mvn-pom--search-result-documents url))
             (versions
              (mapcar
               (mvn-pom--parse-search-results-get-str-with-name "v")
               documents))
             (version-chosen
              (x-popup-menu (mvn-pom--get-menu-position)
                            (mvn-pom--generate-menu versions nil "Select version..."))))
        (when version-chosen
          (let ((version-choice (car version-chosen)))
            (list group-id artifact-id version-choice)
            ))))))


(defun mvn-pom-add-dependency (search-term &optional scope-flag)
  "Do search, then choose groupId, then choose version.  Search
for artifact by search term and insert the dependency stanza.
"
  (interactive "MSearch: \nP")
  (let ((dependency-options (mvn-pom--search-asset search-term)))
    (if dependency-options
        (let ((choice (mvn-pom--present-selection dependency-options)))
          (when choice
            (mvn-pom-moveto-dependency-insertion-point)
            (let ((start-posn-of-new-dependency (point)))
              (insert "\n<dependency>\n")
              (insert (format "<groupId>%s</groupId>\n" (nth 0 choice)))
              (insert (format "<artifactId>%s</artifactId>\n" (nth 1 choice)))
              (insert (format "<version>%s</version>\n" (nth 2 choice)))
              (insert "</dependency>\n\n")
              (xmltok-forward)
              (indent-region start-posn-of-new-dependency (point))
              (goto-char start-posn-of-new-dependency)
              (recenter-top-bottom)
              ))))))



  ;; (if (mvn-pom-moveto-dependency-insertion-point)
  ;;     (progn
  ;;       (maven-pom-add-dependency search-term scope-flag)
  ;;       (indent-for-tab-command))))

(defvar maven-pom-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map nxml-mode-map)
    (define-key map  "\C-cd" 'mvn-pom-add-dependency)
         map))


(add-to-list 'auto-mode-alist '("pom\\.xml\\'" . mvn-pom-mode))

;;
;; Define mvn-pom-mode.
(define-derived-mode mvn-pom-mode nxml-mode
  "mvn-pom-mode" "Major mode for editting maven pom files
\\{mvn-pom-mode-map}
"
  (use-local-map mvn-pom-mode-map)
  (run-mode-hooks 'mvn-pom-mode-hook))

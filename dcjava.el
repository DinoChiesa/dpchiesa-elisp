;;; dcjava.el --- utility functions for working with Java
;;
;; Copyright (C) 2014 Dino Chiesa
;;


;; relates a classname to its common package, suitable for import.
(defconst dcjava-helper-classname-alist
  (list
   '("Benchmark" "com.ibm.devworks.util")
   '("Hashtable" "java.util")
   '("Calendar" "java.util")
   '("GregorianCalendar" "java.util")
   '("Enumeration" "java.util")
   '("ArrayList" "java.util")
   '("File" "java.io")
   '("IOException" "java.io")
   '("InputStream" "java.io")
   '("StringWriter" "java.io")
   '("FileInputStream" "java.io")
   '("BufferedInputStream" "java.io")
   '("XMLReader" "org.xml.sax")
   '("InputSource" "org.xml.sax")
   '("SAXException" "org.xml.sax")
   '("Document" "org.w3c.dom")
   '("XPathExpressionException" "javax.xml.xpath")
   '("DocumentBuilder" "javax.xml.parsers")
   '("DocumentBuilderFactory" "javax.xml.parsers")
   '("TransformerException" "javax.xml.transform")
   '("DOMResult" "javax.xml.transform.dom")
   '("DOMSource" "javax.xml.transform.dom")
   '("SAXSource" "javax.xml.transform.sax")
   '("StreamResult" "javax.xml.transform.stream")
   '("TransformerFactory" "javax.xml.transform")
   '("Transformer" "javax.xml.transform")
   '("SAXDocumentParser" "com.sun.xml.fastinfoset.sax")
   '("ParserConfigurationException" "javax.xml.parsers")
   '("TransformerConfigurationException" "javax.xml.transform")
   )
  )

(defun dcjava-auto-add-import ()
  "adds an import statement for the class or interface at point, if possible."
  (interactive)
  (let* ((symbol-name
          (save-excursion
            (beginning-of-thing 'symbol)
            (set-mark (point))
            (forward-word)
            (buffer-substring-no-properties (mark) (point))))
         (matching-pair
          (assoc symbol-name dcjava-helper-classname-alist)))
    (cond
     (matching-pair
         (let* ((package-name (cadr matching-pair))
                (import-statement
                 (concat "import " package-name "." symbol-name ";")))

           (save-excursion
             (if (not (re-search-backward (concat "^" import-statement) nil t))
                 (if (re-search-backward "^import" nil t)
                     (progn
                       (end-of-line)
                       (newline)
                       (insert import-statement)
                       (message import-statement)
                       ))))))
     (t
      (message "did not find class %s" symbol-name)))
      ))



(provide 'dcjava)

;;; dcjava.el ends here

;;; dcjava.el --- utility functions for working with Java
;;
;; Copyright (C) 2014 Dino Chiesa
;;


;; relates a classname to its common package, suitable for import.
(defconst dcjava-helper-classname-alist
  (list
   '("AlgorithmParameters" "java.security")
   '("ArrayList" "java.util")
   '("Arrays" "java.util")
   '("Benchmark" "com.ibm.devworks.util")
   '("BigInteger" "java.math")
   '("BufferedInputStream" "java.io")
   '("JcePBESecretKeyDecryptorBuilder" "java.security")
   '("ByteArrayInputStream" "java.io")
   '("Calendar" "java.util")
   '("Charset" "java.nio.charset")
   '("Cipher" "javax.crypto")
   '("Collection" "java.util")
   '("Collections2" "com.google.common.collect")
   '("DOMResult" "javax.xml.transform.dom")
   '("DOMSource" "javax.xml.transform.dom")
   '("Document" "org.w3c.dom")
   '("DocumentBuilder" "javax.xml.parsers")
   '("DocumentBuilderFactory" "javax.xml.parsers")
   '("EncryptedPrivateKeyInfo" "javax.crypto")
   '("Enumeration" "java.util")
   '("File" "java.io")
   '("FileInputStream" "java.io")
   '("OutputStream" "java.io")
   '("FileNotFoundException" "java.io")
   '("GeneralSecurityException" "java.security")
   '("GregorianCalendar" "java.util")
   '("Hashtable" "java.util")
   '("IOException" "java.io")
   '("InputSource" "org.xml.sax")
   '("InputStream" "java.io")
   '("InvalidAlgorithmParameterException" "java.security")
   '("InvalidKeyException" "java.security")
   '("InvalidKeySpecException" "java.security.spec")
   '("Key" "java.security")
   '("KeyFactory" "java.security")
   '("KeySpec" "java.security.spec")
   '("List" "java.util")
   '("NoSuchAlgorithmException" "java.security")
   '("NoSuchPaddingException" "javax.crypto")
   '("PBEKeySpec" "javax.crypto.spec")
   '("PKCS8EncodedKeySpec" "java.security.spec")
   '("PKCS8Key" "org.apache.commons.ssl")
   '("ParserConfigurationException" "javax.xml.parsers")
   '("Predicate" "com.google.common.base")
   '("PrivateKey" "java.security")
   '("Properties" "java.util")
   '("PublicKey" "java.security")
   '("RSAPrivateCrtKeySpec" "java.security.spec")
   '("RSAPrivateKey" "java.security.interfaces")
   '("RSAPublicKey" "java.security.interfaces")
   '("SAXDocumentParser" "com.sun.xml.fastinfoset.sax")
   '("SAXException" "org.xml.sax")
   '("SAXSource" "javax.xml.transform.sax")
   '("SecretKeyFactory" "javax.crypto")
   '("StreamResult" "javax.xml.transform.stream")
   '("StringUtils" "org.apache.commons.lang")
   '("StringWriter" "java.io")
   '("SecureRandom" "java.security")
   '("Transformer" "javax.xml.transform")
   '("TransformerConfigurationException" "javax.xml.transform")
   '("TransformerException" "javax.xml.transform")
   '("TransformerFactory" "javax.xml.transform")
   '("X509EncodedKeySpec" "java.security.spec")
   '("NoSuchProviderException" "java.security")
   '("XMLReader" "org.xml.sax")
   '("XPathExpressionException" "javax.xml.xpath")
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

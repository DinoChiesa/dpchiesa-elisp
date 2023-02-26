;;; xml-to-string --- convert lisp s-expressions to xml string

;; Copyright (C) 2012 Dave Paroulek

;; Author: Dave Paroulek <upgradingdave@gmail.com>
;; Maintainer: Dave Paroulek <upgradingdave@gmail.com>
;; Version: 0.1.0
;; Keywords: xml, data
;; URL: http://github.com/upgradingdave/xml-to-string

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Functions like xml-parse-file and xml-parse-region output s-expressions
;; like this: 
;;
;; xml-list   ::= (node node ...)
;;    node       ::= (qname attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    qname      ::= (:namespace-uri . "name") | "name"
;;    attribute_list ::= ((qname . "value") (qname . "value") ...)
;;                       | nil
;;    string     ::= "..."

;; The functions below take s-exp's like these and output them back into xml strings

;;; Code:

(defun xml-to-string (node-list)
  "Convert sexp created by xml-parse-* function back to xml string"
  (let ((result))
    (dolist (node node-list result)
      (setq result (concat result (xml-to-string-parse-node node))))))

(defun xml-to-string-parse-node (node)
  "Convert `(qname attribute-list . child_node_list)` to xml string representation"
  (concat "<" (xml-to-string-parse-qname (symbol-name (car node))) 
          (if (cadr node) " ")
          (xml-to-string-parse-attr-list (cadr node)) ">"
          (xml-to-string-parse-child-node-list (cddr node))
          "</" (xml-to-string-parse-qname (symbol-name (car node))) ">"))

(defun xml-to-string-parse-child-node-list (child-node-list)
  (let (value)
    (dolist (node child-node-list value)
      (setq value (concat value (xml-to-string-parse-child-node node))))))

(defun xml-to-string-parse-child-node (child-node)
  "Convert `node | string' to string representation"
  (if (stringp child-node)
      child-node
    (xml-to-string-parse-node child-node)))

(defun xml-to-string-parse-qname (node)
  "Currently just assumes it's name, need to implement namespace url"
  node)

(defun xml-to-string-parse-attr-list (attr-list)
  "Convert `((qname . \"value\") (qname . \"value\") ...)' to a
  string like \"qname=value qname=value\" ..."
  (mapconcat (lambda (elem) (concat (symbol-name (car elem))
                               "="
                               "\"" (cdr elem) "\"")) attr-list " "))


(provide 'xml-to-string)
    
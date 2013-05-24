;;; smart-op.el --- Insert operators with surrounding spaces smartly
;;
;; derived from  smart-operator.el, which is
;; Copyright (C) 2004, 2005, 2007, 2008, 2009 William Xu
;;
;; Author: William Xu <william.xwl@gmail.com>
;; Version: 1.1
;; Url: http://xwl.appspot.com/ref/smart-operator.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; When typing operators, this package can automatically insert spaces
;; before and after operators. For instance, `=' will become ` = ', `+='
;; will become ` += '. This is handy for writing C-style sources.
;;
;; To use, put this file to your load-path and the following to your
;; ~/.emacs:
;;             (require 'smart-op)
;;
;; Then `M-x smart-op-mode' for toggling this minor mode.
;;
;; Usage Tips
;; ----------
;;
;; - If you want it to insert operator with surrounding spaces , you'd
;;   better not type the front space yourself, instead, type operator
;;   directly. smart-op-mode will also take this as a hint on how
;;   to properly insert spaces in some specific occasions. For
;;   example, in c-mode, `a*' -> `a * ', `char *' -> `char *'.
;;
;;; Acknowledgements
;;
;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.
;;
;;; TODO:
;;
;; - for c mode, probably it would be much better doing this in cc-mode.

;;; Code:

;;; smart-op minor mode

(require 'thingatpt)

(defvar smart-op-smartoperators-alist
  '(
    ( ?= "[^=]" "[^=\+\-/*%&!|^ ]")
    ( ?+ t "[^\+ ]")
    ( ?- t "[^\- ]")
    ( ?* t "[^/ ]")
    ( ?/ t "[^\*/ ]")
    ( ?% t "[^ ]")
    ( ?! t "[^ ]")
    ( ?& t "[^& ]")
    ( ?| t "[^\| ]")
    ( ?, t t t)
    )
"A list of lists; each sublist has three elements. The first
element is a char that gets smart insertion behavior. This often
that when the operator is typed, insert one space, then insert
the operator, then insert one more space. The second and third
elements in each list are strings, regexes that are passed to
`looking-at' and `looking-back', respectively. Insertion of a
space before and after the char occurs if both of those calls
evaluate to true. Either of the regexi can be replaced with
non-string non-nil (eg, t) to always vote yes.

An optional fourth element in the list, when non-nil, means
to NOT insert a preceding space, as with, for example, a comma.
The following space will still be smart-inserted.

")


(defvar smart-op-postfix-doubles
  '(?+ ?-)
  "A list of operators that can be doubled as postfix.
In that case, there will be no spacing between the operators, and
no spacing before the first operator.")

(defvar smart-op-nonpostfix-doubles
  '(?& ?|)
  "A list of operators that can be doubled, but not postfixed.
In this case, there will be no spacing between the operators but
there will be a space before the first operator.")

(defvar smart-op-operators-that-can-precede-equals
  '(?- ?+ ?* ?/ ?% ?& ?| ?^ ?= ?!)
  "A list of operators that can be used before an equals sign, as
in a compound assignment statement or logical test. If an equals
sign follows one of these, there will be no space applied ")


(defvar smart-op--operator-chars nil
  "for internal use only.")

(defun smart-op--operator-list ()
  "returns the list of chars for which smart insertion is performed."
  (or smart-op--operator-chars
      (setq smart-op--operator-chars
            (mapcar 'car smart-op-smartoperators-alist))))

(defvar smart-op--compound-op-regexp nil
  "for internal use only.")


(defun smart-op--op-that-precedes-equals-regexp ()
  "Returns a string, suitable for use as a regexp that tests for
an operator that can be used with equals sign, as in a compound
assignment.

This fn computes the regexp once, then caches it and returns
the same string on subsequent invocations.

"
  (or smart-op--compound-op-regexp
      (setq smart-op--compound-op-regexp
            (concat
             "["
             (regexp-quote
              (apply 'concat (mapcar 'string smart-op-operators-that-can-precede-equals)))
             "] ?"))))


(defun smart-op-self-insert ()
  "Intelligently insert spaces around the given operator e.g.,
'=' will become ' = ', except when the previous char is an
operator that can precede '=', in which case no prior space is
injected.  This magic adjustment of what gets inserted also
removes or collapses spaces, and happens only when point is not
in a comment or string.

I think this needs to be interactive in order to put it into the
mode-map.

See also `smart-op-self-insert-helper'.

"
  (interactive)
  (setq smart-op--last-cmd-was-space nil)
  (let ((op last-command-event)
        (face (plist-get (text-properties-at (point)) 'face)))
    (if (or (and (listp face)
                 (or (memq 'font-lock-string-face face)
                     (memq 'font-lock-comment-face face)))
            (and (symbolp face)
                 (or (eq 'font-lock-string-face face)
                     (eq 'font-lock-comment-face face))))
        ;; special case double-slash comments
        (progn
        (if (looking-back "/ ")
            (backward-delete-char 1))
        (insert (string op)))

      (smart-op-self-insert-helper op))))



(defun smart-op-self-insert-helper (op)
  "Intelligently insert spaces around the given operator e.g.,
'=' will become ' = '. This happens except when the previous char is an
operator that can precede '=', in which case no preceding space is
inserted.

"
  ;; (b-o-l (save-excursion (beginning-of-line) (point)))
  (let ((s-op (string op))
        (member (assoc op smart-op-smartoperators-alist))
        rex1 rex2 no-preceding-space)
    (setq rex1 (cadr member)
          rex2 (caddr member)
          no-preceding-space (cadddr member))
    (cond
     ((and rex1 rex2)
      ;; do we want to insert a space prior to the operator?
      (if (and
           (or (and (stringp rex1) (looking-at rex1))
               (and (not (stringp rex1)) rex1))
           (or (and (stringp rex2) (looking-back rex2))
               (and (not (stringp rex2)) rex2)))

          ;; yes, we do. (unless it's the beginning of line)
          (progn
            (if (bolp)
                (insert s-op)
              (if (and (not no-preceding-space)
                       (not (looking-back " ")))
                  (insert " "))
              (insert (concat s-op " "))))

        ;; no, we do not. But, maybe delete the intervening space,
        ;; as when using compound assignment operators like *=.
        (if (and
             (eq op ?=)
             (looking-back (smart-op--op-that-precedes-equals-regexp)))
            (progn
              ;; collapse all spaces
              (while (looking-back " ") (backward-delete-char 1))
              ;; insert a space prior to the compound operator, as
              ;; necessary
              (save-excursion
                (backward-char 1)
                ;; javascript uses !== and ===
                (if (and (not (looking-back " "))
                         (or (not (eq major-mode 'js-mode))
                             (and (not (looking-back "!"))
                                  (not (looking-back "=")))))
                 (insert " ")))

              ;; insert the operator and maybe a space
              (insert s-op)
              (if (and (not (looking-at " "))
                       (and
                        (eq op ?=)
                        (not (looking-at "="))))
                  (insert " ")))

          ;; no fixup needed, just insert the operator
          (insert s-op))

        ;; finally, fixup some other cases...
        (cond

         ;; remove whitespace before doubled postfix operators,
         ;; and remove whitespace between them.
         ((and
             (memq op smart-op-postfix-doubles)
             (looking-back (regexp-quote (concat " " s-op " " s-op))))
          (backward-delete-char 4)
          (insert (concat s-op s-op)))


         ;; remove whitespace between other doubled operators
         ((and
           (memq op smart-op-nonpostfix-doubles)
           (looking-back (regexp-quote (concat " " s-op " " s-op))))

          (backward-delete-char 3)
          (insert (concat s-op s-op " ")))

         ;; remove whitespace between and before doubled slash comment,
         ;; or between and before /* comment starter.
         ((or (and (eq op ?/)
                   (looking-back (regexp-quote " / /")))
              (and (eq op ?*)
                   (looking-back (regexp-quote " / *"))))
          (save-excursion
               (backward-char 1)
               (backward-delete-char 1)
               (backward-char 1)
               (backward-delete-char 1))
          (insert " "))

         ;; remove whitespace between */ comment ender
         ((and (eq op ?/)
               (looking-back (regexp-quote " * /")))
          (save-excursion
               (backward-char 1)
               (backward-delete-char 1))))))

     (t
      (insert s-op)))))


(defvar smart-op--last-cmd-was-space nil
  "for internal use only.")


(defun smart-op-maybe-insert-space ()
  "Maybe insert a space. This is to prevent multiple spaces when
one already exists. If the space is already there but is forward,
then move forward one character."
  (interactive)
  (cond

   ;; let's not be pigheaded about it.
   (smart-op--last-cmd-was-space
    (insert " "))

   ((looking-back " ")
    nil)

   ((looking-at " ")
    (forward-char 1))

   (t
    (insert " ")))

  (setq smart-op--last-cmd-was-space t))





(defvar smart-op-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap " " 'smart-op-maybe-insert-space)
    (define-key keymap "=" 'smart-op-self-insert)
    (define-key keymap "<" 'smart-op-self-insert)
    (define-key keymap ">" 'smart-op-self-insert)
    (define-key keymap "%" 'smart-op-self-insert)
    (define-key keymap "+" 'smart-op-self-insert)
    (define-key keymap "-" 'smart-op-self-insert)
    (define-key keymap "*" 'smart-op-self-insert)
    (define-key keymap "/" 'smart-op-self-insert)
    (define-key keymap "&" 'smart-op-self-insert)
    (define-key keymap "|" 'smart-op-self-insert)
    (define-key keymap "!" 'smart-op-self-insert)
    (define-key keymap ":" 'smart-op-self-insert)
    (define-key keymap "?" 'smart-op-self-insert)
    (define-key keymap "," 'smart-op-self-insert)
    (define-key keymap "." 'smart-op-self-insert)
    keymap)
  "Keymap used my `smart-op-mode'.")

;;;###autoload
(define-minor-mode smart-op-mode
  "Insert operators with surrounding spaces smartly."
  nil " _+_" smart-op-mode-map)

(defun smart-op-mode-on ()
  (smart-op-mode 1))


(provide 'smart-op)

;;; smart-op.el ends here

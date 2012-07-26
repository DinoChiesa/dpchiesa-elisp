;; menu-play.el
;;
;; playing with menus, including x-popup-menu.
;;
;; Tue, 17 Apr 2012  16:44
;;



(x-popup-menu
 t
 '("Special message"
   (""
    ("This is line 1" .    nil)
    ("This is line 2"    nil )
    ("This is line 3"    nil )
    ("OK"    t))))


 (x-popup-menu t '("TITLE" ("PANE"
                            ("LINE1" . nil)
                            ("LINE2" . 1))))



    (x-popup-menu t (list "TITLE"
                          (list
                           "PANE"
                           (list "menu item 1" nil )
                           (list "menu item 2" nil )
                           (list "--space" nil )
                           (list "menu item 3" nil )
                           )))

    (x-popup-menu t (list "TITLE"
                          (list
                           "PANE"
                           '(menu-item "menu item 1" nil )
                           '(list "menu item 2" nil
                                 :button (:toggle . nil)
                                 :enable nil
                                 :visible nil)
                           (list "--double-line" nil )
                           (list "menu item 3" nil )
                           )))


(defun foop()
  "does nothing"
  (interactive)
  t)

(defun foop2()
  "does nothing"
  (interactive)
  t)
(defun foop3()
  "does nothing"
  (interactive)
  t)
(defun foop4()
  "does nothing"
  (interactive)
  t)


(define-key global-map [dino-menu] (make-sparse-keymap "dino-menu"))
(define-key global-map [dino-menu dino] (cons (purecopy "Dino1") dino-menu-1))

;; Here's what I think I learned:
;;
;; 1. must use "make-sparse-keymap" (intuitively) to create the thing
;;    that holds a popup menu.
;;
;; 2. use `define-key' after that, to add menu items to that menu.
;;    There's that intuitive simplicity again. n
;;
;; 3. The first item added appears last in the rendered popup menu.
;;
;; 4. Each "key" (menu item) must have a no-arg function attached.
;;
;; 5. There is also a property map.  If you don't set :keys to nil, emacs
;;    will garbage up the menu display with a key sequence.
;;

    (defvar dino-menu-1 (make-sparse-keymap "Dino1"))
    (define-key dino-menu-1 [event-name-1]
      `(menu-item ,(purecopy "OK")
                  foop
                  :keys ""
                  :visible t
                  :enable t
                  :help ,(purecopy "Help for menu item 1")))

(define-key dino-menu-1 [separator-1]  menu-bar-separator)

(define-key dino-menu-1 [event-name-2]
  `(menu-item ,(purecopy "Menu Item 2")
              nil
              :visible t
              :button (:radio . t)
              :enable t
              :help ,(purecopy "Help for menu item 2")))

(define-key dino-menu-1 [event-name-3]
  `(menu-item ,(purecopy "Menu Item 3")
              nil
              :visible t
              :button (:toggle . t)
              :enable nil
              :help ,(purecopy "Help for menu item 3")))

(define-key dino-menu-1 [event-name-4]
  `(menu-item ,(purecopy "Menu Item 4")
              foop4
              :keys ""
              :enable t
              :help ,(purecopy "Help for menu item 4")))

(define-key dino-menu-1 [event-name-5]
  `(menu-item ,(purecopy "Menu Item 5")
              nil
              :keys ""
              :enable t
              :help ,(purecopy "Help for menu item 5")))

(x-popup-menu t dino-menu-1)


(defun multiline-message-box (msg)
  "display a multiline message box on Windows.

According to bug #11138, when passing a message with newlines to
`message-box' on Windows, the rendered message-box appears all on
one line.

This function can work around that problem.
"
  (flet ((ok (&optional p1 &rest args) t))

    (let ((parts (split-string msg "\n"))
          (c 0)
          (menu-1 (make-sparse-keymap "Attention")))

      (define-key menu-1 [menu-1-ok-event]
        `(menu-item ,(purecopy "OK")
                    ok
                    :keys ""))

      (define-key menu-1 [separator-1] menu-bar-separator)

      (while (nth c parts)
        (define-key menu-1 (vector (intern (format "menu-1-fake-event-%d" c)))
          `(menu-item ,(purecopy (nth c parts))
                      nil
                      :keys ""
                      :enable t))

        (setq c (1+ c)))
      (x-popup-menu t menu-1))))


(multiline-message-box "Hello!\nI must be going!\nThis is line 3.")





    (flet ((ok (&optional p1 &rest args) t))
      (setq menu-1 (make-sparse-keymap "Title"))
      (define-key menu-1 [menu-1-ok-event]
        `(menu-item "OK"
                    ok
                    :keys ""
                    :visible t
                    :enable t))
      (x-popup-menu t menu-1))




    (flet ((ok (&optional p1 &rest args) t))
      (setq menu-1 (make-sparse-keymap "Title"))
      (define-key menu-1 [menu-1-event-ok]
        `(menu-item "OK"
                    ok
                    :keys ""
                    :visible t
                    :enable t))
      (define-key menu-1 [menu-1-event-1]
        `(menu-item "This is line 1"
                    nil
                    :keys ""
                    :visible t
                    :enable t))
      (x-popup-menu t menu-1))




(x-popup-menu
 t
 (list
       "Commands"
       (list
        (cons "Copy                C-insert" nil)
        (cons "Goto Line"              "(call-interactively 'goto-line)")
        (cons "Paste/yank        S-insert"     "(yank)")
        (cons "Redo"                   "(redo)")
        (cons "Search files"           "(call-interactively 'grep)")
        (cons "Undo                 C-_"       "(undo)")
        (cons "Word completion M-/"    "(call-interactively 'dabbrev-expand)")
        )))




(x-popup-menu
 t
 '("title1"
   ("ignored1"
     ('menu-item "Item1"))))





  (menu-item "Item1" nil
            :button (:none . nil)))))






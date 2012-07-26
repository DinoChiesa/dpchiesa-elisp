;; see http://lists.gnu.org/archive/html/help-gnu-emacs/2010-12/msg01114.html

;; rather than modify cc-cmds.el, I use this to
;; just redefine/wrap the functions .

(defun c-mask-paragraph (fill-paragraph apply-outside-literal fun &rest args)
  ;; Calls FUN with ARGS ar arguments while the current paragraph is
  ;; masked to allow adaptive filling to work correctly.  That
  ;; includes narrowing the buffer and, if point is inside a comment,
  ;; masking the comment starter and ender appropriately.
  ;;
  ;; FILL-PARAGRAPH is non-nil if called for whole paragraph filling.
  ;; The position of point is then less significant when doing masking
  ;; and narrowing.
  ;;
  ;; If APPLY-OUTSIDE-LITERAL is nil then the function will be called
  ;; only if the point turns out to be inside a comment or a string.
  ;;
  ;; Note that this function does not do any hidden buffer changes.

  (let (fill
        ;; beg and end limit the region to narrow.  end is a marker.
        beg end
        ;; tmp-pre and tmp-post mark strings that are temporarily
        ;; inserted at the start and end of the region.  tmp-pre is a
        ;; cons of the positions of the prepended string.  tmp-post is
        ;; a marker pointing to the single character of the appended
        ;; string.
        tmp-pre tmp-post
        ;; If hang-ender-stuck isn't nil, the comment ender is
        ;; hanging.  In that case it's set to the number of spaces
        ;; that should be between the text and the ender.
        hang-ender-stuck
        ;; auto-fill-spaces is the exact sequence of whitespace between a
        ;; comment's last word and the comment ender, temporarily replaced
        ;; with 'x's before calling FUN when FILL-PARAGRAPH is nil.
        auto-fill-spaces
        (here (point))
        (c-lit-limits c-lit-limits)
        (c-lit-type c-lit-type))

    ;; Restore point on undo.  It's necessary since we do a lot of
    ;; hidden inserts and deletes below that should be as transparent
    ;; as possible.
      (if (and buffer-undo-list (not (eq buffer-undo-list t)))
        (setq buffer-undo-list (cons (point) buffer-undo-list)))

    ;; Determine the limits and type of the containing literal (if any):
    ;; C-LIT-LIMITS, C-LIT-TYPE;  and the limits of the current paragraph:
    ;; BEG and END.
    (c-save-buffer-state ()
      (save-restriction
        ;; Widen to catch comment limits correctly.
        (widen)
        (unless c-lit-limits
          (setq c-lit-limits (c-literal-limits nil fill-paragraph)))
        (setq c-lit-limits (c-collect-line-comments c-lit-limits))
        (unless c-lit-type
          (setq c-lit-type (c-literal-type c-lit-limits))))

      (save-excursion
        (unless (c-safe (backward-char)
                        (forward-paragraph)
                        (>= (point) here))
          (goto-char here)
          (forward-paragraph))
        (setq end (point-marker)))
      (save-excursion
        (unless (c-safe (forward-char)
                        (backward-paragraph)
                        (<= (point) here))
          (goto-char here)
          (backward-paragraph))
        (setq beg (point))))

    (unwind-protect
        (progn
          ;; For each of the possible types of text (string, C comment ...)
          ;; determine BEG and END, the region we will narrow to.  If we're in
          ;; a literal, constrain BEG and END to the limits of this literal.
          ;;
          ;; For some of these text types, particularly a block comment, we
          ;; may need to massage whitespace near literal delimiters, so that
          ;; these don't get filled inappropriately.
          (cond

           ((eq c-lit-type 'c++)        ; Line comment.
            (save-excursion
              ;; Limit to the comment or paragraph end, whichever
              ;; comes first.
              (set-marker end (min end (cdr c-lit-limits)))

              (when (<= beg (car c-lit-limits))
                ;; The region includes the comment starter, so we must
                ;; check it.
                (goto-char (car c-lit-limits))
                (back-to-indentation)
                (if (eq (point) (car c-lit-limits))
                    ;; Include the first line in the region.
                    (setq beg (c-point 'bol))
                  ;; The first line contains code before the
                  ;; comment.  We must fake a line that doesn't.
                  (setq tmp-pre t))))

            (setq apply-outside-literal t))

           ((eq c-lit-type 'c)          ; Block comment.
            (when
                (or (> end (cdr c-lit-limits))
                    (and (= end (cdr c-lit-limits))
                         (eq (char-before end) ?/)
                         (eq (char-before (1- end)) ?*)
                         ;; disallow "/*/"
                         (> (- (cdr c-lit-limits) (car c-lit-limits)) 3)))
              ;; There is a comment ender, and the region includes it.  If
              ;; it's on its own line, it stays on its own line.  If it's got
              ;; company on the line, it keeps (at least one word of) it.
              ;; "=====*/" counts as a comment ender here, but "===== */"
              ;; doesn't and "foo*/" doesn't.
              (unless
                  (save-excursion
                    (goto-char (cdr c-lit-limits))
                    (beginning-of-line)
                    ;; The following conjunct was added to avoid an
                    ;; "Invalid search bound (wrong side of point)"
                    ;; error in the subsequent re-search.  Maybe
                    ;; another fix would be needed (2007-12-08).
                    (and (> (- (cdr c-lit-limits) 2) (point))
                         (search-forward-regexp
                          (concat "\\=[ \t]*\\(" c-current-comment-prefix "\\)")
                          (- (cdr c-lit-limits) 2) t)
                         (not (search-forward-regexp
                               "\\(\\s \\|\\sw\\)"
                               (- (cdr c-lit-limits) 2) 'limit))
                             ;; The comment ender IS on its own line.  Exclude
                             ;; this line from the filling.
                         (set-marker end (c-point 'bol))))

                ;; The comment ender is hanging.  Replace all space between it
                ;; and the last word either by one or two 'x's (when
                ;; FILL-PARAGRAPH is non-nil), or a row of x's the same width
                ;; as the whitespace (when auto filling), and include it in
                ;; the region.  We'll change them back to whitespace
                ;; afterwards.  The effect of this is to glue the comment
                ;; ender to the last word in the comment during filling.
                (let* ((ender-start (save-excursion
                                      (goto-char (cdr c-lit-limits))
                                      (skip-syntax-backward "^w ")
                                      (point)))
                       (ender-column (save-excursion
                                       (goto-char ender-start)
                                       (current-column)))
                       (point-rel (- ender-start here))
                       ;; dino - BEGIN PATCH
                      (sentence-ends-comment
                       (save-excursion
                         (goto-char ender-start)
                         (and (search-backward-regexp
                               (c-sentence-end) (c-point 'bol) t)
                              (goto-char (match-end 0))
                         (looking-at "[ \t]*")
                         (= (match-end 0) ender-start))))
                       ;; dino - END PATCH
                       spaces)

                  (save-excursion
                    ;; Insert a CR after the "*/", adjust END
                    (goto-char (cdr c-lit-limits))
                    (setq tmp-post (point-marker))
                    (insert ?\n)
                    (set-marker end (point))

                    (forward-line -1)   ; last line of the comment
                    (if (and (looking-at (concat "[ \t]*\\(\\("
                                                 c-current-comment-prefix
                                                 "\\)[ \t]*\\)"))
                             (eq ender-start (match-end 0)))
                        ;; The comment ender is prefixed by nothing but a
                        ;; comment line prefix.  IS THIS POSSIBLE?  (ACM,
                        ;; 2006/4/28).  Remove it along with surrounding ws.
                        (setq spaces (- (match-end 1) (match-end 2)))
                      (goto-char ender-start))
                    (skip-chars-backward " \t\r\n") ; Surely this can be
                                        ; " \t"? "*/" is NOT alone on the line (ACM, 2005/8/18)

                    ;; What's being tested here?  2006/4/20.  FIXME!!!
                    (if (/= (point) ender-start)
                        (progn
                          (if (<= here (point))
                              ;; Don't adjust point below if it's
                              ;; before the string we replace.
                              (setq point-rel -1))
                          ;; Keep one or two spaces between the
                          ;; text and the ender, depending on how
                          ;; many there are now.
                          (unless spaces
                            (setq spaces (- ender-column (current-column))))
                          (setq auto-fill-spaces (c-delete-and-extract-region
                                                  (point) ender-start))
                          ;; paragraph filling condenses multiple spaces to
                          ;; single or double spaces.  auto-fill doesn't.
                          (if fill-paragraph
                              (setq spaces
                                    (max
                                     (min spaces
                                          ;; dino - BEGIN PATCH
                                          ;; (if sentence-end-double-space 2 1))
                                         (if (and sentence-ends-comment
                                                  sentence-end-double-space)
                                             2 1))
                                          ;; dino - END PATCH
                                     1)))
                          ;; Insert the filler first to keep marks right.
                          (insert-char ?x spaces t)
                          (setq hang-ender-stuck spaces)
                          (setq point-rel
                                (and (>= point-rel 0)
                                     (- (point) (min point-rel spaces)))))
                      (setq point-rel nil)))

                  (if point-rel
                      ;; Point was in the middle of the string we
                      ;; replaced above, so put it back in the same
                      ;; relative position, counting from the end.
                      (goto-char point-rel)))
                ))

            (when (<= beg (car c-lit-limits))
              ;; The region includes the comment starter.
              (save-excursion
                (goto-char (car c-lit-limits))
                (if (looking-at (concat "\\(" comment-start-skip "\\)$"))
                    ;; Begin with the next line.
                    (setq beg (c-point 'bonl))
                  ;; Fake the fill prefix in the first line.
                  (setq tmp-pre t))))

            (setq apply-outside-literal t))

           ((eq c-lit-type 'string)     ; String.
            (save-excursion
              (when (>= end (cdr c-lit-limits))
                (goto-char (1- (cdr c-lit-limits)))
                (setq tmp-post (point-marker))
                (insert ?\n)
                (set-marker end (point)))
              (when (<= beg (car c-lit-limits))
                (goto-char (1+ (car c-lit-limits)))
                (setq beg (if (looking-at "\\\\$")
                              ;; Leave the start line if it's
                              ;; nothing but an escaped newline.
                              (1+ (match-end 0))
                            (point)))))
            (setq apply-outside-literal t))

           ((eq c-lit-type 'pound)      ; Macro
            ;; Narrow to the macro limits if they are nearer than the
            ;; paragraph limits.  Don't know if this is necessary but
            ;; do it for completeness sake (doing auto filling at all
            ;; inside macros is bogus to begin with since the line
            ;; continuation backslashes aren't handled).
            (save-excursion
              (c-save-buffer-state ()
                (c-beginning-of-macro)
                (beginning-of-line)
                (if (> (point) beg)
                    (setq beg (point)))
                (c-end-of-macro)
                (forward-line)
                (if (< (point) end)
                    (set-marker end (point))))))

           (t                           ; Other code.
            ;; Try to avoid comments and macros in the paragraph to
            ;; avoid that the adaptive fill mode gets the prefix from
            ;; them.
            (c-save-buffer-state nil
              (save-excursion
                (goto-char beg)
                (c-forward-syntactic-ws end)
                (beginning-of-line)
                (setq beg (point))
                (goto-char end)
                (c-backward-syntactic-ws beg)
                (forward-line)
                (set-marker end (point))))))

          (when tmp-pre
            ;; Temporarily insert the fill prefix after the comment
            ;; starter so that the first line looks like any other
            ;; comment line in the narrowed region.
            (setq fill (c-save-buffer-state nil
                         (c-guess-fill-prefix c-lit-limits c-lit-type)))
            (unless (string-match (concat "\\`[ \t]*\\("
                                          c-current-comment-prefix
                                          "\\)[ \t]*\\'")
                                  (car fill))
              ;; Oops, the prefix doesn't match the comment prefix
              ;; regexp.  This could produce very confusing
              ;; results with adaptive fill packages together with
              ;; the insert prefix magic below, since the prefix
              ;; often doesn't appear at all.  So let's warn about
              ;; it.
              (message "\
Warning: Regexp from `c-comment-prefix-regexp' doesn't match the comment prefix %S"
                       (car fill)))
            ;; Find the right spot on the line, break it, insert
            ;; the fill prefix and make sure we're back in the
            ;; same column by temporarily prefixing the first word
            ;; with a number of 'x'.
            (save-excursion
              (goto-char (car c-lit-limits))
              (if (looking-at (if (eq c-lit-type 'c++)
                                  c-current-comment-prefix
                                comment-start-skip))
                  (goto-char (match-end 0))
                (forward-char 2)
                (skip-chars-forward " \t"))
              (while (and (< (current-column) (cdr fill))
                          (not (eolp)))
                (forward-char 1))
              (let ((col (current-column)))
                (setq beg (1+ (point))
                      tmp-pre (list (point)))
                (unwind-protect
                    (progn
                      (insert-and-inherit "\n" (car fill))
                      (insert-char ?x (- col (current-column)) t))
                  (setcdr tmp-pre (point))))))

          (when apply-outside-literal
            ;; `apply-outside-literal' is always set to t here if
            ;; we're inside a literal.

            (let ((fill-prefix
                   (or fill-prefix
                       ;; Kludge: If the function that adapts the fill prefix
                       ;; doesn't produce the required comment starter for
                       ;; line comments, then force it by setting fill-prefix.
                       (when (and (eq c-lit-type 'c++)
                                  ;; Kludge the kludge: filladapt-mode doesn't
                                  ;; have this problem, but it currently
                                  ;; doesn't override fill-context-prefix
                                  ;; (version 2.12).
                                  (not (and (boundp 'filladapt-mode)
                                            filladapt-mode))
                                  (not (string-match
                                        "\\`[ \t]*//"
                                        (or (fill-context-prefix beg end)
                                            ""))))
                         (c-save-buffer-state nil
                           (car (or fill (c-guess-fill-prefix
                                          c-lit-limits c-lit-type)))))))

                  ;; Save the relative position of point if it's outside the
                  ;; region we're going to narrow.  Want to restore it in that
                  ;; case, but otherwise it should be moved according to the
                  ;; called function.
                  (point-rel (cond ((< (point) beg) (- (point) beg))
                                   ((> (point) end) (- (point) end)))))

              ;; Preparations finally done!  Now we can call the
              ;; actual function.
              (prog1
                  (save-restriction
                    (narrow-to-region beg end)
                    (apply fun args))
                (if point-rel
                    ;; Restore point if it was outside the region.
                    (if (< point-rel 0)
                        (goto-char (+ beg point-rel))
                      (goto-char (+ end point-rel))))))))

      (when (consp tmp-pre)
        (delete-region (car tmp-pre) (cdr tmp-pre)))

      (when tmp-post
        (save-excursion
          (goto-char tmp-post)
          (delete-char 1))
        (when hang-ender-stuck
          ;; Preserve point even if it's in the middle of the string
          ;; we replace; save-excursion doesn't work in that case.
          (setq here (point))
          (goto-char tmp-post)
          (skip-syntax-backward "^w ")
          (forward-char (- hang-ender-stuck))
          (if (or fill-paragraph (not auto-fill-spaces))
              (insert-char ?\  hang-ender-stuck t)
            (insert auto-fill-spaces))
          (delete-char hang-ender-stuck)
          (goto-char here))
        (set-marker tmp-post nil))

      (set-marker end nil))))


(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment, fill the
comment or the paragraph of it that point is in, preserving the
comment indentation or line-starting decorations (see the
`c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
details).

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

char description[] = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let ((fill-paragraph-function
         ;; Avoid infinite recursion.
         (if (not (eq fill-paragraph-function 'c-fill-paragraph))
             ;; dino - BEGIN PATCH

    ;;          fill-paragraph-function)))
    ;; (c-mask-paragraph t nil 'fill-paragraph arg))

            fill-paragraph-function))
       (start-point (point-marker)))
     (c-mask-paragraph
      t nil (lambda () (fill-region-as-paragraph (point-min) (point-max) arg)))
     (goto-char start-point))

             ;; dino - END PATCH

  ;; Always return t.  This has the effect that if filling isn't done
  ;; above, it isn't done at all, and it's therefore effectively
  ;; disabled in normal code.
  t)

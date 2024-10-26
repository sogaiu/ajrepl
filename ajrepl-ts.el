;;; ajrepl-ts.el --- Tree-sitter-based features

;;; Commentary:

;;; Code:

(require 'ajrepl-core)
(require 'treesit)

(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defun ajrepl--ts-outside-top-level-exprs-p (&optional pos)
  "Check if not inside any top-level expression.

Optional argument `POS' specifies the point to check.  If unspecified
the value of point is used for checking.

Returns t or nil corresponding to the result of checking."
  (let* ((target-pos (or pos (point)))
         ;; XXX: not sure how much this can be relied on -- see docstring
         (node (treesit-node-on target-pos target-pos)))
    (string= "source" (treesit-node-type node))))

(defun ajrepl--ts-comment-form-p (node)
  "Check if `NODE' represents a comment form."
  (and (string= "par_tup_lit" (treesit-node-type node))
       (let ((head-node (treesit-node-child node 0 :named)))
         (and (string= "sym_lit" (treesit-node-type head-node))
              (string= "comment" (treesit-node-text head-node))))))

;; XXX: when cursor in the comment symbol that's at the head of
;;      a comment form, cursor will move to the beginning of the
;;      comment symbol.  is this a problem?
(defun ajrepl-ts-climb-to-ceiling ()
  "Move point to an appropriate ceiling.

The primary motivation for this function is to establish the
starting bounds of a sensible expression to evaluate.  As
evaluating a comment form produces nil, we don't want to move up
that far if one of point's ancestors happens to be a comment
form.

A ceiling is a location that contributes to a sensible
evaluation.  It is either:

1. the beginning of a top-level expression that contains point if
   none of point's ancestors are comment forms, or

2. the top-most containing expression of point which is a child
   of the closest ancestor comment form

This latter condition is a convoluted way of saying that point is
not moved beyond (or up to) any containing comment forms.

If point is not within a top-level expression, an error is raised."
  (interactive)
  ;; the intention of the check below is to distinguish between two
  ;; edge cases and act differently depending on what is encountered:
  ;;
  ;; 1) point is immediately after a top-level form, though on the
  ;;    same line as the last character of the expression,
  ;;
  ;; 2) point is on a "blank" line outside of all top-level
  ;;    expressions
  ;;
  ;; the check is intended to block case 2 but allow case 1.
  (when (and (ajrepl--ts-outside-top-level-exprs-p)
             (ajrepl-line-empty-p))
    (error "Point is not within a top-level expression"))
  (let ((ceiling-node
         (treesit-parent-until
          (treesit-node-at (point))
          (lambda (node)
            (or (when-let ((parent (treesit-node-parent node)))
                  (or (string= "source" (treesit-node-type parent))
                      (ajrepl--ts-comment-form-p parent)))
                (ajrepl--ts-comment-form-p node)))
          'include-node)))
    (goto-char (treesit-node-start
                (if (treesit-node-check ceiling-node 'named)
                    ceiling-node
                  (treesit-node-parent ceiling-node))))))

;; XXX: this is not quite as nice as `ajrepl-send-expression-at-point'
;;      for the case where point is on a line with a comment that is
;;      outside of all top-level forms.
(defun ajrepl-ts-send-expr-dwim ()
  "Send intended expression.

Sends a sensible outer-most expression that contains point.

1. If none of point's ancestor expressions are `(comment ...)'
   forms, send the top-level expression containing point.

2. If there is at least one ancestor `(comment ...)' form
   containing point, send the outer-most expression that contains
   no `(comment ...)' forms and still contains point.

3. If point is outside of all top-level expressions, sits after a
   top-level expression, and is on the last line of that
   expression, the preceding top-level expression is sent.  This
   matches the behavior of Emacs' `eval-defun'.

In all other cases, do nothing.

Note that case 2 is basically about trying to avoid sending
`(comment ...)'  forms which would just result in nil.

Note also that Emacs' `eval-defun' behaves differently when point
is outside of all top-level expression, on a blank line, and
there is a following \"defun\".  In such a case, the \"defun\" is
evaluated.  This behavior seemed strange and is not emulated
here."
  (interactive)
  (save-excursion
    (ajrepl-ts-climb-to-ceiling)
    (let* ((beg (point))
           (beg-node (treesit-node-at beg))
           (end (treesit-node-end (if (treesit-node-check beg-node 'named)
                                      beg-node
                                    (treesit-node-parent beg-node)))))
      (ajrepl-send-region beg end))))

(provide 'ajrepl-ts)

;;; ajrepl-ts.el ends here

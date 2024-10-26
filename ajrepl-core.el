;;; ajrepl-core.el --- core for ajrepl -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20201226
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;;; Acknowledgments:

;; Ruin0x11's janet-mode

;;;; Requirements

(require 'comint)
(require 'subr-x)

;;;; The Rest

(defgroup ajrepl nil
  "Code for interacting with Janet via a REPL."
  :prefix "ajrepl-"
  :group 'applications)

(defvar ajrepl-repl-buffer-name "*ajrepl-repl*"
  "Name of repl buffer.")

(defvar ajrepl-prompt "repl:[^>]+> "
  "Regular expression to match Janet repl prompt.")

(defun ajrepl-get-process ()
  "Return Janet process for repl buffer."
  (get-buffer-process ajrepl-repl-buffer-name))

;; XXX: trailing newlines in combination with telling comint not to
;; send a newline appended to the input can lead to comint-simple-send
;; sending eof (it appears that rms committed that code in 2002, so at
;; this point, no one may know why it's there).  this can result in
;; unexpected ending of one's repl session. to avoid such issues, if
;; there is a trailing newline for one's input, trim it and rely on
;; comint to append a newline
(defun ajrepl-trim-trailing-newline-maybe (a-str)
  "If A-STR has a trailing newline, trim it."
  (if (and (not (string-empty-p a-str))
           (string-equal "\n" (substring a-str -1)))
      (substring a-str 0 -1)
    a-str))

(ignore

 (ajrepl-trim-trailing-newline-maybe "hello\n")
 ;; =>
 "hello"

 (ajrepl-trim-trailing-newline-maybe "")
 ;; =>
 ""

 (ajrepl-trim-trailing-newline-maybe "hello")
 ;; =>
 "hello"

 )

(defun ajrepl-send-code (code-str)
  "Send CODE-STR to Janet repl."
  (interactive "sCode: ")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer ajrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajrepl-repl-buffer-name))
      ;; switch to ajrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (setq code-str
            (ajrepl-trim-trailing-newline-maybe code-str))
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

(defun ajrepl-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer ajrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajrepl-repl-buffer-name))
      ;; switch to ajrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      ;; switch back
      (set-buffer original-buffer)
      (let ((code-str (ajrepl-trim-trailing-newline-maybe
                       (buffer-substring-no-properties start end))))
        (set-buffer repl-buffer)
        (insert code-str)
        (comint-send-input)
        (set-buffer original-buffer)
        (goto-char here)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ajrepl-send-string (string &optional process)
  "Send STRING to Janet PROCESS."
  (interactive
   (list (read-string "Eval: ") nil t))
  (let ((process (or process (ajrepl-get-process))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ajrepl-line-empty-p (&optional line)
  "Determine if `LINE` is empty.

If `LINE' is not given, uses the line number that point is on."
  (save-excursion
    (let* ((current-line (line-number-at-pos))
           (target-line (or line current-line)))
      ;; side-effect of `forward-line' seems to be that point is at bol
      (forward-line (- target-line current-line))
      (looking-at-p "[[:blank:]]*$"))))

(defun ajrepl-at-depth (&optional pos)
  "Return the depth of `POS'.

If `POS' is not given, uses point.

Note that the depth of the opening delimiter of a top-level
expression is 0 unlike that of some of the following
characters (e.g. the closing delimiter is at depth 1), one less
than might be expected.  A similar thing applies for additional
depths.

See `parse-partial-sexp' for further details."
  (let ((target-pos (or pos (point))))
    (save-excursion
      (nth 0 (syntax-ppss target-pos)))))

(defun ajrepl-in-string-p (&optional pos)
  "Test if `POS' is in a string.

If `POS' is not given, uses point.

See `parse-partial-sexp' for further details."
  (let ((target-pos (or pos (point))))
    (save-excursion
      (nth 3 (syntax-ppss target-pos)))))

(defun ajrepl-in-comment-p (&optional pos)
  "Test if `POS' is in a comment.

If `POS' is not given, uses point.

See `parse-partial-sexp' for further details."
  (let ((target-pos (or pos (point))))
    (save-excursion
      (nth 4 (syntax-ppss target-pos)))))

(defun ajrepl-start-of-comment-or-string (&optional pos)
  "Find start of comment or string containg `POS'.

If `POS' is not given, uses point.

See `parse-partial-sexp' for further details."
  (let ((target-pos (or pos (point))))
    (save-excursion
      (nth 8 (syntax-ppss target-pos)))))

(defun ajrepl-ancestor-open-delims-pos (&optional pos)
  "Find open delimiter positions of ancestors containing `POS'.

If `POS' is not given, uses point.

If there are none, return nil.

See `parse-partial-sexp' for further details."
  (let ((target-pos (or pos (point))))
    (save-excursion
      (nth 9 (syntax-ppss target-pos)))))

(defun ajrepl-backward-up ()
  "Janet-specific simplified version of `backward-up-list'.

Treats arrays, tuples, tables, and structs as containers to
climb."
  (let ((cur-pos (point))
        (target-pos nil))
    (save-excursion
      ;; ensure not within a string or comment
      (when (or (ajrepl-in-string-p cur-pos)
                (ajrepl-in-comment-p cur-pos))
        (goto-char (ajrepl-start-of-comment-or-string cur-pos))
        ;; XXX: uncertain of this behavior -- it matches `backward-up-list'
        ;;      but is that wanted?
        (setq target-pos (point)))
      ;; check for any containing forms
      (when (ajrepl-ancestor-open-delims-pos (point))
        (backward-up-list)
        (setq target-pos (point))))
    (when target-pos
      (goto-char target-pos))))

;; XXX: (comment "hello") ; <- things like this not handled
(defun ajrepl-backward-up-to-non-comment-form ()
  "Climb up to the first outer-most non-comment form container.

The word \"first\" should be interpreted as counting from where
point is initially and successively \"upwards\".

If applicable, the function repeatedly climbs up to the next
ancestor container checking for a stopping condition.  After each
climb, the current container form is examined to see whether it
is a `(comment ...)' form.  If it is, this means the target
destination has been passed so point is ultimately moved to the
start of the previous container form (so a child of the current
one).

Above \"start of the previous container form\" may not be what
one expects in the case of tables and arrays (which start with
at-marks) and forms that have leading \"reader macros\" such as a
tilde, single quote, pipe, semi-colon, or comma.  This is because
the underlying machinery for parsing -- `parse-partial-sexp' --
only yields certain kinds of positions, typically the last
character in a delimiters sequence (e.g. for @{}, the left curly
brace position), that is, not quite to the starting
position (which would be the at-mark for @{}).

In general, the existence of a suitable target location is not
guaranteed.  In such a case, point is not moved."
  (interactive)
  (let* ((cur-pos (point))
         (delims-pos (reverse (ajrepl-ancestor-open-delims-pos cur-pos)))
         (cur-cand nil)
         (last-cand nil)
         (found-comment nil))
    (when delims-pos
      (save-excursion
        (while (and delims-pos (not found-comment))
          (setq cur-cand (nth 0 delims-pos))
          (goto-char cur-cand)
          (if (looking-at "(comment" 'inhibit-modify)
              (setq found-comment t)
            (setq last-cand cur-cand)
            (setq delims-pos (cdr delims-pos)))))
      (goto-char last-cand))))

(defun ajrepl-back-to-start-of-expr ()
  "Move point to the start of an expression.

`backward-sexp' does not appear to appropriately account for
quotes and other things that precede things if they have spaces
after them, e.g. for:

  [' '[:a :b :c]]

starting at the right-most square bracket, invoking
`backward-sexp' should bring point to the left-most single quote,
but instead it ends up at the right single quote.

If point is within a string or line comment, `backward-sexp' also
doesn't treat either as opaque and move to the leading character
\(e.g. double quote or semi-colon), but rather seems to treat both
as having internal structure.  This behavior is not emulated
here.

This function attempts to provide backward motion appropriately."
  (interactive)
  (let ((cur-pos (point))
        (cur-char nil)
        (sure-pos nil))
    ;; initial backwards motion
    (if (or (ajrepl-in-string-p cur-pos)
            (ajrepl-in-comment-p cur-pos))
        ;; strings, buffers, comments
        (goto-char (ajrepl-start-of-comment-or-string cur-pos))
      ;; numbers, symbols, keywords, booleans, nil
      (backward-sexp))
    ;; account for leading @ if any
    (when (not (bobp))
      (when (eql ?@ (char-after (1- (point))))
        (backward-char)))
    ;; keep moving back if needed and note characters that we are
    ;; "sure" out part of our expression
    (setq cur-pos (point))
    (setq sure-pos cur-pos)
    (let ((done nil))
      (while (and (not (bobp)) (not done))
        (setq cur-pos (1- cur-pos))
        (setq cur-char (char-after cur-pos))
        ;; see "Basic Char Syntax" chapter for details
        (cond ((memql cur-char '(?\s ?\t ?\r ?\n))
               (goto-char cur-pos))
              ((memql cur-char '(?~ ?' ?| ?, ?\;))
               (setq sure-pos cur-pos)
               (goto-char cur-pos))
              (t
               (setq done t))))
      ;; move point to the determined start of the expression
      (goto-char sure-pos))))

(provide 'ajrepl-core)

;;; ajrepl-core.el ends here

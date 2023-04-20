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

(provide 'ajrepl-core)

;;; ajrepl-core.el ends here

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
  "A Janet REPL"
  :prefix "ajrepl-"
  :group 'applications)

(defvar ajrepl-repl-buffer-name "*ajrepl-repl*"
  "Name of repl buffer.")

(defvar ajrepl-prompt "repl:[^>]+> "
  "Regular expression to match Janet repl prompt.")

(defun ajrepl-get-process ()
  "Return Janet process for repl buffer."
  (get-buffer-process ajrepl-repl-buffer-name))

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
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ajrepl-output-filter-in-progress nil)
(defvar ajrepl-output-filter-buffer nil)

(defun ajrepl-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match (concat "\r?\n?" ; \r for macos
                        ajrepl-prompt
                        (rx eos))
                output))

(defun ajrepl-output-filter (string)
  "Filter used in `ajrepl-send-code-async' to capture output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`ajrepl-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq ajrepl-output-filter-buffer
        (concat ajrepl-output-filter-buffer
                (ansi-color-filter-apply string)))
  (when (ajrepl-comint-end-of-output-p ajrepl-output-filter-buffer)
    ;; end of output marked by prompt in `ajrepl-output-filter-buffer'
    (setq ajrepl-output-filter-in-progress nil)
    (setq ajrepl-output-filter-buffer
          (substring ajrepl-output-filter-buffer
                     ;; `ajrepl-comint-end-of-output-p' uses `string-match'
                     0 (match-beginning 0))))
  ;; don't let anything through to repl buffer
  "")

(defun ajrepl-send-string (string &optional process)
  "Send STRING to Janet PROCESS."
  (interactive
   (list (read-string "Eval: ") nil t))
  (let ((process (or process (ajrepl-get-process))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

;; XXX: unsure if using the same process is a good idea
;; XXX: don't send "multiline" strings -- don't currently have a way to
;;      "clean up" / "remove" the intermediate prompts
;; XXX: sending code that generates output (e.g. on stdout or stderr)
;;      might confuse things too?
(defun ajrepl-send-code-async (code-str &optional process)
  "Asynchronously send CODE-STR to Janet PROCESS."
  (let ((process (or process (ajrepl-get-process)))
        (comint-preoutput-filter-functions '(ajrepl-output-filter))
        (ajrepl-output-filter-in-progress t)
        (inhibit-quit t))
    (or (with-local-quit
          (ajrepl-send-string code-str process)
          ;; when `ajrepl-output-filter' detects end of output, it
          ;; sets `ajrepl-output-filter-in-progress' to nil
          ;; XXX: this could end up as an infinite loop if the process
          ;;      doesn't end up sending a prompt
          (while ajrepl-output-filter-in-progress
            (accept-process-output process))
          (prog1
              ajrepl-output-filter-buffer
            (setq ajrepl-output-filter-buffer nil)))
        (with-current-buffer (process-buffer process)
          (comint-interrupt-subjob)))))

(provide 'ajrepl-core)

;;; ajrepl-core.el ends here

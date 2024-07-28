;;; ajrepl.el --- A Janet REPL -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20201226
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Janet REPL - simple janet repl interaction and editor functionality

;;;; Installation

;;;;; Manual

;;  Add the directory this file is in to your load-path.
;;
;;  Put this in your relevant init file:
;;
;;    (require 'ajrepl)
;;
;;  Optionally, add:
;;
;;    (add-hook 'janet-mode-hook
;;              #'ajrepl-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  janet code in it.
;;
;;  If you use, a-janet-mode, you can add the following instead:
;;
;;    (add-hook 'a-janet-mode-hook
;;              #'ajrepl-interaction-mode)

;;;;; Automatic

;; TODO :)

;;;; Usage

;; 0. Open a Janet file

;; 1. Start an interactive REPL for Janet by:
;;
;;      M-x ajrepl
;;
;;    A buffer for a Janet repl should appear.

;; 2. For editor features, in a relevant buffer with a Janet source file:
;;
;;      M-x ajrepl-interaction-mode
;;
;;    There should be an Ajrepl menu containing some convenience commands:
;;
;;      Send buffer
;;      Send expression at point
;;      Send region
;;
;;      Insert last output
;;
;;      Start REPL
;;      Switch to REPL

;;;;; Acknowledgments

;; Thanks to those involved in:
;;
;;   emacs
;;   janet
;;
;; and transitively involved folks too ;)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'comint)
(require 'ajrepl-core)

;;;; The Rest

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

(defun ajrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (ajrepl-send-region (point-min) (point-max)))

;; XXX: better to support more than just paren expressions -- e.g. symbols
(defvar ajrepl--helper-path
  (expand-file-name
   (concat (expand-file-name
            (file-name-directory (or load-file-name
                                     buffer-file-name)))
           "ajrepl/"
           "janet-last-expression/"
           "janet-last-expression/"
           "last-expression.janet"))
  "Path to helper program to determine last paren expression.")

(defvar ajrepl--repl-helper-path
  (expand-file-name
   (concat (expand-file-name
            (file-name-directory (or load-file-name
                                     buffer-file-name)))
           "ajrepl/"
           "custom-repl.janet"))
  "Path to helper program for repl customization.")

(defvar ajrepl--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar ajrepl--temp-buffers
  '()
  "List of buffers to clean up before executing `ajrepl--helper'.")

(defvar ajrepl--run-under-gdb
  nil
  "If non-nil, start janet binary under the control of gdb.

Requires the janet binary be built with debug symbols.")

(defun ajrepl--helper (start end)
  "Determine last paren expression by asking Janet.

A region bounded by START and END is sent to a helper program."
  (interactive "r")
  (condition-case err
      (let ((temp-buffer (generate-new-buffer "*ajrepl-helper*"))
            (result nil))
        (dolist (old-buffer ajrepl--temp-buffers)
          (kill-buffer old-buffer))
        (add-to-list 'ajrepl--temp-buffers temp-buffer)
        (save-excursion
          (when ajrepl--debug-output
            (message "region: %S"
                     (buffer-substring-no-properties start end)))
          (call-process-region start end
                               "janet"
                               ;; https://emacs.stackexchange.com/a/54353
                               nil `(,temp-buffer nil) nil
                               ajrepl--helper-path)
          (set-buffer temp-buffer)
          (setq result
                (buffer-substring-no-properties (point-min) (point-max)))
          (when ajrepl--debug-output
            (message "ajrepl: %S" result))
          ;; https://emacs.stackexchange.com/a/14599
          (if (string-match "^[\0-\377[:nonascii:]]*" result)
              (match-string 0 result)
            (message "Unexpected result - source ok? <<%s>>" result)
            nil)))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

(defun ajrepl--start-of-top-level-char-p (char)
  "Return non-nil if CHAR can start a top level container construct.

Supported top level container constructs include:

  * paren tuple            ()
  * quoted constructs      \\='() ~()

Note that constructs such as numbers, keywords, and symbols are excluded."
  (member char '(?\( ?\~ ?\')))

(defun ajrepl--column-zero-target-backward ()
  "Move backward to the closest column zero target.

Does not move point if there is no such construct.

See `ajrepl--start-of-top-level-char-p' for which characters determine
a column zero target."
  (when (not (bobp))             ; do nothing if at beginning of buffer
    (let ((pos (point)))
      ;; only consider positions before the starting point
      (if (bolp)
          (forward-line -1)
        (beginning-of-line))
      (if (ajrepl--start-of-top-level-char-p (char-after (point)))
          (setq pos (point))
        (let ((done nil))
          (while (not done)
            (forward-line -1)
            (cond ((ajrepl--start-of-top-level-char-p
                    (char-after (point)))
                   (setq pos (point))
                   (setq done t))
                  ((bobp)
                   (setq done t))))))
      (goto-char pos))))

(defun ajrepl-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (save-excursion
    (let ((end (point)))
      ;; XXX: if skipping backward over comments was easy, that might be
      ;;      even nicer
      ;;(skip-chars-backward " \t\n")
      ;; XXX: cheap version
      (save-excursion
        (skip-chars-backward " \t\n")
        (beginning-of-line)
        (when (looking-at "[ \t]*#")
          (setq end (point))))
      (when-let ((beg (ajrepl--column-zero-target-backward)))
        (when-let ((code (ajrepl--helper beg end)))
          (ajrepl-send-code code))))))

;; XXX: improve with treesitter?
(defun ajrepl-send-expression-upscoped ()
  "Send expression at point wrapped in (upscope ... :done).

This is to avoid copious output from evaluating certain forms."
  (interactive)
  (save-excursion
    (let ((end (point)))
      ;; XXX: if skipping backward over comments was easy, that might be
      ;;      even nicer
      ;;(skip-chars-backward " \t\n")
      ;; XXX: cheap version
      (save-excursion
        (skip-chars-backward " \t\n")
        (beginning-of-line)
        (when (looking-at "[ \t]*#")
          (setq end (point))))
      (when-let ((beg (ajrepl--column-zero-target-backward)))
        (when-let ((code (ajrepl--helper beg end)))
          (ajrepl-send-code (format "(upscope\n%s\n:done)" code)))))))

(defun ajrepl-switch-to-repl ()
  "Switch to the repl buffer named by `ajrepl-repl-buffer-name`."
  (interactive)
  (pop-to-buffer ajrepl-repl-buffer-name))

;; XXX: assumes that output from process does not contain strings that match
;;      repl prompt
;;
;; XXX: in some cases comint-last-output-start was reporting incorrect
;;      values.  the current approach (relying on searching for ajrepl-prompt)
;;      tries to work around that.
(defun ajrepl-insert-last-output ()
  "Insert last evaluation result."
  (interactive)
  ;; XXX: temporary measure to avoid problems?
  (if (eq last-command 'ajrepl-send-buffer)
      (message "Sorry, this doesn't work right after ajrepl-send-buffer.")
    (let ((original-buffer (current-buffer))
          (repl-buffer (get-buffer ajrepl-repl-buffer-name))
          (last-output ""))
      (if (not repl-buffer)
          (message (format "%s is missing..." ajrepl-repl-buffer-name))
        ;; switch to ajrepl buffer to prepare for appending
        (set-buffer repl-buffer)
        (save-excursion
          (let ((start nil)
                (multiline nil))
            (goto-char (point-max))
            (when (and (re-search-backward ajrepl-prompt)
                       (re-search-backward ajrepl-prompt))
              (setq multiline
                    (not (looking-at "repl:[0-9]+:>" :inhibit-modify)))
              (when (re-search-forward ajrepl-prompt)
                (when (not multiline)
                  ;; XXX: might not work for all cases...keep an eye out
                  (forward-sexp))
                (setq start (point))
                (when (and (re-search-forward ajrepl-prompt)
                           (re-search-backward ajrepl-prompt))
                  (setq last-output
                        (string-trim
                         (buffer-substring-no-properties start (point)))))))))
        (set-buffer original-buffer)
        (if (not last-output)
            (message "Did not identify last output")
          (insert last-output))))))

;; XXX: hacky because of the waiting
(defun ajrepl-insert-rest-of-usage ()
  "Insert rest of usage."
  (interactive)
  (newline-and-indent)
  (insert "# =>")
  (newline-and-indent)
  (ajrepl-send-expression-at-point)
  ;; XXX
  (sit-for 0.1)
  (ajrepl-insert-last-output))

(defvar ajrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'ajrepl-send-buffer)
    (define-key map "\C-x\C-e" 'ajrepl-send-expression-at-point)
    (define-key map "\C-c\C-u" 'ajrepl-send-expression-upscoped)
    (define-key map "\C-c\C-r" 'ajrepl-send-region)
    (define-key map "\C-c\C-i" 'ajrepl-insert-last-output)
    (define-key map "\C-c\C-c" 'ajrepl-insert-rest-of-usage)
    (define-key map "\C-c\C-z" 'ajrepl-switch-to-repl)
    (easy-menu-define ajrepl-interaction-mode-map map
      "A Janet REPL Interaction Mode Menu"
      '("Ajrepl"
        ["Send buffer" ajrepl-send-buffer t]
        ["Send expression at point" ajrepl-send-expression-at-point t]
        ["Send expression upscoped" ajrepl-send-expression-upscoped t]
        ["Send region" ajrepl-send-region t]
        "--"
        ["Insert last output" ajrepl-insert-last-output t]
        ["Insert rest of usage" ajrepl-insert-rest-of-usage t]
        "--"
        ["Start REPL" ajrepl t]
        ["Switch to REPL" ajrepl-switch-to-repl t]))
    map)
  "Ajrepl interaction mode map.")

(defvar ajrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (easy-menu-define ajrepl-mode-map map
          "A Janet REPL Mode Menu"
          '("Ajrepl"
            ["Switch to other window" other-window t]))
    map)
  "Ajrepl mode map.")

(define-derived-mode ajrepl-mode comint-mode "A Janet REPL"
  "Major mode for ajrepl.

\\{ajrepl-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq mode-line-process '(":%s")))

;;;###autoload
(define-minor-mode ajrepl-interaction-mode
  "Minor mode for ajrepl interaction from a buffer with Janet code.

The following keys are available in `ajrepl-interaction-mode`:

\\{ajrepl-interaction-mode}"
  :init-value nil
  :lighter " ajrepl"
  :keymap ajrepl-interaction-mode-map)

;;;###autoload
(defun ajrepl ()
  "Start ajrepl."
  (interactive)
  (let ((start-buffer (current-buffer))
        ;; XXX: work-around
        (better-dir default-directory))
    (unless
        ;;(ignore-errors ;; XXX: uncomment at some point...
        (with-current-buffer (get-buffer-create ajrepl-repl-buffer-name)
          ;; XXX: work-around
          (setq default-directory better-dir)
          (prog1
              (if ajrepl--run-under-gdb
                  (make-comint-in-buffer "ajrepl" ajrepl-repl-buffer-name
                                         "gdb" nil
                                         "--quiet"
                                         "--eval-command=run"
                                         "--args" "janet" "-s")
                (make-comint-in-buffer "ajrepl" ajrepl-repl-buffer-name
                                       "janet" nil "-s"
                                       ;"janet" nil ajrepl--repl-helper-path
                                       ))
            (goto-char (point-max))
            (ajrepl-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            (pop-to-buffer start-buffer)))
      (message "Failed to connect to janet"))))

(provide 'ajrepl)

;;; ajrepl.el ends here

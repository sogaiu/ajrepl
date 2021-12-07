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
      (append-to-buffer repl-buffer start end)
      (set-buffer repl-buffer)
      (comint-send-input)
      (set-buffer original-buffer)
      (goto-char here))))

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
	   "ajrepl/last-expression.janet"))
  "Path to helper program to determine last paren expression.")

(defvar ajrepl--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar ajrepl--temp-buffers
  '()
  "List of buffers to clean up before executing `ajrepl--helper'.")

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
  * quoted constructs      '() ~()

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

(defun ajrepl-switch-to-repl ()
  "Switch to the repl buffer named by `ajrepl-repl-buffer-name`."
  (interactive)
  (pop-to-buffer ajrepl-repl-buffer-name))

(defun ajrepl-repl-buffer-new-frame ()
  "Create a new frame and switch to the repl buffer in it."
  (interactive)
  (select-frame-set-input-focus (make-frame-command))
  (switch-to-buffer (get-buffer ajrepl-repl-buffer-name)))

(defun ajrepl-set-pretty-format ()
  "Set :pretty-format to multiline."
  (interactive)
  (ajrepl-send-code "(setdyn :pretty-format \"%.20M\")"))

(defun ajrepl-insert-last-output ()
  "Insert last evaluation result."
  (interactive)
  (let ((original-buffer (current-buffer))
        (repl-buffer (get-buffer ajrepl-repl-buffer-name))
        (last-output ""))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajrepl-repl-buffer-name))
      ;; switch to ajrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (setq last-output
            (buffer-substring-no-properties comint-last-input-end
                                            (nth 0 comint-last-prompt)))
      (set-buffer original-buffer)
      (insert last-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX: should the string argument be sanity-checked?
;; (defun ajrepl-company-candidates (string)
;;   "Get candidates for completion of STRING."
;;   (when (ajrepl-get-process)
;;     (when-let* ((code-str
;;                  (format
;;                   (concat "(filter "
;;                           "  (fn [name] "
;;                           "    (string/has-prefix? \"%s\" (string name)))"
;;                           "  (keys root-env))")
;;                   string))
;;                 (candidates (ajrepl-send-code-async code-str)))
;;       ;; XXX: consider expressing via rx
;;       (when (string-match "@\\[\\(.*\\)\\]" candidates)
;;         (when-let* ((beg (nth 2 (match-data)))
;;                     (end (nth 3 (match-data)))
;;                     (spaced (substring candidates beg end)))
;;           (split-string spaced " "))))))

;; (defun company-ajrepl (command &optional arg &rest ignored)
;;   "Integration into company for ajrepl.
;; COMMAND, ARG, IGNORED are part of the standard signature."
;;   (interactive (list 'interactive))
;;   (cl-case command
;;     (prefix (company-grab-symbol)) ; XXX: change to tree-sitter-based later?
;;     (candidates (ajrepl-company-candidates arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ajrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'ajrepl-send-buffer)
    (define-key map "\C-x\C-e" 'ajrepl-send-expression-at-point)
    (define-key map "\C-c\C-r" 'ajrepl-send-region)
    (define-key map "\C-c\C-i" 'ajrepl-insert-last-output)
    (define-key map "\C-c\C-n" 'ajrepl-repl-buffer-new-frame)
    (define-key map "\C-c\C-z" 'ajrepl-switch-to-repl)
    (easy-menu-define ajrepl-interaction-mode-map map
      "A Janet REPL Interaction Mode Menu"
      '("Ajrepl"
        ["Send buffer" ajrepl-send-buffer t]
        ["Send expression at point" ajrepl-send-expression-at-point t]
        ["Send region" ajrepl-send-region t]
        "--"
        ["Insert last output" ajrepl-insert-last-output t]
        "--"
        ["Start REPL" ajrepl t]
        ["Multiline Formatting" ajrepl-set-pretty-format t]
        ["New Frame with REPL" ajrepl-repl-buffer-new-frame t]
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
  (setq mode-line-process '(":%s")))

;;;###autoload
(define-minor-mode ajrepl-interaction-mode
  "Minor mode for ajrepl interaction from a lisp buffer.

The following keys are available in `ajrepl-interaction-mode`:

\\{ajrepl-interaction-mode}"

  nil " ajrepl" ajrepl-interaction-mode-map)

;;;###autoload
(defun ajrepl ()
  "Start ajrepl."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (unless
        ;;(ignore-errors ;; XXX: uncomment at some point...
        (with-current-buffer (get-buffer-create ajrepl-repl-buffer-name)
          (prog1
              (make-comint-in-buffer "ajrepl" ajrepl-repl-buffer-name
                                     "janet" nil "-s")
            (goto-char (point-max))
            (ajrepl-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            (pop-to-buffer start-buffer)))
      (message "Failed to connect to janet"))))

(provide 'ajrepl)

;;; ajrepl.el ends here

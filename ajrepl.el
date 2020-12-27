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
;;  janet code in it

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
;;   tree-sitter
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
(require 'subr-x)

;;;; The Rest

(defgroup ajrepl nil
  "A Janet REPL"
  :prefix "ajrepl-"
  :group 'applications)

(defvar ajrepl-repl-buffer-name "*ajrepl-repl*"
  "Name of repl buffer.")

(defun ajrepl-switch-to-repl ()
  "Switch to the repl buffer named by `ajrepl-repl-buffer-name`."
  (interactive)
  (pop-to-buffer ajrepl-repl-buffer-name))

(defun ajrepl-send-code (code-str)
  "Send CODE-STR, a Lisp form."
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

(defun ajrepl-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (start (car bounds))
         (end (cdr bounds)))
    (when (and start end)
      (ajrepl-send-region start end))))

(defun ajrepl-insert-last-output ()
  "Insert last evaluation result."
  (interactive)
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer ajrepl-repl-buffer-name))
        (last-output ""))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajrepl-repl-buffer-name))
      ;; switch to ajrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (setq last-output
            (buffer-substring-no-properties comint-last-input-end
                                            (first comint-last-prompt)))
      (set-buffer original-buffer)
      (insert last-output))))

(defvar ajrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'ajrepl-send-buffer)
    (define-key map "\C-x\C-e" 'ajrepl-send-expression-at-point)
    (define-key map "\C-c\C-r" 'ajrepl-send-region)
    (define-key map "\C-c\C-i" 'ajrepl-insert-last-output)
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

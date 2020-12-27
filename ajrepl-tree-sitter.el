;;; ajrepl-tree-sitter.el --- arepl tree-sitter bits -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20201227
;; Package-Requires: ((smartparens "1.11.0") (emacs "26.2") (emacs-tree-sitter))
;; Keywords: janet repl tree-sitter

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ajrepl bits that use tree-sitter

;;;; Installation

;;;;; Manual

;; 0. Ensure emacs-tree-sitter is installed (and tree-sitter-janet support)
;;
;; 1. Ensure ajrepl is setup (see ajrepl.el)
;;
;; 2. Put the following in the relevant init file:
;;
;;      (require 'ajrepl-tree-sitter)

;;;; Usage

;; 0. Ensure a Janet file is open and a connection to a REPL is made

;; 1. Put point on a symbol and invoke one of:
;;
;;    * `ajrepl-ts-doc'
;;    * `ajrepl-ts-doc-new-frame'

;;; Code:

(require 'tree-sitter)
(require 'ajrepl)

(defun ajrepl-ts-doc ()
  "Show docs for symbol at point."
  (interactive)
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let ((node (tree-sitter-node-at-point)))
      (when (and node
                 (eq (tsc-node-type node)
                     'symbol))
        (let ((node-text (tsc-node-text node)))
          (ajrepl-send-code (format "(doc %s)" node-text)))))))

(defun ajrepl-ts-doc-new-frame ()
  "Show docs for symbol at point in new frame."
  (interactive)
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let ((node (tree-sitter-node-at-point)))
      (when (and node
                 (eq (tsc-node-type node)
                     'symbol))
        (let ((node-text (tsc-node-text node)))
          (select-frame-set-input-focus (make-frame-command))
          (switch-to-buffer (get-buffer ajrepl-repl-buffer-name))
          (ajrepl-send-code (format "(doc %s)" node-text)))))))

(define-key-after
  ajrepl-interaction-mode-map
  [menu-bar Ajrepl ajrepl-ts-separator] ; XXX: need to keep changing the last element?
  '(menu-item "--"))

(define-key-after
  ajrepl-interaction-mode-map
  [menu-bar Ajrepl ajrepl-ts-show-doc-new-frame-item]
  '(menu-item "Show doc in new frame" ajrepl-ts-doc-new-frame))

(define-key ajrepl-interaction-mode-map
  "\C-c\C-d" 'ajrepl-ts-doc-new-frame)

(provide 'ajrepl-tree-sitter)
;;; ajrepl-tree-sitter ends here

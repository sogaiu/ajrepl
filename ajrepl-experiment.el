;;; ajrepl-experiment.el --- Experimental features  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ajrepl)

;; XXX: to get this to work, need to separate out all require, import, and use
;;      forms and send them first, then send remaining bits wrapped in upscope.
;;      possibly there's going to be code that doesn't work with this.
(defun ajrepl-send-region-upscoped (start end)
  "Send a region bounded by START and END wrapped in (upscope ...)."
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
        (insert "(upscope\n")
        (insert code-str)
        (insert "\n)")
        (comint-send-input)
        (set-buffer original-buffer)
        (goto-char here)))))

(defun ajrepl-send-buffer-upscoped ()
  "Send buffer content wrapped in (upscope ...)."
  (interactive)
  (ajrepl-send-region-upscoped (point-min) (point-max)))

;; XXX: thing-at-pt dependency
(defun ajrepl-send-expression-lengthed ()
  "Send expression at point wrapped in (length ...).

This is to ascertain the length of data."
  (interactive)
  ;; XXX: monitor if this doesn't work well for some janet things
  ;; XXX: have a region version?
  (ajrepl-send-code (format "(length %s)" (thing-at-point 'sexp))))

(defun ajrepl-repl-buffer-new-frame ()
  "Create a new frame and switch to the repl buffer in it."
  (interactive)
  (select-frame-set-input-focus (make-frame-command))
  (pop-to-buffer (get-buffer ajrepl-repl-buffer-name))
  (delete-other-windows))

(defun ajrepl-set-pretty-format ()
  "Set :pretty-format to multiline."
  (interactive)
  (ajrepl-send-code "(setdyn :pretty-format \"%.20M\")"))

(defun ajrepl-simplify-repl-prompt ()
  "Make :repl-prompt simpler."
  (interactive)
  (ajrepl-send-code
   (concat "(setdyn :repl-prompt\n"
           "  (fn [p]\n"
           "    (if (empty? (get (parser/state p) :delimiters))\n"
           "      `repl> `\n"
           "      \"\")))")))

(defun ajrepl-timestampify-repl-prompt ()
  "Make :repl-prompt use a timestamp."
  (interactive)
  (ajrepl-send-code
   (concat "(setdyn :repl-prompt\n"
           "  (fn [p]\n"
           "    (if (empty? (get (parser/state p) :delimiters))\n"
           "      (string \"\\n\"\n"
           "              (os/strftime \"%Y-%m-%d %H:%M:%S\\n\\n\"))\n"
           "      \"\")))")))

(defun ajrepl-reset-repl-prompt ()
  "Reset :repl-prompt."
  (interactive)
  (ajrepl-send-code "(setdyn :repl-prompt nil)"))

(defun ajrepl-redefine-comment-macro ()
  "Redefine comment macro."
  (interactive)
  (ajrepl-send-code
   (concat "(defmacro comment\n"
           "  [& args]\n"
           "  (when-let [head (first args)\n"
           "             _ (symbol? head)]\n"
           "    (tuple head ;(drop 1 args))))")))

(defun ajrepl-reset-comment-macro ()
  "Reset comment macro."
  (interactive)
  (ajrepl-send-code
   (concat "(defmacro comment\n"
           "  \"Ignores the body of the comment.\"\n"
           "  [&])")))

(defun ajrepl-insert-last-eval-result ()
  "Try to insert the last evaluation result at point.

If a standard janet repl prompt is in use, extraneous text may be
inserted when the sent expression is multiline.  To avoid such fates,
use one of the other prompts such as the simple one or the
timestampified one."
  (interactive)
  (save-excursion
    (let ((original-buffer (current-buffer)))
      ;; work inside repl buffer
      (set-buffer ajrepl-repl-buffer-name)
      (let ((start (marker-position comint-last-input-end))
            (proc (get-buffer-process (current-buffer))))
        (when (and start proc)
          (save-excursion
            (goto-char start)
            (when-let ((next-prompt (condition-case nil
                                        (comint-next-prompt 1)
                                      (error "comint-next-prompt failed"))))
              (when (> next-prompt start)
                (let ((inhibit-field-text-motion t)) ; prompt is "shielded"
                  (move-beginning-of-line nil))
                (let ((target
                       (buffer-substring-no-properties start (1- (point)))))
                  ;; insert captured text in appropriate buffer
                  (set-buffer original-buffer)
                  (insert target))))))))))

(defun ajrepl-complete-usage ()
  "Complete usage by inserting the last evaluation result nicely.

Assumes point is right after an expression that has just been evaluated.
Upon invocation, the following is inserted after point:

* a newline
* an indented \"# =>\" followed by another newline
* an indented evaluation result

See `ajrepl-insert-last-eval-result' concerning advice about avoiding
the use of a standard janet repl prompt."
  (interactive)
  (insert "\n")
  (indent-for-tab-command)
  (insert "# =>\n")
  (indent-for-tab-command)
  (insert "'")
  (ajrepl-insert-last-eval-result))

;; XXX: likely a better way to do this

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/ \
;;         Modifying-pull_002ddown-menus.html
;; https://emacs.stackexchange.com/questions/15093/ \
;;         how-to-add-an-item-to-the-menu-bar

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl sep-before-exp-features]
  '(menu-item "--"))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl rbnf-item]
  '("New Frame with REPL" . ajrepl-repl-buffer-new-frame))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl spf-item]
  '("Multiline Formatting" . ajrepl-set-pretty-format))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl rdcm-item]
  '("Redefine comment macro" . ajrepl-redefine-comment-macro))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl rscm-item]
  '("Reset comment macro" . ajrepl-reset-comment-macro))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl srp-item]
  '("Simplify repl prompt" . ajrepl-simplify-repl-prompt))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl trp-item]
  '("Timestampify repl prompt" . ajrepl-timestampify-repl-prompt))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl rrp-item]
  '("Reset repl prompt" . ajrepl-reset-repl-prompt))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl iler-item]
  '("Insert last eval result" . ajrepl-insert-last-eval-result))

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl cu-item]
  '("Complete usage" . ajrepl-complete-usage))

(provide 'ajrepl-experiment)

;;; ajrepl-experiment.el ends here

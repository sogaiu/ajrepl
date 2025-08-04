;;; ajrepl-experiment.el --- Experimental features

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
  [menu-bar ajrepl ilo-item]
  '("Insert last ouput" . ajrepl-insert-last-output))

(define-key ajrepl-interaction-mode-map "\C-c\C-i"
            'ajrepl-insert-last-output)

(define-key-after ajrepl-interaction-mode-map
  [menu-bar ajrepl irou-item]
  '("Insert rest of usage" . ajrepl-insert-rest-of-usage))

(define-key ajrepl-interaction-mode-map "\C-c\C-c"
            'ajrepl-insert-rest-of-usage)

(provide 'ajrepl-experiment)

;;; ajrepl-experiment.el ends here

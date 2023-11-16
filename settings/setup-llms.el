;; I would like to modify this function.
;; Right now it takes the current region and passes it to `llm`.
;; If a prefix argument is used, I would like to prompt for a message
;; and pass that message as a system message like this `llm -s "<PROMPT>"
(defun llm-query-and-insert (start end command)
  (interactive
   (let* ((prompt "Enter system message: ")
          (system-message (if current-prefix-arg (read-string prompt) ""))
          (command (format "llm -s \"%s\"" system-message)))
     (if (use-region-p)
         (list (region-beginning) (region-end) command)
       (list (line-beginning-position) (line-end-position) command))))
  (message command)
  (let ((response (shell-command-on-region-to-string start end command)))
    (kill-new response)
    (save-excursion
      (end-of-visual-line)
      (newline)
      (insert response))))

(defvar llm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'copilot-mode)
    (define-key map (kbd "w") #'whisper-run)
    (define-key map (kbd "l") #'llm-query-and-insert)
    map)
  "Keymap for LLM commands.")
(fset 'llm-command-map llm-command-map)
(define-key projectile-mode-map (kbd "C-c l") 'llm-command-map)

(provide 'setup-llms)

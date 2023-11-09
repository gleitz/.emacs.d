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

(provide 'setup-llms)

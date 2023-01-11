(defun shell-command-on-region-to-string (start end command)
  (with-output-to-string
    (shell-command-on-region start end command standard-output)))

(defun howdoi (start end command)
  (interactive
   (let ((command "xargs -0 howdoi")) ;; (read-shell-command "Shell command on region: ")
     (if (use-region-p)
         (list (region-beginning) (region-end) command)
       (list (line-beginning-position) (line-end-position) command)
       )))
  (let ((response (shell-command-on-region-to-string start end command)))
    (kill-new response)
  (save-excursion
    (end-of-visual-line)
    (newline)
    (insert (shell-command-on-region-to-string start end command)))))

(provide 'setup-howdoi)

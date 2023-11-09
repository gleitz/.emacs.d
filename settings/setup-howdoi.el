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
      (insert response))))

(provide 'setup-howdoi)

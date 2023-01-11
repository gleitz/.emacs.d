(require 'copilot)

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(defun my/copilot-next-completion ()
  (interactive)
  (or (copilot-next-completion)))

(defun my/copilot-previous-completion ()
  (interactive)
  (or (copilot-previous-completion)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<M-RET>") #'my/copilot-tab)
  (define-key copilot-mode-map (kbd "C-M-y") #'my/copilot-next-completion)
  (define-key copilot-mode-map (kbd "C-c C-M-y") #'my/copilot-previous-completion))

(provide 'setup-copilot)

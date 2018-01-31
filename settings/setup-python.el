(setq ffip-prune-patterns '())

(elpy-enable)

(define-key elpy-mode-map (kbd "M-<right>") 'subword-right)
(define-key elpy-mode-map (kbd "M-<left>") 'subword-left)

(setq python-python-command "python3")
(setq elpy-rpc-python-command "python3")

(provide 'setup-python)

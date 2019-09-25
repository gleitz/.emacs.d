(setq ffip-prune-patterns '())

(elpy-enable)
(setq elpy-rpc-backend "jedi")

(remove-hook 'elpy-modules 'elpy-module-flymake)

(define-key elpy-mode-map (kbd "M-<right>") 'subword-right)
(define-key elpy-mode-map (kbd "M-<left>") 'subword-left)

(setq python-python-command "python3")
(setq elpy-rpc-python-command "python3")

(flycheck-add-next-checker 'python-flake8 'python-pylint)

(provide 'setup-python)

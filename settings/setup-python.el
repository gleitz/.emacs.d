(setq ffip-prune-patterns '())

(advice-add 'python-mode :before 'elpy-enable)
(setq elpy-rpc-backend "jedi")

(remove-hook 'elpy-modules 'elpy-module-flymake)

(add-hook 'elpy-mode-hook (lambda ()
                            (define-key elpy-mode-map (kbd "M-<right>") 'subword-right)
                            (define-key elpy-mode-map (kbd "M-<left>") 'subword-left)
                            (define-key elpy-mode-map [backtab] 'tab-indent-or-complete)))

(setq python-python-command "python3")
(setq elpy-rpc-python-command "python3")

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)))

(add-hook 'python-mode-hook 'jedi:setup)

(provide 'setup-python)

;; Ensure lsp-mode is loaded before adding hooks
(require 'lsp-mode)

(add-hook 'yaml-mode-hook 'lsp-deferred)
(add-hook 'yaml-mode-hook 'flycheck-mode)

;; Enable LSP diagnostics for flycheck and chain checkers
(with-eval-after-load 'flycheck
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  (flycheck-add-next-checker 'lsp '(t . yaml-yamllint)))

(provide 'setup-yaml)

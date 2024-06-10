(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)) ;; not sure if I need this?
  ;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (column-enforce-mode +1)
  (company-mode +1)
  (define-key tide-mode-map (kbd "C-.") 'tide-references))

(require 'tide)

(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; removed because this forces indent which causes `parameters are not aligned`
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; TSX
(add-hook 'js-mode-hook
          (lambda ()
            (setup-tide-mode)))

;; Treesitter
(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setup-tide-mode)))

(flycheck-define-checker typescript-tsc
  "A TypeScript syntax checker using tsc, the TypeScript compiler.

See URL `http://www.typescriptlang.org/'."
  ;; :command ("tsc"
  ;;           "--noEmit"
  ;;           source-inplace)
  :command ("tsc"
            "--noEmit"
            "--pretty"
            "false")
  :enabled (lambda () (vc-find-root buffer-file-name "tsconfig.json"))
  :error-patterns
  ((error line-start (file-name) "(" line "," column "): error"
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes js-mode)

;; Disable tslint for eslint instead
;; (flycheck-add-mode 'typescript-tide 'web-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'typescript-tsc)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
;; (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)

(provide 'setup-typescript)

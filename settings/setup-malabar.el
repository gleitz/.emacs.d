(load-file "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/malabar-mode/src/main/lisp")

(setq malabar-extra-source-locations (quote ("~/projects/java/" "~/projects/ebay/TeamFeed/feedhome/feedhome/src/main/java/")))

(add-hook 'after-init-hook (lambda ()
                             (message "activate-malabar-mode")
                             (activate-malabar-mode)))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

;; (eval-after-load 'inf-groovy
;; (add-hook 'inf-groovy-load-hook 'flycheck-mode))
;; (eval-after-load 'java-mode
;; (add-hook 'java-mode-hook   'flycheck-mode))

(provide 'setup-malabar)

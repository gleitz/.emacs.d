(setq user-scala-dir
      (concat user-emacs-directory "lib/scala/ensime/src/main/elisp/"))
(add-to-list 'load-path user-scala-dir)

(require 'ensime)

(add-hook 'scala-mode-hook '(lambda ()
  ;; format buffer on save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'ensime-format-source)
  (flycheck-mode 1)
  (ensime-scala-mode-hook)
))

(add-to-list 'load-path (concat user-emacs-directory "users/" user-login-name) "ENSIME_ROOT/elisp/")

(require 'nodejs-repl)

(provide 'setup-scala)

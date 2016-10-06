(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(custom-safe-themes (quote ("9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "f3ec2da81c2b1f66f911fe47843a09055754b40fafaddcce79bbd4d781161329" "30c6aef3025cd6f05ccb611ec8838a448a14a6784987ed98b24f78916d63b388" "84ff07913c6109d12bfda40644daeaaa8f4665afb5f04e13e422bd98b02ee88b" "cf33119622dd833e4d8f904f34c5e3ff95d1d3d45bada72dd44648b3470bdebe" "f5776f3da6117901f29405fe52edb2bcba6a687629b4cbd5923d1a642484f2f9" "d56e289b10204629ac5c35b9621a650a534ef3baf183a1c601b4936482321df1" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" "ff73e1b0216feca9e041dcb3196938442cc6aa8319f97eedbc2a3e38c8ca9825" "a18dd0a954ac63a80e62c8cb1b550ffcf5d8461189c7c672555faadf2facfcf3" "cb36f8e44d41595010baa23737984c4ecb2d8cc2e363ec15fbfa0408c2f8ea9f" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" default)))
 '(diff-refine-change ((t (:background "midnight blue"))) t)
 '(flycheck-error ((t (:underline "Pink"))))
 '(flycheck-error-face ((t (:inherit error :underline "pink"))) t)
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(flycheck-warning-face ((t (:inherit warning :underline "DarkOrange"))) t)
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(js2-error-face ((t nil)) t)
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))) t)
 '(js2-warning-face ((t nil)) t)
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(safe-local-variable-values (quote ((eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples[[:blank:]]+\\(.*\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face)))))) (eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode)) (eval font-lock-add-keywords nil (quote (("defexamples\\| => " (0 (quote font-lock-keyword-face)))))) (encoding . utf-8))))
 '(show-paren-match ((nil (:background "#5a00ff"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("fbfbd18874a5a5a0d1ac9036f57c189195f30d57d92d9cf96b12195d18714850" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default)))
 '(ensime-default-server-root "/Users/bgleitzman/.emacs.d/lib/scala/ensime/")
 '(ensime-sbt-compile-on-save nil)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-jshintrc "~/.jshintrc")
 '(flycheck-pylintrc "~/.pylintrc")
 '(global-auto-complete-mode t)
 '(global-semantic-idle-scheduler-mode nil)
 '(initial-scratch-message "")
 '(js2-pretty-multiline-decl-indentation-p t)
 '(magit-push-always-verify nil t)
 '(magit-stage-all-confirm nil t)
 '(message-log-max 1000)
 '(nxml-child-indent 4)
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (arduino-mode yesql-ghosts whitespace-cleanup-mode web-mode visual-regexp-steroids vagrant-tramp swiper string-edit speed-type smartparens slime-js simple-httpd scala-mode2 reveal-in-finder restclient quickrun projectile prodigy php-mode paredit package-utils nodejs-repl move-text maxframe markdown-mode malabar-mode magit-tramp less-css-mode keyfreq jsx-mode js2-refactor inflections ido-vertical-mode ido-at-point hydra htmlize highlight-escape-sequences guide-key gitignore-mode gitconfig-mode git-messenger gist flycheck-pos-tip flx-ido fill-column-indicator exec-path-from-shell elpy elisp-slime-nav dockerfile-mode dired-details+ css-eldoc clojure-mode-extra-font-locking auto-complete ag)))
 '(read-quoted-char-radix 16)
 '(recentf-max-saved-items 1000)
 '(rst-compile-toolsets
   (quote
    ((html "rst2html5" ".html" nil)
     (latex "rst2latex" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml" ".xml" nil)
     (xml "rst2xml" ".xml" nil)
     (pdf "rst2pdf" ".pdf" nil)
     (s5 "rst2s5" ".html" nil))))
 '(semantic-idle-scheduler-idle-time 10)
 '(sgml-basic-offset 4)
 '(shell-file-name "/bin/bash")
 '(split-height-threshold nil)
 '(split-width-threshold 120)
 '(tab-width 4)
 '(tern-ac-on-dot nil)
 '(tramp-default-method "ssh")
 '(tramp-ssh-controlmaster-options "" t)
 '(visible-bell nil)
 '(warning-minimum-level :error))

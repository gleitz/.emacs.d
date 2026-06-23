(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(beacon-fallback-background ((t (:background "#333366"))))
 '(claude-code-repl-face ((t (:family "JuliaMono"))) t)
 '(company-scrollbar-bg ((t (:background "#199919991999"))) t)
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))) t)
 '(company-tooltip ((t (:inherit default :background "#051e051e051e"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#3fff3fff3fff"))))
 '(company-tooltip-scrollbar-track ((t (:background "#7fff7fff7fff"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(custom-safe-themes '("9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "f3ec2da81c2b1f66f911fe47843a09055754b40fafaddcce79bbd4d781161329" "30c6aef3025cd6f05ccb611ec8838a448a14a6784987ed98b24f78916d63b388" "84ff07913c6109d12bfda40644daeaaa8f4665afb5f04e13e422bd98b02ee88b" "cf33119622dd833e4d8f904f34c5e3ff95d1d3d45bada72dd44648b3470bdebe" "f5776f3da6117901f29405fe52edb2bcba6a687629b4cbd5923d1a642484f2f9" "d56e289b10204629ac5c35b9621a650a534ef3baf183a1c601b4936482321df1" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" "ff73e1b0216feca9e041dcb3196938442cc6aa8319f97eedbc2a3e38c8ca9825" "a18dd0a954ac63a80e62c8cb1b550ffcf5d8461189c7c672555faadf2facfcf3" "cb36f8e44d41595010baa23737984c4ecb2d8cc2e363ec15fbfa0408c2f8ea9f" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" default))
 '(diff-refine-change ((t (:background "midnight blue"))) t)
 '(ediff-current-diff-A ((t (:background "RosyBrown4"))))
 '(ediff-current-diff-B ((t (:background "DarkGreen"))))
 '(ediff-even-diff-A ((t (:background "dim gray"))))
 '(ediff-even-diff-B ((t (:background "dim gray"))))
 '(ediff-fine-diff-A ((t (:background "IndianRed3"))))
 '(ediff-fine-diff-B ((t (:background "SeaGreen3"))))
 '(ediff-odd-diff-A ((t (:background "dim gray"))))
 '(ediff-odd-diff-B ((t (:background "dim gray"))))
 '(flycheck-error ((t (:underline "Pink"))))
 '(flycheck-error-face ((t (:inherit error :underline "pink"))) t)
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(flycheck-warning-face ((t (:inherit warning :underline "DarkOrange"))) t)
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(hl-line ((t (:extend t))))
 '(js2-error-face ((t nil)))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(js2-warning-face ((t nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(safe-local-variable-values '((eval font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => " (0 'font-lock-keyword-face)) ("(defexamples[[:blank:]]+\\(.*\\)" (1 'font-lock-function-name-face)))) (eval font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => " (0 'font-lock-keyword-face)))) (eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode)) (eval font-lock-add-keywords nil '(("defexamples\\| => " (0 'font-lock-keyword-face)))) (encoding . utf-8)))
 '(selectrum-current-candidate ((t (:inherit hl-line :extend t))))
 '(show-paren-match ((nil (:background "#5a00ff"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case" "--stats" "--hidden" "-W 200"))
 '(ag-ignore-list '("node_modules" "/vendor" "/build"))
 '(auth-source-save-behavior nil)
 '(auto-save-list-file-prefix nil)
 '(beacon-color "LightGoldenrod")
 '(c-basic-offset 4)
 '(claude-code-sandbox-program "claude")
 '(claude-code-toggle-auto-select t)
 '(column-enforce-column 119)
 '(copilot-log-max 10000)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("31898ac90e9b683ce996b6378f9ae8c0a7356e8a4a96c4311dcfc10306027c46"
     "50cdbe9e35198343589c648f73fab07c3e1fb4e53057a9e5d3ddd5246140b07a"
     "cfff5a1f3b63f487c1ecda2d7aa1a7ea206533e003c17e5b8d7142ea45444a6e"
     "fbfbd18874a5a5a0d1ac9036f57c189195f30d57d92d9cf96b12195d18714850"
     "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920"
     default))
 '(ein:output-area-inlined-images t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv
                         elpy-module-highlight-indentation
                         elpy-module-django elpy-module-sane-defaults))
 '(elpy-rpc-backend "rope" t)
 '(elpy-syntax-check-command "pylint")
 '(ensime-default-server-root "/Users/bgleitzman/.emacs.d/lib/scala/ensime/")
 '(ensime-sbt-compile-on-save nil)
 '(explicit-bash-args '("--noediting" "--noprofile" "--norc" "-i"))
 '(fci-rule-column nil)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-checker-error-threshold 2000)
 '(flycheck-checkers
   '(tsx-tide typescript-tide python-mypy bash coffee-coffeelint
              css-csslint elixir emacs-lisp emacs-lisp-checkdoc erlang
              go-gofmt go-build go-test haml html-tidy lua perl php
              php-phpcs puppet-parser puppet-lint python-flake8
              python-pylint rst ruby-rubocop ruby ruby-jruby rust sass
              scala scss sh-dash sh-bash tex-chktex tex-lacheck
              xml-xmlstarlet zsh javascript-jshint javascript-eslint
              javascript-tide jsx-tide typescript-tsc yaml-yamllint))
 '(flycheck-disabled-checkers
   '(json-jsonlist emacs-lisp emacs-lisp-checkdoc jsx-tide
                   typescript-tslint typescript-tide tsx-tide
                   javascript-tide javascript-jshint))
 '(flycheck-eslint-args nil)
 '(flycheck-jshintrc "~/.jshintrc")
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-mypy-config '("mypy.ini" "setup.cfg" "~/.mypy.ini"))
 '(flycheck-python-pylint-executable "pylint")
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.inf.replicant.ai" "gitlab.inf.replicant.ai/api/v4"
      "gitlab.inf.replicant.ai" forge-gitlab-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com"
      forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org"
      forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org"
      forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org"
      forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org"
      forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org"
      forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org"
      forge-cgit**-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org"
      forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))
 '(global-semantic-idle-scheduler-mode nil)
 '(grep-find-ignored-directories
   '(".meteor" "node_modules" "target" "SCCS" "RCS" "CVS" "MCVS" ".src"
     ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "bin"))
 '(grep-save-buffers nil)
 '(haskell-compile-cabal-build-command "stack build")
 '(ido-buffer-disable-smart-matches nil)
 '(ido-cr+-replace-completely nil)
 '(initial-scratch-message "")
 '(jedi:tooltip-method nil)
 '(js2-basic-offset 2)
 '(js2-pretty-multiline-decl-indentation-p t)
 '(magit-branch-adjust-remote-upstream-alist '(("origin/main" . "")))
 '(magit-process-apply-ansi-colors 'filter)
 '(magit-push-always-verify nil t)
 '(magit-stage-all-confirm nil t)
 '(markdown-command "marked")
 '(message-log-max 1000)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-scroll-amount-horizontal 100)
 '(mouse-wheel-tilt-scroll t)
 '(nxml-child-indent 4)
 '(package-archive-priorities '(("MELPA Stable" . 10) ("GNU ELPA" . 5) ("MELPA" . 0)))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(ace-jump-mode ace-window ag aider aidermacs arduino-cli-mode
                   arduino-mode atomic-chrome auto-complete
                   auto-virtualenv bash-completion beacon
                   benchmark-init browse-kill-ring change-inner
                   clojure-mode-extra-font-locking column-enforce-mode
                   company-jedi company-prescient css-eldoc diminish
                   dockerfile-mode dot-mode edit-server editorconfig
                   ein elisp-slime-nav elpy emojify eproject
                   exec-path-from-shell fill-column-indicator
                   find-file-in-project fireplace flx-ido
                   flycheck-haskell flycheck-popup-tip forge gist
                   git-link gptel groovy-mode guide-key
                   highlight-escape-sequences hydra ido-at-point
                   ido-completing-read+ ido-vertical-mode
                   impatient-mode inflections inheritenv jinja2-mode
                   json-mode jtsx jump-char keyfreq live-py-mode
                   lsp-mode lsp-pyright magit-popup maxframe
                   mermaid-mode mic-paren move-text multifiles
                   multiple-cursors nodejs-repl obsidian olivetti
                   package-utils pandoc pandoc-mode paredit
                   persistent-scratch perspective php-mode pkg-info
                   prodigy projectile python-black python-mls quickrun
                   realgud restclient reveal-in-osx-finder rg ripgrep
                   rjsx-mode scala-mode selectrum-prescient
                   shell-command+ shell-maker simplezen smartparens
                   smex smooth-scrolling speed-type sqlite3
                   string-edit-at-point symbol-overlay tagedit tern
                   tide transpose-frame tumblesocks typo undo-tree
                   virtualenvwrapper visual-regexp-steroids vterm
                   whitespace-cleanup-mode yaml-mode))
 '(prescient-filter-method '(literal regexp initialism fuzzy))
 '(projectile-enable-caching nil)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_"
     ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build"))
 '(read-quoted-char-radix 16)
 '(realgud:pdb-command-name "./pdb")
 '(recentf-max-saved-items 1000)
 '(ripgrep-arguments '("--max-columns=100" "--max-columns-preview"))
 '(ripgrep-highlight-search t)
 '(rst-compile-toolsets
   '((html "rst2html5" ".html" nil) (latex "rst2latex" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml" ".xml" nil) (xml "rst2xml" ".xml" nil)
     (pdf "rst2pdf" ".pdf" nil) (s5 "rst2s5" ".html" nil)))
 '(safe-local-variable-values
   '((arduino-cli-default-fqbn . "arduino:avr:uno")
     (arduino-cli-default-port . "/dev/cu.usbmodem14601")
     (arduino-cli-default-fqbn quote arduino:avr:uno)
     (arduino-cli-default-port quote /dev/cu\.usbmodem14601)
     (eval setq flycheck-disabled-checkers
           (append flycheck-disabled-checkers '(typescript-tsc)))
     (flycheck-disabled-checkers . typescript-tsc)
     (web-mode-code-indent-offset . 2)
     (web-mode-markup-indent-offset . 2)
     (web-mode-code-indent-offset . 4)
     (web-mode-markup-indent-offset . 4) (jsx-indent-level . 4)))
 '(semantic-idle-scheduler-idle-time 10)
 '(sgml-basic-offset 2)
 '(sh-basic-offset 2)
 '(shell-file-name "/bin/bash")
 '(smex-flex-matching t)
 '(split-height-threshold nil)
 '(split-width-threshold 160)
 '(tab-width 4)
 '(tern-ac-on-dot nil)
 '(tide-format-options '(indentSize 2 tabSize 2))
 '(tramp-default-method "ssh")
 '(tramp-ssh-controlmaster-options "" t)
 '(typescript-indent-level 2)
 '(typo-quotation-marks
   '(("Czech" "„" "“" "‚" "‘") ("Czech (Guillemets)" "»" "«" "›" "‹")
     ("English" "\"" "\"" "'" "'") ("German" "„" "“" "‚" "‘")
     ("German (Guillemets)" "»" "«" "›" "‹")
     ("French" "«" "»" "‹" "›") ("Finnish" "”" "”" "’" "’")
     ("Finnish (Guillemets)" "»" "»" "›" "›")
     ("Swedish" "”" "”" "’" "’") ("Russian" "«" "»" "„" "“")
     ("Italian" "«" "»" "“" "”") ("Polish" "„" "”" "‚" "’")
     ("Serbian" "„" "”" "’" "’") ("Ukrainian" "«" "»" "„" "“")))
 '(undo-tree-auto-save-history nil)
 '(visible-bell nil)
 '(vr/command-python
   "python3 ~/.emacs.d/elpa/visual-regexp-steroids-20170222.253/regexp.py")
 '(warning-minimum-level :error)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2)
 '(whisper-install-directory "~/projects/")
 '(whisper-model "tiny.en.bin-q4_0"))

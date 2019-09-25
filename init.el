;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Don't backup files, please
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(ace-jump-mode
     ace-window
     ag
     arduino-mode
     auto-complete
     bash-completion
     benchmark-init
     browse-kill-ring
     change-inner
     column-enforce-mode
     clojure-mode
     clojure-mode-extra-font-locking
     css-eldoc
     dockerfile-mode
     emojify
     elisp-slime-nav
     elpy
     ensime
     exec-path-from-shell
     expand-region
     f
     fill-column-indicator
     flx
     flx-ido
     flycheck
     flycheck-haskell
     flycheck-mypy
     flycheck-pos-tip
     gist
     gitconfig-mode
     gitignore-mode
     groovy-mode
     guide-key
     haskell-mode
     highlight-escape-sequences
     htmlize
     hydra
	 ido-completing-read+
     ido-at-point
     ido-vertical-mode
     inflections
     js2-mode
     js2-refactor
     json-mode
     jsx-mode
     jump-char
     keyfreq
     less-css-mode
     magit
     markdown-mode
     maxframe
     mic-paren
     move-text
     multifiles
     nodejs-repl
     package-utils
     paredit
     persistent-scratch
     perspective
     php-mode
     prodigy
     projectile
     quickrun
     realgud
     restclient
     reveal-in-osx-finder
     rjsx-mode
     scala-mode
     simple-httpd
     simplezen
     smartparens
     smex
     speed-type
     string-edit
     tagedit
     tern
     tide
     virtualenvwrapper
     visual-regexp
     visual-regexp-steroids
     web-mode
     wgrep
     whitespace-cleanup-mode
     yasnippet
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Need to know how fast we start
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Arduino
;; (require 'arduino-mode)

 ;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-haskell)
(require 'setup-html-mode)
(require 'setup-paredit)
(require 'setup-projectile)
(require 'setup-python)
(require 'setup-js-beautify)
(require 'setup-nodejs)
(require 'setup-scala)
(require 'setup-web-mode)
(require 'setup-jsx-mode)
;; (require 'setup-realgud)
(require 'setup-typescript)
(require 'setup-keyfreq)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode
          scala-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
;; (eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)
(autoload 'auto-complete-mode "auto-complete" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Load SimpleRTM
(autoload 'simple-rtm-mode "simple-rtm" "Interactive mode for Remember The Milk" t)
(defun simple-rtm-open-inbox ()
  (interactive)
  (kmacro-exec-ring-item (quote ([19 73 110 98 111 120 13 tab] 0 "%d")) 1))

;; Map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(require 'visual-regexp-steroids)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'change-inner)
(require 'multifiles)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Misc
;; (require 'project-archetypes)
(require 'my-misc)
(when is-mac (require 'mac))

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Edit with Emacs
(require 'edit-server)
(edit-server-start)
(setq edit-server-new-frame nil)
(add-hook 'edit-server-start-hook
         (lambda ()
           (normal-mode t)))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
(put 'ido-exit-minibuffer 'disabled nil)

;; Recompile files on exit
(setq config-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defun byte-compile-dotfiles ()
  "Byte compile all Emacs dotfiles."
  (interactive)
  ;; Automatically recompile the entire .emacs.d directory.
  (byte-recompile-directory (expand-file-name config-dir) 0))
(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (byte-compile-dotfiles)
    ;; (message "%s compiled" user-init-file)
    ))
(add-hook 'kill-emacs-hook 'byte-compile-user-init-file t t)

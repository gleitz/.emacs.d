(setq ffip-prune-patterns '())

(advice-add 'python-mode :before 'elpy-enable)
(setq elpy-rpc-backend "jedi")

(remove-hook 'elpy-modules 'elpy-module-flymake)

(add-hook 'elpy-mode-hook (lambda ()
                            (define-key elpy-mode-map (kbd "C-<return>") nil) ;; this is going to be used by copilot
                            (define-key elpy-mode-map (kbd "M-<right>") 'subword-right)
                            (define-key elpy-mode-map (kbd "M-<left>") 'subword-left)
                            (define-key elpy-mode-map [backtab] 'tab-indent-or-complete)))

(setq python-python-command "python3")
(setq elpy-rpc-python-command "python3")

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (python-black-on-save-mode 1)))

(add-hook 'python-mode-hook 'jedi:setup)

;; Checker for jupyter notebook
(defvar current-jupyter-file-name "*.ipynb")
(defun my-ein-window-configuration-change-hook ()
  ;; check to see if (ein:get-notebook) is defined
  (when (fboundp 'ein:get-notebook)
    ;; check to see if the notebook is defined
    (when (ein:get-notebook)
      ;; set the current jupyter file name
      (setq current-jupyter-file-name (replace-regexp-in-string "http://[^/]+/api/contents/" "./" (ein:notebook-url (ein:get-notebook))))
      ;; write current-jupyter-file-name to /tmp/current-jupyter-file-name
      (with-temp-file "/tmp/current-jupyter-file-name"
        (insert current-jupyter-file-name))
      )))
(add-hook 'window-configuration-change-hook #'my-ein-window-configuration-change-hook)

;; (global-flycheck-mode)
;; (flycheck-def-executable-var flake8-nb "flake8_nb_runner")
;; (defun flycheck-parse-flake8-nb (output checker buffer)
;;   "Parse flake8-nb errors from OUTPUT and return a list of `flycheck-error' objects."
;;   (let ((errors nil)
;;         (patterns (rx line-start
;;                       (group-n 1 (zero-or-more not-newline))  ; File path without suffix  ;;REM used to have "src/" after the 1
;;                       "#In[" (zero-or-more not-newline) "]"          ; Cell reference, ignored
;;                       ":"
;;                       (group-n 2 (one-or-more digit))                ; Line number
;;                       ":"
;;                       (group-n 3 (one-or-more digit))                ; Column number
;;                       ": "
;;                       (group-n 4 (one-or-more (not space)))          ; Error code
;;                       " "
;;                       (group-n 5 (zero-or-more not-newline))         ; Message
;;                       line-end)))
;;     (dolist (line (split-string output "\n"))
;;       (when (string-match patterns line)
;;         (let ((file (match-string-no-properties 1 line))
;;               (line (string-to-number (match-string-no-properties 2 line)))
;;               (column (string-to-number (match-string-no-properties 3 line)))
;;               (code (match-string-no-properties 4 line))
;;               (message (match-string-no-properties 5 line)))
;;           (push (flycheck-error-new-at line column 'error message
;;                                        :id code
;;                                        :checker checker
;;                                        :buffer buffer
;;                                        :filename file)
;;                 errors))))
;;     (nreverse errors)))

;; (flycheck-define-command-checker 'flake8-nb
;;   "A Flake8 checker for jupyter notebooks"
;;   :command '("flake8-nb-runner")
;;   ;; :error-patterns '((error line-start (file-name) ":" line ":" column ": " (message) line-end))
;;   :error-parser 'flycheck-parse-flake8-nb
;;   :modes '(python-mode)) ;;fundamental-mode markdown-mode ein:markdown-mode))

;; Config for jupyter notebook
(defun my/flycheck-buffer-action ()
  ;; if flycheck mode is enabled
    (when flycheck-mode
      (flycheck-buffer)))
(defvar current-ein-major-mode nil)
(defun my/flycheck-check-ein-mode-update ()
  (if (and (eq major-mode 'python-mode)
           (bound-and-true-p ein:notebook-mode))
      (progn
        (if (not (eq major-mode current-ein-major-mode))
            (progn
              (setq flycheck-disabled-checkers (append '(python-flake8 python-pylint python-mypy) flycheck-disabled-checkers))
              (flycheck-select-checker 'flake8-nb)
              (run-with-idle-timer 0.5 t #'my/flycheck-buffer-action)
              (setq current-ein-major-mode major-mode))))
    (progn
      (setq current-ein-major-mode major-mode)
      )
    ))

;; doesn't seem to want to work with Emacs 29
(defun setup-ein-keys ()
  (define-key python-mode-map (kbd "C-<return>") 'ein:worksheet-execute-cell-and-goto-next-km)
  (run-with-idle-timer 0.5 t #'my/flycheck-check-ein-mode-update))

;; (add-hook 'ein:notebook-mode-hook #'setup-ein-keys)

(provide 'setup-python)

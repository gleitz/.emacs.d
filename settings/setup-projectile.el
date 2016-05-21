;; Use projectile everywhere
(projectile-global-mode)

;; Use silver searcher in projectile
(defvar projectile-ag-command
  (concat "\\ag" ; used unaliased version of `ag': \ag
          " -i" ; case insensitive
          " -f" ; follow symbolic links
          " -0" ; output null separated results
          " -g ''") ; get file names matching the regex ''
  "Ag command to be used by projectile to generate file cache.")

(when (executable-find "ag")
  (defun projectile-get-ext-command ()
    "Override the projectile-defined function so that `ag' is always used for
getting a list of all files in a project."
    projectile-ag-command))

(provide 'setup-projectile)

;; Use projectile everywhere
(projectile-global-mode)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Use silver searcher in projectile
;; (defvar projectile-ag-command
;;   (concat "\\ag" ; used unaliased version of `ag': \ag
;;           " -i" ; case insensitive
;;           " -f" ; follow symbolic links
;;           " -0" ; output null separated results
;;           " -g ''") ; get file names matching the regex ''
;;   "Ag command to be used by projectile to generate file cache.")

;; (when (executable-find "ag")
;;   (defun projectile-get-ext-command ()
;;     "Override the projectile-defined function so that `ag' is always used for
;; getting a list of all files in a project."
;;     projectile-ag-command))

(setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))

;; Never treat ~/ as a project. Projectile will try to enumerate every file
;; under ~/ which hangs Emacs.
(defvar my--home-directory (expand-file-name "~/"))

(define-advice projectile-project-root (:around (orig-fn &rest args) skip-home)
  "Return nil when in ~/, but resolve symlinks first."
  (let ((dir (or (car args) default-directory)))
    (if (and dir (string= (expand-file-name dir) my--home-directory))
        ;; In ~/: resolve symlinks on buffer-file-name to find real directory.
        (when-let* ((truename (and buffer-file-name
                                   (file-truename buffer-file-name)))
                    (true-dir (file-name-directory truename)))
          (let ((default-directory true-dir))
            (apply orig-fn args)))
      (apply orig-fn args))))

(define-advice project-current (:around (orig-fn &rest args) skip-home)
  "Return nil when default-directory is ~/."
  (unless (string= (expand-file-name default-directory) my--home-directory)
    (apply orig-fn args)))

(provide 'setup-projectile)

;; Malabar mode
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
;; Enable EDE (Project Management) features
(global-ede-mode 1)
(setq malabar-extra-source-locations (quote ("~/projects/java/" "~/projects/ebay/TeamFeed/feedhome/feedhome/src/main/java/")))
(setq malabar-groovy-lib-dir (expand-file-name "~/.emacs.d/site-lisp/malabar/lib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/malabar/lisp"))
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-mru-bookmark-mode))
(require 'cedet)
(semantic-mode 1)
(require 'malabar-mode)

(defun walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))
(defun walk-path-visitor (dir file)
  "Called by walk-path for each file found"
  ;; (message (concat dir file))
  (if (or
       (string-match "-sources\.jar$" file)
       (string-match "\.jar$" file))
      (progn
        (add-to-list 'malabar-extra-source-locations (expand-file-name (concat dir file)))
        t)
    t))

(walk-path "~/maven.repo/" 'walk-path-visitor)

(defun malabar-refresh ()
  (interactive)
  (walk-path "~/maven.repo/" 'walk-path-visitor)
  (malabar-clear-typecache))

;; Autocompile on save
(add-hook 'malabar-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)
            (setq indent-tabs-mode t)
            ))

;; Make Malabar's autoimport behave more like Eclipse
(defun malabar-eclipse-import ()
  "Eclipse style import handling."
  (interactive)
  (malabar-import-all)
  (malabar-import-group-imports))
(add-hook 'malabar-mode-hook
          (lambda () (local-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(provide 'setup-malabar)

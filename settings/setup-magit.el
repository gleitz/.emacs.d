;; full screen magit-status

(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

;; don't prompt me

(set-default 'magit-push-always-verify nil)
(set-default 'magit-revert-buffers 'silent)
(set-default 'magit-no-confirm '(stage-all-changes
                                 unstage-all-changes))

;; move cursor into position when entering commit message

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (forward-line 2)))

(add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; C-c C-a to amend without any prompt

(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

;; Rebase until it hurts
;; http://jrb.tumblr.com/post/49248876242/git-pull-rebase-until-it-hurts
(defun magit-pull ()
  (interactive)
  (magit-run-git-async "pull" "--rebase" "-v"))

(defun magit-abort-rebase-and-pull ()
  (interactive)
  (magit-run-git-async "rebase" "--abort" "&&" "git" "pull"))

(defun magit-pull-no-rebase ()
  (interactive)
  (magit-run-git-async "pull"))

;; Don't use Emacs version control
(setq vc-handled-backends ())

;; Create Github PRs

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (or (magit-get-remote-branch)
                    (user-error "No remote branch"))))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url))

;;; modified for gitlab
(defun endless/visit-pull-request-url-gitlab ()
  "Visit the current branch's PR on Gitlab."
  (interactive)
  (browse-url
   (format "https://gitlab.inf.replicant.ai/%s/merge_requests/new"
           (replace-regexp-in-string
            "\\`.+gitlab\\.inf\\.replicant\\.ai:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url")))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url-gitlab))

;; At some point, started getting the issue
;; Symbol’s function definition is void: -reductions-from
;; Included from https://github.com/magnars/dash.el/blob/master/dash.el#L286
(defun -reductions-from (fn init list)
  "Return a list of the intermediate values of the reduction.
See `-reduce-from' for explanation of the arguments.
See also: `-reductions', `-reductions-r', `-reduce-r'"
  (nreverse (--reduce-from (cons (funcall fn (car acc) it) acc) (list init) list)))

(provide 'setup-magit)

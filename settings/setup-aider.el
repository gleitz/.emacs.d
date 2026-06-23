(require 'aider)

;; Read the current ANTHROPIC_API_KEY from the rc file the shell sources
;; (`~/.claude-api-keyrc`, managed by `claude-auth max|api`). Replaces the old
;; auth-source / ~/.authinfo lookup, which drifted to a stale key.
(defun gleitz--anthropic-api-key ()
  "Return ANTHROPIC_API_KEY exported from `~/.claude-api-keyrc', or nil."
  (let ((file (expand-file-name "~/.claude-api-keyrc")))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^export ANTHROPIC_API_KEY=\\(.*\\)$" nil t)
          (let ((val (match-string 1)))
            (cond
             ((string-match "\\`\"\\(.*\\)\"\\'" val) (match-string 1 val))
             ((string-match "\\`'\\(.*\\)'\\'" val)   (match-string 1 val))
             (t val))))))))

(with-eval-after-load 'aider
  ;; aider runs against lm_studio (local) — no Anthropic key needed here.
  (setq aider-args '("--model" "lm_studio/qwen/qwen3-coder-30b")))

(with-eval-after-load 'aidermacs
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-default-model "sonnet")
  (add-to-list 'aidermacs-extra-args "--verbose")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)

  ;; Inject env via aidermacs's scoped hook. `aidermacs-run-backend' wraps the
  ;; hook in `(let ((process-environment process-environment)) ...)', so these
  ;; setenvs are visible only to the aidermacs subprocess. A top-level setenv
  ;; would mutate Emacs's global env and leak into every other subprocess —
  ;; previously this was 401-ing happy on phone takeover.
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              (let ((key (gleitz--anthropic-api-key)))
                (when key (setenv "ANTHROPIC_API_KEY" key)))
              (setenv "LM_STUDIO_API_KEY" "dummy-api-key")
              (setenv "LM_STUDIO_API_BASE" "http://localhost:1234/v1"))))

(provide 'setup-aider)

(require 'aider)
(require 'auth-source)

;; Helper function to retrieve Anthropic API key from .authinfo
(defun get-anthropic-api-key ()
  "Retrieve Anthropic API key from auth-source."
  (let ((auth (nth 0 (auth-source-search :host "api.anthropic.com"
                                          :requires '(secret)))))
    (when auth
      (let ((secret (plist-get auth :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(with-eval-after-load 'aider
  ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setq aider-args '("--model" "lm_studio/qwen/qwen3-coder-30b"))
  (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
)

(with-eval-after-load 'aidermacs
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-default-model "sonnet")
  (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
  (setenv "LM_STUDIO_API_KEY" "dummy-api-key")
  (setenv "LM_STUDIO_API_BASE" "http://localhost:1234/v1")
  (add-to-list 'aidermacs-extra-args "--verbose")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
)

(provide 'setup-aider)

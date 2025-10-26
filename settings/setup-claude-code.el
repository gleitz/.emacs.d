(require 'monet)
(require 'claude-code)
(require 'matisse)

;; Enable claude-code-mode
(claude-code-mode 1)

(setq claude-code-terminal-backend 'vterm)

;; Set up key bindings
(global-set-key (kbd "C-c c") claude-code-command-map)

;; Define a function to apply Claude Code settings
(defun my-claude-code-setup ()
  "Apply custom keybindings and font settings for Claude Code."
  ;; Unbind vterm-copy-mode from C-c C-t first
  (local-unset-key (kbd "C-c C-t"))
  (local-set-key (kbd "C-c C-t") 'claude-code-toggle-read-only-mode)
  ;; important - tell emacs to use our fontset settings
  (setq use-default-font-for-symbols nil))

(custom-set-faces
   '(claude-code-repl-face ((t (:family "JuliaMono")))))

;; Apply settings both when Claude Code starts and when vterm mode initializes
(add-hook 'claude-code-start-hook #'my-claude-code-setup)

;; Also apply when vterm-mode-hook runs, but only in claude-code buffers
(add-hook 'vterm-mode-hook
          (lambda ()
            (when (and (boundp 'claude-code-mode) claude-code-mode)
              (my-claude-code-setup))))

;; Optional: Set up repeat map for cycling modes
(defvar my-claude-code-map (make-sparse-keymap))
(define-key my-claude-code-map (kbd "M") 'claude-code-cycle-mode)
(put 'claude-code-cycle-mode 'repeat-map 'my-claude-code-map)

;; Optional IDE integration with Monet
(when (featurep 'monet)
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1))

(defun my-claude-notify (title message)
  "Send notification using terminal-notifier with Sonar sound."
  (call-process "terminal-notifier" nil nil nil
                "-message" message
                "-title" title
                "-sound" "Ping"))

(setq claude-code-notification-function #'my-claude-notify)

;; Example: Always show Claude in split buffer on the right
(defun my-claude-display-right (buffer)
  "Display Claude buffer in right window, taking half the frame width."
  (let ((window (display-buffer buffer '((display-buffer-in-direction)
                                         (direction . right)
                                         (window-width . 0.5)))))

    ;; Send SIGWINCH to force TUI to recalculate layout
    (run-with-timer 0.2 nil
      (lambda (buf)
        (when-let ((proc (get-buffer-process buf)))
          (signal-process proc 'SIGWINCH)))
      buffer)
    window))
(setq claude-code-display-window-fn #'my-claude-display-right)

;; Claude Code IDE
(require 'claude-code-ide)
;; Set up the built-in Emacs tools
(global-set-key (kbd "C-c C-'") 'claude-code-ide-menu)
(claude-code-ide-emacs-tools-setup)

(provide 'setup-claude-code)

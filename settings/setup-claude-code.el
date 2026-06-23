(require 'monet)
(require 'claude-code)
(require 'matisse)


;; Disable claude-code's window-resize optimization advice.  It only
;; signals a resize event when the selected window's buffer is NOT a
;; claude buffer (or, if it is, only on width changes and not
;; read-only).  The effect: resizing the Emacs frame reflows claude
;; in *non-selected* panes but the active pane stays stuck at its
;; old layout.  Disabling restores normal vterm resize behavior for
;; the selected window too.
(setq claude-code-optimize-window-resize nil)

;; Already-set-up claude buffers have the advice installed at buffer
;; creation time.  Remove it globally so existing buffers (e.g. an
;; in-flight session) also get normal resize behavior.
(when (advice-member-p 'claude-code--adjust-window-size-advice
                       'vterm--window-adjust-process-window-size)
  (advice-remove 'vterm--window-adjust-process-window-size
                 'claude-code--adjust-window-size-advice))

;; Enable claude-code-mode
(claude-code-mode 1)

(setq claude-code-terminal-backend 'vterm)

;; Use Happy CLI instead of bare claude — sessions are visible from phone.
;; Path is resolved per-machine via ~ so this shared config works on both the
;; mac (/Users/gleitz) and the GPD (/home/gleitz). See the happy-on-gpd plan.
(setq claude-code-program
      (expand-file-name "~/projects/happy/packages/happy-cli/bin/happy.mjs"))
(setq claude-code-program-switches '("--dangerously-skip-permissions"))

;; Tell Happy where the server is, and pin the Claude credential per the
;; ~/.claude-auth-mode marker. Mirrors scripts/happy-autostart.sh in the happy
;; repo. Without this, when the phone takes over a terminal-launched session,
;; happy's claudeRemote.ts sees a stale ANTHROPIC_API_KEY leaked into Emacs by
;; setup-aider.el's setenv, no CLAUDE_CODE_OAUTH_TOKEN, doesn't strip the API
;; key, and the SDK 401s with "Invalid API key · Fix external API key".

(defun gleitz--read-rc-export (file var)
  "Return VAR's value from a `export VAR=...' line in FILE, or nil.
Strips surrounding single/double quotes."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^export %s=\\(.*\\)$" (regexp-quote var)) nil t)
        (let ((val (match-string 1)))
          (cond
           ((string-match "\\`\"\\(.*\\)\"\\'" val) (match-string 1 val))
           ((string-match "\\`'\\(.*\\)'\\'" val)   (match-string 1 val))
           (t val)))))))

(defun gleitz--happy-env (_buffer-name _directory)
  "Return env strings for Happy: server URL + Claude credential per mode marker."
  (let* ((mode-file (expand-file-name "~/.claude-auth-mode"))
         (mode (if (file-readable-p mode-file)
                   (string-trim
                    (with-temp-buffer
                      (insert-file-contents mode-file)
                      (buffer-string)))
                 "max"))
         (envs (list "HAPPY_SERVER_URL=https://gleitz-happy.ngrok.app")))
    (cond
     ((string= mode "api")
      (let ((key (gleitz--read-rc-export "~/.claude-api-keyrc" "ANTHROPIC_API_KEY")))
        (push (concat "ANTHROPIC_API_KEY=" (or key "")) envs)
        (push "CLAUDE_CODE_OAUTH_TOKEN=" envs)))
     (t
      (let ((token (gleitz--read-rc-export "~/.claude-oauth-tokenrc"
                                           "CLAUDE_CODE_OAUTH_TOKEN")))
        (push (concat "CLAUDE_CODE_OAUTH_TOKEN=" (or token "")) envs)
        (push "ANTHROPIC_API_KEY=" envs))))
    envs))

(add-hook 'claude-code-process-environment-functions #'gleitz--happy-env)

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

(defun gleitz--vterm-font-setup ()
  "Configure font settings specifically for vterm buffers, workaround claude-code."

  ;; Apply ASCII replacements for vterm specifically
  (let ((tbl (or buffer-display-table (setq buffer-display-table (make-display-table)))))
    (dolist (pair
             '((#x273B . ?*) ; ✻ TEARDROP-SPOKED ASTERISK
               (#x273D . ?*) ; ✽ HEAVY TEARDROP-SPOKED ASTERISK
               (#x2722 . ?+) ; ✢ FOUR TEARDROP-SPOKED ASTERISK
               (#x2736 . ?+) ; ✶ SIX-POINTED BLACK STAR
               (#x2733 . ?*) ; ✳ EIGHT SPOKED ASTERISK
               ))
      (aset tbl (car pair) (vector (cdr pair))))))

(add-hook 'vterm-mode-hook #'gleitz--vterm-font-setup)

;; Replace the font to be monospace (to avoid jumping around when Claude Code uses different symbols)
;; (custom-set-faces
   ;; '(claude-code-repl-face ((t (:family "JuliaMono")))))

;; Fix cursor disappearing after toggling read-only mode (C-c C-t).
;; vterm sets cursor-type to nil in copy-mode and doesn't restore it on exit.
;; See: https://github.com/stevemolitor/claude-code.el/issues/118
(add-hook 'vterm-copy-mode-hook
          (lambda ()
            (when (string-prefix-p "*claude:" (buffer-name))
              (setq-local cursor-type (if vterm-copy-mode t nil)))))

;; Apply settings both when Claude Code starts and when vterm mode initializes
(add-hook 'claude-code-start-hook #'my-claude-code-setup)

;; Automatically switch to the Claude Code buffer when starting a new session
(advice-add 'claude-code--start :around
            (lambda (orig-fn arg extra-switches &optional force-prompt _force-switch-to-buffer)
              (funcall orig-fn arg extra-switches force-prompt t)))

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
  ;; Disable diff tools so openDiff MCP tool is not registered.
  ;; Without this, monet intercepts file edits with an approval prompt
  ;; that overrides bypass-permissions mode.
  (setq monet-diff-tool nil)
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1))

(defun my-claude-notify (title message &optional buffer-name)
  "Send notification via terminal-notifier; click jumps to BUFFER-NAME.
Only one click action is allowed by terminal-notifier — when BUFFER-NAME
is supplied we use `-execute' so the elisp form can both raise Emacs and
pop to the buffer.  Otherwise we fall back to `-activate' so a click at
least brings Emacs forward."
  (let* ((emacsclient (or (executable-find "emacsclient") "emacsclient"))
         (click-args
          (cond
           ((and buffer-name (get-buffer buffer-name))
            (list "-execute"
                  (format "%s -n --eval %s"
                          emacsclient
                          (shell-quote-argument
                           (format "(let ((frame (or (car (seq-filter #'display-graphic-p (frame-list))) (selected-frame)))) (select-frame-set-input-focus frame) (raise-frame frame) (pop-to-buffer %S))"
                                   buffer-name)))))
           (t (list "-activate" "org.gnu.Emacs")))))
    (apply #'call-process "terminal-notifier" nil nil nil
           "-title" title
           "-message" message
           "-sound" "Ping"
           click-args)))

(setq claude-code-notification-function #'my-claude-notify)

;; Display Claude in a sensible window:
;;  - If the frame has only one window, split it to the right at 50%.
;;  - Otherwise, take over the currently selected window (avoids crushing
;;    a small pane when splitting by a fraction of the *frame* width).
(defun my-claude-display-right (buffer)
  "Display Claude BUFFER intelligently based on current window layout."
  (let ((window (if (one-window-p)
                    (display-buffer buffer '((display-buffer-in-direction)
                                             (direction . right)
                                             (window-width . 0.5)))
                  (display-buffer buffer '((display-buffer-same-window))))))
    ;; Send SIGWINCH to force TUI to recalculate layout
    (run-with-timer 0.2 nil
      (lambda (buf)
        (when-let ((proc (get-buffer-process buf)))
          (signal-process proc 'SIGWINCH)))
      buffer)
    window))
(setq claude-code-display-window-fn #'my-claude-display-right)

(defun my-claude-code-force-relayout ()
  "Force a Claude Code TUI relayout by briefly maximizing its window.
Works because we disabled `claude-code-optimize-window-resize' —
both the maximize and the restore now propagate as real resize
events that vterm forwards to claude."
  (interactive)
  (let ((win (seq-find
              (lambda (w)
                (string-prefix-p "*claude:" (buffer-name (window-buffer w))))
              (window-list))))
    (cond
     ((not win)
      (message "No visible Claude Code buffer"))
     ((one-window-p win)
      (message "Claude is already the only window — split (C-x 3) and back to reflow"))
     (t
      (let ((config (current-window-configuration)))
        (select-window win)
        (delete-other-windows)
        (redisplay t)
        (set-window-configuration config))))))

(define-key claude-code-command-map (kbd "l") #'my-claude-code-force-relayout)

;; Claude Code IDE — disabled for now (2026-03-26)
;; The emacs-tools (xref, imenu, treesit) are never used in practice since
;; Claude's built-in Grep/Glob/Read cover the same ground. The openDiff handler
;; also overrides bypass-permissions mode, causing unexpected edit prompts.
;; Re-enable if the emacs-tools become useful or the permissions issue is fixed.
;; (require 'claude-code-ide)
;; (global-set-key (kbd "C-c C-'") 'claude-code-ide-menu)
;; (claude-code-ide-emacs-tools-setup)

(setopt vterm-min-window-width 80)

(provide 'setup-claude-code)

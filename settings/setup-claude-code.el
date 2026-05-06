(require 'monet)
(require 'claude-code)
(require 'matisse)


;; Enable claude-code-mode
(claude-code-mode 1)

(setq claude-code-terminal-backend 'vterm)

;; Use Happy CLI instead of bare claude — sessions are visible from phone
(setq claude-code-program
      "/Users/gleitz/projects/happy/packages/happy-cli/bin/happy.mjs")
(setq claude-code-program-switches '("--dangerously-skip-permissions"))

;; Tell Happy where the server is
(add-hook 'claude-code-process-environment-functions
          (lambda (_buffer-name _directory)
            '("HAPPY_SERVER_URL=https://gleitz-happy.ngrok.app")))

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

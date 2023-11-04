;; Install the following
;; pip install pylint elpy flake8 pyyaml flake8_nb
;; npm install -g jshint
;; npm install -g jsxhint
;; npm install -g tern
;; pip install -U /Users/gleitz/.emacs.d/elpa/jedi-core-20191011.1750

;; if elpy complains
;; trash /Users/gleitz/.emacs.d/elpy/rpc-venv
;; M-x elpy-config

;; Default indentation levels
(setq js-indent-level 2)
(setq ruby-indent-level 2)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

;; Keyboard modifications
(global-set-key (kbd "M-4") 'ispell-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "C-c d") 'desktop-change-dir)
(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key (kbd "C-x i") 'find-grep-dired)
(global-set-key (kbd "C-x n") 'find-name-dired)
(global-set-key (kbd "<S-down>") 'ff/comment-and-go-down)
(global-set-key (kbd "<S-up>") 'ff/uncomment-and-go-up)

;; Flycheck
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-error-display-timer nil)

;; Infinite pop mark
(setq set-mark-command-repeat-pop t)

;; No backups, please
(setq auto-save-default nil)

;; Edit the buffer as super user
(defun sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

;; Remove console.log statements from the current buffer
(defun console-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274console.log" 0 "%d")) 100))

;; Remove debugger; statements from the current buffer
(defun debugger-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274debugger;" 0 "%d")) 100))

;; Remove alert() statements from the current buffer
(defun alert-remove ()
  (interactive)
  (kmacro-exec-ring-item (quote ("\274alert(" 0 "%d")) 100))

;; Collapse whitespace between two lines
(defun collapse-whitespace ()
  (interactive)

  )
(fset 'collapse-whitespace
      [left ?\C-\M-s ?\[ ?\[ ?: ?g ?r ?a ?p ?h ?: ?\] ?\] ?\C-m left ?\C-  ?\S-\C-\M-r ?\[ ?\[ ?: ?g ?r ?a ?p ?h ?: ?\] ?\] ?\C-m right ?\C-x ?\C-k])
(global-set-key "\C-x\C-j" 'collapse-whitespace)

(defun tidy ()
  "Tidy up a buffer by replacing all special unicode characters
   (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u2032|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

;; Comments and goes down ARG lines
(defun ff/comment-and-go-down (arg)
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (ff/comment-and-go-down (1- arg))))

;; Uncomments and goes up ARG lines
(defun ff/uncomment-and-go-up (arg)
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (ff/uncomment-and-go-up (1- arg))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ))

;; Find init file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Cleanup buffer when saving
(add-hook 'before-save-hook
                   (lambda ()
                     (cleanup-buffer-safe)))

;; Let me open large files
(setq large-file-warning-threshold 300000000)

;; Zone when away
(require 'zone)
;; Note: zoning can kill battery
;; (zone-when-idle (* 60 60))

;; Enable emojis
(add-hook 'text-mode-hook 'emojify-mode)

;; PHP
(add-to-list 'auto-mode-alist '("[^.][^t][^p][^l]\\.php$" . web-mode))
(add-hook 'php-mode-hook '(lambda ()
  (flycheck-mode 1)
))

;; Disable vc-mode
(setq vc-handled-backends nil)

;; Useful commands
;; Skip to next tag sgml-skip-tag-forward (C-c C-f/b)

;; Large file handling
(defun disable-all-minor-modes ()
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (when (functionp mode-symbol)
       ;; some symbols are functions which aren't normal mode functions
       (ignore-errors
         (funcall mode-symbol -1))))
   minor-mode-list))

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024 5))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; Enable mic-paren
(require 'mic-paren)
(paren-activate)

;; Extract sequence numbers
(fset 'extract-sequence-number
   [?\C-s ?s ?e ?q ?u ?e ?n ?c ?e ?n ?u ?m return ?\C-a ?\C-  ?\C-e ?\M-w ?\C-x ?o ?\C-y return ?\C-x ?o down ?\C-a])

;; Update packages on exit
(defadvice save-buffers-kill-terminal (before save-buffers-kill-terminal-before activate)
  (when (display-graphic-p)
    (if (equal current-prefix-arg '(4)) ;; This means C-u
        (package-utils-upgrade-all)
      (byte-compile-dotfiles))))

;; A button for completion
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

(global-set-key [backtab] 'tab-indent-or-complete)

;; Always find your init file
(global-set-key [f7] (lambda () (interactive) (find-file user-init-file)))

;; Always find your mark
(beacon-mode)

;; Increment numbers
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; Seems to be an issue with dired and magit
;; if: Autoloading file /Applications/Emacs.app/Contents/Resources/lisp/dired.elc failed to define function dired-jump
(autoload 'dired-jump "dired")

;; Tumblr
(setq tumblesocks-blog "gleitzman.tumblr.com")
(setq oauth-nonce-function 'oauth-internal-make-nonce)
(defun oauth-build-signature-hmac-sha1 (req secret)
  "Returns the signature for the given request object"
  (let* ((token (oauth-request-token req))
         (key (concat secret "&" (when token (oauth-t-token-secret token))))
         (hmac-params
          (list (string-to-unibyte (encode-coding-string key 'utf-8 t))
                (string-to-unibyte (encode-coding-string
                 (oauth-build-signature-basestring-hmac-sha1 req) 'utf-8 t)))))
    (if oauth-hmac-sha1-param-reverse (setq hmac-params (reverse hmac-params)))
    (base64-encode-string (apply 'hmac-sha1 hmac-params))))

;; ripgrep default args
;; bug: seem to make it so that you can't select the results to jump to the file
;; (setq ripgrep--base-arguments '("--line-number" "--with-filename"))

;; Be able to go back to the last change
;;; record two different file's last change. cycle them
(defvar feng-last-change-pos1 nil)
(defvar feng-last-change-pos2 nil)

(defun feng-swap-last-changes ()
  (when feng-last-change-pos2
    (let ((tmp feng-last-change-pos2))
      (setf feng-last-change-pos2 feng-last-change-pos1
            feng-last-change-pos1 tmp))))

(defun feng-goto-last-change ()
  (interactive)
  (when feng-last-change-pos1
    (let* ((buffer (find-file-noselect (car feng-last-change-pos1)))
           (win (get-buffer-window buffer)))
      (if win
          (select-window win)
        (switch-to-buffer-other-window buffer))
      (goto-char (cdr feng-last-change-pos1))
      (feng-swap-last-changes))))

(defun feng-buffer-change-hook (beg end len)
  (let ((bfn (buffer-file-name))
        (file (car feng-last-change-pos1)))
    (when bfn
      (if (or (not file) (equal bfn file)) ;; change the same file
          (setq feng-last-change-pos1 (cons bfn end))
        (progn (setq feng-last-change-pos2 (cons bfn end))
               (feng-swap-last-changes))))))

(add-hook 'after-change-functions 'feng-buffer-change-hook)
;;; just quick to reach
(global-set-key (kbd "M-`") 'feng-goto-last-change)

;; Allow highlighting of symbols
(require 'symbol-overlay)
(defun enable-symbol-overlay-mode ()
  (unless (or (minibufferp)
              (derived-mode-p 'magit-mode)
              (derived-mode-p 'xref--xref-buffer-mode))
    (symbol-overlay-mode t)))
(define-global-minor-mode global-symbol-overlay-mode ;; name of the new global mode
  symbol-overlay-mode                                ;; name of the minor mode
  enable-symbol-overlay-mode)
(global-symbol-overlay-mode)                         ;; enable it
(global-set-key (kbd "s-`") 'symbol-overlay-put)
(setq symbol-overlay-ignore-functions nil)           ;; don't ignore keywords in various languages
;; (setq symbol-overlay-map (make-sparse-keymap))       ;; disable special cmds on overlays

(defun my-reverse-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

;; Allow editing within Emacs from the browser
(atomic-chrome-start-server)

;; Randomize lines
(defun my-random-sort-lines (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))
(global-set-key (kbd "C-c r") 'my-random-sort-lines)

;; extract all matches of regex
(defun show-matches (regex string)
  "Show all matches of REGEX in STRING."
  (let ((pos 0))
    (while (string-match regex string pos)
      (setq pos (match-end 0))
      (message "%s" (match-string 0 string)))))

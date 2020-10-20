;; Seed the random-number generator
(random t)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("Urban Dictionary" .
                             [simple-query
                              "www.urbandictionary.com"
                              "http://www.urbandictionary.com/define.php?term="
                              ""])))

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 - >") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))

;; Add JSP expansions to html-mode
(eval-after-load "sgml-mode" '(require 'jsp-expansions))

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

;; Uniquify lines
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;; Open github pages from files
(defun browse-on-github (prefix-arg)
  "Show the current file on github"
  (interactive "P")
  (browse-url
   (trim-string
    (shell-command-to-string
     (format "python %s %s %d %d %s"
             (expand-file-name "~/.emacs.d/lib/python/gleitzpy/gitopener.py")
             (buffer-file-name)
             (if mark-active (line-number-at-pos (region-beginning))
               (string-to-number (replace-regexp-in-string "Line " "" (what-line))))
             (if mark-active (line-number-at-pos (region-end))
               0)
             (if prefix-arg "upstream"
               ""))))))

;; I know that string is in my buffer somewhere
(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)

(defun my-isearch-buffers ()
  "isearch multiple buffers."
  (interactive)
  (multi-isearch-buffers
   (delq nil (mapcar (lambda (buf)
                       (set-buffer buf)
                       (and (not (equal major-mode 'dired-mode))
                            (not (string-match "^[ *]" (buffer-name buf)))
                            buf))
                     (buffer-list)))))
(global-set-key (kbd "M-s \\") 'my-isearch-buffers)

;; Don't lose that scratch buffer
(persistent-scratch-autosave-mode 1)

;; Load scratch from Dropbox
(setq scratch-buffer-path
      (loop for filename in '("~/Dropbox/Personal/documents/scratch.txt")
            when (file-exists-p filename) collect filename into valid-files
            finally return (car valid-files)))
(setq initial-buffer-choice scratch-buffer-path)

;; Scroll to the end of the scratch buffer
(defun end-of-scratch ()
  (set-window-point (get-buffer-window (file-name-nondirectory scratch-buffer-path)) 10000000000))
(kill-buffer "*scratch*")
(run-at-time "1 sec" nil 'end-of-scratch)

(provide 'my-misc)

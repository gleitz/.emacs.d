;; C-c d (documentation)
;; C-c g (jump to definition)
;; C-c f (find occurances)
;; M-/ (code assist)

(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" (expand-file-name "~/.emacs.d/lib/python")))

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(require 'python)
(require 'pymacs)
(setq pymacs-loaded 'nil)

(defun pymacs-restart ()
  (interactive)
  (progn
    (pymacs-load "ropemacs" "rope-")
    (setq pymacs-loaded t)))

(defun my-python-hook-mode ()
  (interactive)
  (if (not pymacs-loaded)
      (progn
        (autoload 'pymacs-apply "pymacs")
        (autoload 'pymacs-call "pymacs")
        (autoload 'pymacs-eval "pymacs" nil t)
        (autoload 'pymacs-exec "pymacs" nil t)
        (autoload 'pymacs-load "pymacs" nil t)
        (pymacs-restart)))

  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-guess-project 1)
  (ropemacs-mode 1)
  (require 'auto-complete)
  (global-auto-complete-mode 1)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-rope)))
  (set (make-local-variable 'ac-find-function) 'ac-python-find)
  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
  (set (make-local-variable 'ac-auto-start) nil)
  (define-key python-mode-map [(control tab)] 'gleitz-python-tab)
  (define-key ropemacs-local-keymap [(meta ??)] 'rope-code-assist)
  (define-key ropemacs-local-keymap [(meta /)] 'hippie-expand)
  ;; (define-key python-mode-map [(control up)] 'beginning-of-defun)
  ;; (define-key python-mode-map [(control down)] 'end-of-defun)
  )
(add-hook 'python-mode-hook 'my-python-hook-mode)

;; Try the following:
;; 1) Do a yasnippet expansion
;; 2) Do a Rope code completion
;; 3) Do an indent
(defun gleitz-python-tab ()
  (interactive)
  (if (eql (ac-start) nil)
      (indent-for-tab-command)))
(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(provide 'setup-ropemacs)

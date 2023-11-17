(require 'obsidian)
(obsidian-specify-path "~/Dropbox/Personal/documents/obsidian")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "Gleitzvault")
(setq obsidian-daily-notes-directory "Gleitzvault/notes")

;; Define obsidian-mode bindings
(add-hook
 'obsidian-mode-hook
 (lambda ()
   ;; Replace standard command with Obsidian.el's in obsidian vault:
   (local-set-key (kbd "C-c C-o") 'obsidian-follow-link-at-point)

   ;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
   (local-set-key (kbd "C-c C-l") 'obsidian-insert-wikilink)

   ;; Following backlinks
   (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump)))

;; Optionally you can also bind `obsidian-jump' and `obsidian-capture'
;; replace "YOUR_BINDING" with the key of your choice:
;; (global-set-key (kbd "YOUR_BINDING") 'obsidian-jump)
;; (global-set-key (kbd "YOUR_BINDING") 'obsidian-capture)

;; Activate detection of Obsidian vault
(global-obsidian-mode t)

(defun obsidian-capture-with-title ()
  "Create new obsidian daily note with a specific title.
Fill in the buffer with a proper Markdown heading based on the name.
In the `obsidian-daily-notes-directory' if set otherwise in `obsidian-inbox-directory' - if that's also unset,
in `obsidian-directory' root.
."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         ;; create a full-title with date and title, replacing any spaces with dashes
         (full-title (s-concat (format-time-string "%Y-%m-%d") "-" (s-replace " " "-" title)))
         (filename (s-concat obsidian-directory "/" obsidian-inbox-directory "/" full-title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    ;; capitalize first letter of each word in title
    (insert (format "# %s\n" (s-titleize title)))
    (save-buffer)
    (add-to-list 'obsidian-files-cache clean-filename)))

(defun obsidian-daily-note-with-title ()
  "Create new obsidian daily note with a specific title.
Fill in the buffer with a proper Markdown heading based on the name.
In the `obsidian-daily-notes-directory' if set otherwise in `obsidian-inbox-directory' - if that's also unset,
in `obsidian-directory' root.
."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         ;; create a full-title with date and title, replacing any spaces with dashes
         (full-title (s-concat (format-time-string "%Y-%m-%d") "-" (s-replace " " "-" title)))
         (filename (s-concat obsidian-directory "/" obsidian-daily-notes-directory "/" full-title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    ;; capitalize first letter of each word in title
    (insert (format "# %s\n" (s-titleize title)))
    (save-buffer)
    (add-to-list 'obsidian-files-cache clean-filename)))

(provide 'setup-obsidian)

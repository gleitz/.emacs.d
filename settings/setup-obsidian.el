(require 'obsidian)
(obsidian-specify-path "~/Dropbox/Personal/documents/obsidian/Gleitzvault")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "inbox")
(setq obsidian-daily-notes-directory "notes")

;; Define obsidian-mode bindings
(add-hook
 'obsidian-mode-hook
 (lambda ()
   ;; Replace standard command with Obsidian.el's in obsidian vault:
   (local-set-key (kbd "C-c C-o") 'obsidian-follow-link-at-point)

   ;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
   (local-set-key (kbd "C-c C-l") 'obsidian-insert-wikilink)

   (local-set-key (kbd "C-c C-t") 'obsidian-insert-task)

   ;; Following backlinks
   (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump)))

;; Optionally you can also bind `obsidian-jump' and `obsidian-capture'
;; replace "YOUR_BINDING" with the key of your choice:
;; (global-set-key (kbd "YOUR_BINDING") 'obsidian-jump)
;; (global-set-key (kbd "YOUR_BINDING") 'obsidian-capture)

;; Activate detection of Obsidian vault
(global-obsidian-mode t)

(defun obsidian-insert-task ()
  (interactive)
  (insert "- [ ] "))

(defun obsidian-capture-with-title (&optional title dir)
  "Create new obsidian note with a specific title and move it to the specified directory.
If TITLE is provided, use it; otherwise, prompt for a title.
If DIR is provided, use it; otherwise, use the default inbox directory."
  (interactive)
  (let* ((title (or title (read-from-minibuffer "Title: ")))
         (dir (or dir obsidian-inbox-directory))
         (full-title (s-concat (format-time-string "%Y-%m-%d") "-" (s-replace " " "-" title)))
         (filename (s-concat obsidian-directory "/" dir "/" full-title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
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

;; Add emacs advice after calling obsidian-daily-note function to insert `# %Y-%m-%d` at the top of the file
(defun obsidian-insert-date ()
  (interactive)
  (insert (format-time-string "# Daily Note %Y-%m-%d\n"))
  (save-buffer))
(advice-add 'obsidian-daily-note :after #'obsidian-insert-date)

;; Due to weirdness with Dropbox folders (existing at ~/Dropbox and ~/Library/CloudStorage/Dropbox)
;; we have to modify this script slightly
(defun obsidian-file-p (&optional file)
  "Return t if FILE is an obsidian.el file, nil otherwise.

If FILE is not specified, use the current buffer's file-path.
FILE is an Org-roam file if:
- It's located somewhere under `obsidian-directory
- It is a markdown .md file
- Is not a dot file or, if `obsidian-include-hidden-files' is t, then:
  - It is not in .trash
  - It is not an Emacs temp file"
  (-when-let* ((path (s-replace "Library/CloudStorage/Dropbox" "Dropbox" (or file buffer-file-name)))
               (relative-path (file-relative-name path obsidian-directory))
               (ext (file-name-extension relative-path))
               (md-p (string= ext "md"))
               (obsidian-dir-p (obsidian-descendant-of-p path obsidian-directory))
               (not-dot-file (or obsidian-include-hidden-files (not (obsidian-dot-file-p path))))
               (not-trash-p (obsidian-not-trash-p path))
               (not-dot-obsidian (obsidian-not-dot-obsidian-p path))
               (not-temp-p (not (s-contains-p "~" relative-path))))
    t))

(defun capture-and-move-jen ()
  "Capture a note for Jen and move it to the coaching folder."
  (interactive)
  (obsidian-capture-with-title "jen" "coaching/jen"))

(defun capture-and-move-jen ()
  "Capture a note for Jen and move it to the coaching folder."
  (interactive)
  (obsidian-capture-with-title "greg" "coaching/greg"))

(provide 'setup-obsidian)

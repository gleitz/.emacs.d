(defun hugo-new-blog-post (&optional title)
  "Create new Hugo blog post with a specific title.
If TITLE is provided, use it; otherwise, prompt for a title.
Creates a file in the format YYYY-MM-DD-<slugified-title>.md with YAML frontmatter."
  (interactive)
  (let* ((title (or title (read-from-minibuffer "Blog post title: ")))
         (date-prefix (format-time-string "%Y-%m-%d"))
         (slugified-title (downcase (s-replace " " "-" title)))
         (filename (format "%s-%s.md" date-prefix slugified-title))
         (full-path (expand-file-name filename "/Users/gleitz/projects/transmogrify/blog/content/posts"))
         (current-datetime (format-time-string "%Y-%m-%dT%H:%M:%S")))
    (find-file full-path)
    (insert (format "---\ndate: '%s'\ntags:\n- \ntitle: \"%s\"\n---\n\n"
                    current-datetime
                    title))
    (save-buffer)
    (message "Created blog post: %s" filename)))

(defun hugo-deploy ()
  "Generate the Hugo blog and publish it via rsync.
Runs 'hugo --minify' to build the site, then syncs to the remote server."
  (interactive)
  (let ((default-directory "/Users/gleitz/projects/transmogrify/blog/"))
    (message "Building Hugo site...")
    (let ((build-result (shell-command "hugo --minify")))
      (if (= build-result 0)
          (progn
            (message "Hugo build successful. Deploying...")
            (let ((deploy-result (shell-command "rsync -avzP /Users/gleitz/projects/transmogrify/blog/public/ g:/var/www/hugoblog/")))
              (if (= deploy-result 0)
                  (message "Hugo site deployed successfully!")
                (message "Error during deployment. Check rsync output."))))
        (message "Hugo build failed. Check output for errors.")))))

(provide 'setup-hugo)

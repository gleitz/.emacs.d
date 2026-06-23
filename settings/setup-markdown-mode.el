(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"   "^# \\(.*\\)$" 1)
        ("h2"   "^## \\(.*\\)$" 1)
        ("h3"   "^### \\(.*\\)$" 1)
        ("h4"   "^#### \\(.*\\)$" 1)
        ("h5"   "^##### \\(.*\\)$" 1)
        ("h6"   "^###### \\(.*\\)$" 1)
        ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
        ))

(defun my-markdown-mode-hook ()
  (when (not (member major-mode '(forge-post-mode)))
    (flycheck-mode +1)
    (flycheck-select-checker 'markdown-markdownlint-cli)
    (flyspell-mode +1))
  (setq imenu-generic-expression markdown-imenu-generic-expression))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(define-key markdown-mode-map (kbd "M-<right>") 'subword-right)
(define-key markdown-mode-map (kbd "M-<left>") 'subword-left)
(define-key markdown-mode-map (kbd "C-_") 'undo)

(defun markdown-browser-preview ()
  (interactive)
  (unless (get-process "httpd")
    (httpd-start))
  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients)
  ;; open the browser navigating to the current filename we're viewing
  (browse-url (concat "http://localhost:8080/imp/live/" (buffer-name))))

;; The original markdown -> HTML viewer
(defun markdown-html (buffer)
  "Convert the contents of the given BUFFER from Markdown with wikilinks to HTML."
  (with-current-buffer buffer
    (let ((content (buffer-string))) ;; Get the content of the buffer
      (with-temp-buffer ;; Work in a temporary buffer to avoid changes in the original one
        (insert content) ;; Insert the original content into the temp buffer
        (goto-char (point-min))
        ;; Convert wikilinks like [[https://platform.openai.com/docs/guides/fine-tuning/preparing-your-dataset]] to <a> links like <a target="_blank" href="https://platform.openai.com/docs/guides/fine-tuning/preparing-your-dataset">https://platform.openai.com/docs/guides/fine-tuning/preparing-your-dataset</a> (be sure to remove everything after the | in the wikilink)
        (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\]" nil t)
          (replace-match "<a target=\"_blank\" href=\"\\1\">\\1</a>"))
        ;; Return the resulting HTML string
        (princ (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
                       (buffer-substring-no-properties (point-min) (point-max))))))))

;; Github flavored HTML preview
;; (defun markdown-html (buffer)
;;   (princ (with-current-buffer buffer
;;            (format "<!DOCTYPE html><html><script src=\"https://cdnjs.cloudflare.com/ajax/libs/he/1.1.1/he.js\"></script><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/github-e6bb18b320358b77abe040d2eb46b547.css\"><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/frameworks-95aff0b550d3fe338b645a4deebdcb1b.css\"><title>Impatient Markdown</title><div id=\"markdown-content\" style=\"display:none\">%s</div><div class=\"markdown-body\" style=\"max-width:968px;margin:0 auto;\"></div><script>fetch('https://api.github.com/markdown', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ \"text\": document.getElementById('markdown-content').innerHTML, \"mode\": \"gfm\", \"context\": \"knit-pk/homepage-nuxtjs\"}) }).then(response => response.text()).then(response => {document.querySelector('.markdown-body').innerHTML = he.decode(response)}).then(() => { fetch(\"https://gist.githubusercontent.com/FieryCod/b6938b29531b6ec72de25c76fa978b2c/raw/\").then(response => response.text()).then(eval)});</script></html>"
;;                    (buffer-substring-no-properties (point-min) (point-max))))
;;          (current-buffer)))

(require 'xml)

;; Call `markdown-export`, open the resulting HTML file, and then remove all content that is not within the <body> tag
(defun markdown-export-and-open ()
  (interactive)
  ;; Export markdown to HTML.
  (markdown-export)
  ;; Construct HTML file path.
  (let* ((md-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension md-file) ".html"))
         title) ;; Declare title variable
    ;; Open HTML file in Emacs.
    (find-file html-file)
    ;; Modify HTML content.
    (goto-char (point-min))
    ;; Extract and delete <h1> content.
    (when (re-search-forward "<h1>\\(.*?\\)</h1>" nil t)
      (setq title (decode-html (match-string 1))) ;; Save <h1> content to title
      (replace-match "") ;; Delete <h1> tag
      (kill-new title)) ;; Copy title to clipboard
    ;; Delete content up to first <p> tag
    (goto-char (point-min))
    (when (search-forward "<p>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (when (search-forward "</body>" nil t)
      (backward-char (length "</body>"))
      (delete-region (point) (point-max)))
    ;; Save and close the buffer.
    (save-buffer)
    ;; (kill-buffer)
    ;; Open HTML file in the browser.
    (browse-url (concat "file://" html-file))))

(defun decode-html (html-string)
  "Decode HTML entities in a string."
  (with-temp-buffer
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))
    (buffer-string)))

;; GitHub-style markdown preview with dark mode support
(setq markdown-css-paths
      '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css"))

(setq markdown-xhtml-header-content
      "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css\" media=\"(prefers-color-scheme: dark)\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css\" media=\"(prefers-color-scheme: light)\" />
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js\"></script>
<style>
        body {
          box-sizing: border-box;
          min-width: 200px;
          max-width: 980px;
          margin: 0 auto;
          padding: 45px;
        }
        @media (prefers-color-scheme: dark) {
          body { background-color: #0d1117; }
        }
        @media (prefers-color-scheme: light) {
          body { background-color: #ffffff; }
        }
        html { scroll-behavior: smooth; }
        /* Code block line stripes */
        pre {
          background-image: repeating-linear-gradient(
            180deg, transparent 0, transparent 1.6em,
            rgba(128,128,128,0.06) 1.6em, rgba(128,128,128,0.06) 3.2em);
          background-size: 100% 3.2em;
          background-position: 0 16px;
        }
        pre code.hljs { padding: 16px; line-height: 1.6; }
        /* Full-width tables */
        .markdown-body table { display: table; width: 100%; }
        /* Heading anchors */
        .heading-anchor {
          opacity: 0; text-decoration: none; margin-left: 0.3em;
          font-weight: normal; color: inherit;
        }
        h1:hover .heading-anchor, h2:hover .heading-anchor,
        h3:hover .heading-anchor, h4:hover .heading-anchor { opacity: 0.5; }
        .heading-anchor:hover { opacity: 1 !important; }
        h2[id], h3[id], h4[id] { scroll-margin-top: 16px; }
      </style>")

(setq markdown-xhtml-body-preamble "<article class=\"markdown-body\">")
(setq markdown-xhtml-body-epilogue "</article>
<script>
  hljs.highlightAll();
  document.querySelectorAll('h1,h2,h3,h4').forEach(function(h) {
    var id = h.textContent.trim().toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-|-$/g,'');
    h.id = id;
    var a = document.createElement('a');
    a.href = '#' + id;
    a.className = 'heading-anchor';
    a.textContent = '#';
    h.appendChild(a);
  });
</script>")

;; Mermaid: higher quality rendering
(setq mermaid-output-format ".png")
(setq mermaid-flags "-s 2 -w 4096 -b white")

(provide 'setup-markdown-mode)

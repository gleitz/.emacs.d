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
  (when (not (member major-mode '(forge-post-mode tumblesocks-compose-mode)))
    (flycheck-mode +1)
    (flycheck-select-checker 'markdown-markdownlint-cli))
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

(provide 'setup-markdown-mode)

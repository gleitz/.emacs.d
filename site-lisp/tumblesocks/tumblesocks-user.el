;; tumblesocks-user.el -- higher-level functions for the tumblr api

(require 'tumblesocks-api)
(provide 'tumblesocks-user)

(defcustom tumblesocks-post-default-state "published"
  "Change the default state of your newly created posts.

Use 'ask' to prompt the user each time a post is created."
  :type '(choice (const :tag "Published" "published")
                 (const :tag "Draft" "draft")
                 (const :tag "Queue" "queue")
                 (const :tag "Private" "private")
                 (const :tag "Schedule" "schedule")
                 (const :tag "Ask" "ask"))
  :group 'tumblesocks)

(defun tumblesocks-get-post-state (&optional state)
  "If the default post state is 'ask', then this function asks
the user what to do for each post."
  (interactive)
  (if (or (string= (downcase tumblesocks-post-default-state) "ask")
          (and (not (eq nil state))
               (string= (downcase state) "ask")))
      (completing-read "State: "
                       '("published"
                         "draft"
                         "queue"
                         "private"
                         "schedule") nil t)
    tumblesocks-post-default-state))

(defun tumblesocks-follow-blog (blog)
  "Follow the given Tumblr blog"
  (interactive "sTumblr blog to follow (URL): ")
  (tumblesocks-api-user-follow blog)
  (message (concat "Now following " blog)))

(defun tumblesocks-unfollow-blog (blog-url)
  "Unfollow a certain Tumblr blog on your list, with tab completion."
  (interactive (list (completing-read
                      "Blog URL to unfollow (TAB to complete): "
                      (let ((bloglist (plist-get (tumblesocks-api-user-following)
                                                 :blogs)))
                        (mapcar #'(lambdao (blog)
                                   (plist-get blog :url))
                                bloglist))
                      nil t)))
  (tumblesocks-api-user-unfollow blog-url)
  (message (concat "No longer following " blog-url)))

(defun tumblesocks-text-post-from-region (begin end title &optional tags state)
  "Create a new Tumblr markdown text post from the given region, returning the ID and copying the URL to the clipboard."
  (interactive "r\nsTitle: \nsTags (optional, comma separated): ")
  (when (and tags (string= tags "")) (setq tags nil))
  ;;(when (string= title "") (error "You must provide a title."))
  (let ((args (append
               `(:type "text"
                 :format "markdown"
                 :state ,(tumblesocks-get-post-state state)
                 :body ,(buffer-substring begin end)
                 :title ,title)
               (and tags `(:tags ,tags)))))
    (if (string= (plist-get args :state) "schedule")
        (progn
          (plist-put args :state "queue")
          (plist-put args :publish_on (read-string "Publish On: "))))
    (let* ((blog-url
            (plist-get (plist-get (tumblesocks-api-blog-info) :blog) :url))
           (new-post-id (format "%d" (plist-get (tumblesocks-api-new-post args)
                                                :id)))
           (new-post-url
            (let* ((last-char (substring blog-url -1)))
              (cond ((string= last-char "/")
                     (concat blog-url new-post-id)) ; url has a trailing slash
                    (t (concat blog-url (concat "/" new-post-id)))))))
      ;; So we need to both return this ID
      ;; and copy the URL to the clipboard (and message it too.)
      ;; Thanks to tumble.el for this:
      (kill-new new-post-url)
      (message (concat "New post created at " new-post-url))
      new-post-id)))

(defun tumblesocks-text-post-from-buffer (title &optional tags)
  "Create a new Tumblr markdown text post from the current buffer, returning the ID and copying the URL to the clipboard."
  (interactive "sTitle: \nsTags (optional, comma separated): ")
  (tumblesocks-text-post-from-region (point-min) (point-max) title tags))

(provide 'tumblesocks-user)
;;; tumblesocks-user.el ends here

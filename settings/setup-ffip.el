(require 'find-file-in-project)
(require 's)

;; Helper methods to create local settings

(defun ffip--create-exclude-find-options (names)
  (mapconcat (lambda (name)
               (concat "-not -regex \".*" name ".*\"")) names " "))

(defun ffip-local-excludes (&rest names)
  "Given a set of names, will exclude results with those names in the path."
  (set (make-local-variable 'ffip-find-options)
       (ffip--create-exclude-find-options names)))

(defun ffip-local-patterns (&rest patterns)
  "An exhaustive list of file name patterns to look for."
  (set (make-local-variable 'ffip-patterns) patterns))

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (let ((pattern-list patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns pattern-list))
        (find-file-in-project)))))

;; Default excludes - override with ffip-local-excludes

(setq-default ffip-find-options
              (ffip--create-exclude-find-options
               '("/node_modules"
                 "/bower_components"
                 "/target"
                 "/out"
                 "/overlays"
                 "/build"
                 "/dist"
                 "/vendor"
                 ".cask"
                 "/generated"
                 "/resources/public/js/compiled"
                 "/.repl"
                 "/.git"
                 "/.tmp"
                 "/.idea"
                 "/.meteor"
                 "/.beans")))

(provide 'setup-ffip)

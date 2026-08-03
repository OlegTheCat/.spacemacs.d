;;; projectile-cache.el --- Cached Projectile file search -*- lexical-binding: t -*-

;; Keep project file lists for one hour.  The cache is transient, so it is
;; rebuilt after restarting Emacs even if the hour has not elapsed.
(setq projectile-enable-caching t
      projectile-files-cache-expire (* 60 60))

(defun my/helm-projectile-find-file (&optional refresh)
  "Find a project file, invalidating Projectile's cache when REFRESH is non-nil.

Interactively, use plain `M-h p f' for the cached search and
`C-u M-h p f' to rebuild the current project's file list first."
  (interactive "P")
  (when refresh
    (projectile-invalidate-cache nil))
  ;; REFRESH belongs to this wrapper; do not leak it into Helm Projectile.
  (let ((current-prefix-arg nil))
    (call-interactively #'helm-projectile-find-file)))

(defun my/projectile-cache-bind-keys ()
  "Route Spacemacs project-file search through the cache-aware wrapper."
  (spacemacs/set-leader-keys "pf" #'my/helm-projectile-find-file))

(with-eval-after-load 'helm-projectile
  (my/projectile-cache-bind-keys))

(provide 'projectile-cache-config)

;;; projectile-cache.el ends here

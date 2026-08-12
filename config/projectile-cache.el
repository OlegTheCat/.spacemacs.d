;;; projectile-cache.el --- Cached Projectile file search -*- lexical-binding: t -*-

(require 'seq)

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

(defconst my/projectile-start-files
  '("README.md" "AGENTS.md" "CLAUDE.md")
  "Root-level files to prefer when entering a project without a live buffer.")

(defun my/projectile--start-file (root)
  "Return the preferred landing file for project ROOT, relative to ROOT."
  (or (seq-find
       (lambda (file)
         (file-exists-p (expand-file-name file root)))
       my/projectile-start-files)
      (car (projectile-project-files root))))

(defun my/projectile-switch-to-last-buffer-or-file ()
  "Switch to the project's most recent buffer or open a useful landing file."
  (let ((buffer (car (projectile-project-buffers-non-visible))))
    (if buffer
        (switch-to-buffer buffer nil t)
      (let* ((root (projectile-acquire-root))
             (file (my/projectile--start-file root)))
        (if file
            (find-file (expand-file-name file root))
          (user-error "Project %s has no files"
                      (abbreviate-file-name root)))))))

(setq projectile-switch-project-action
      #'my/projectile-switch-to-last-buffer-or-file)

(defun my/projectile-cache-bind-keys ()
  "Install custom Projectile bindings."
  (when (fboundp 'spacemacs/set-leader-keys)
    (spacemacs/set-leader-keys "pf" #'my/helm-projectile-find-file))
  (global-set-key (kbd "s-p") #'helm-projectile-switch-project)
  (global-set-key (kbd "s-f") #'my/helm-projectile-find-file)
  (global-set-key (kbd "s-s") #'spacemacs/helm-project-smart-do-search)
  (global-set-key (kbd "s-g") #'magit-status))

(my/projectile-cache-bind-keys)

(provide 'projectile-cache-config)

;;; projectile-cache.el ends here

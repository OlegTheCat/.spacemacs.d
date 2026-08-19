;;; projectile.el --- Local Projectile behavior and shortcuts -*- lexical-binding: t -*-

(require 'seq)

(defvar projectile-known-projects nil)

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

(defun my/copy-file-path (&optional absolute)
  "Copy the current file's project-relative path.

With prefix argument ABSOLUTE, copy the full path instead.  In Dired, use the
file at point, matching `spacemacs/copy-file-path'.  When the file is not in a
Projectile project, fall back to its full path."
  (interactive "P")
  (if-let* ((file-path (or (spacemacs--file-path)
                           (and (derived-mode-p 'dired-mode)
                                (dired-get-filename nil t)))))
      (let* ((root (unless absolute
                     (ignore-errors
                       (projectile-project-root
                        (file-name-directory file-path)))))
             (copied-path (if root
                              (file-relative-name file-path root)
                            file-path)))
        (kill-new copied-path)
        (message "%s" copied-path))
    (user-error "Current buffer is not visiting a file")))

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

(defun my/projectile--text-buffer-p (buffer)
  "Return non-nil when BUFFER is a file-visiting textual buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and buffer-file-name
              (not (derived-mode-p 'special-mode))))))

(defun my/projectile-switch-to-last-buffer-or-file ()
  "Switch to the project's most recent text buffer or useful landing file."
  (let ((buffer
         (seq-find #'my/projectile--text-buffer-p
                   (projectile-project-buffers-non-visible))))
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

(defun my/projectile-promote-selected-project (project &rest _)
  "Move successfully selected PROJECT to the front of the known-project list."
  (let* ((project (file-name-as-directory (abbreviate-file-name project)))
         (projects (projectile-known-projects)))
    (when (and (member project projects)
               (not (equal project (car projects))))
      (setq projectile-known-projects
            (cons project (delete project (copy-sequence projects))))
      (projectile-merge-known-projects))))

(defun my/projectile-install-mru-tracking ()
  "Record successful Projectile selections as most recently used."
  (unless (advice-member-p #'my/projectile-promote-selected-project
                           #'projectile-switch-project-by-name)
    (advice-add #'projectile-switch-project-by-name :after
                #'my/projectile-promote-selected-project)))

(with-eval-after-load 'projectile
  (my/projectile-install-mru-tracking))

(defun my/projectile-bind-keys ()
  "Install custom Projectile bindings."
  (when (fboundp 'spacemacs/set-leader-keys)
    (spacemacs/set-leader-keys "pf" #'my/helm-projectile-find-file)
    (spacemacs/set-leader-keys "fyy" #'my/copy-file-path))
  (global-set-key (kbd "s-p") #'helm-projectile-switch-project)
  (global-set-key (kbd "s-f") #'my/helm-projectile-find-file)
  (global-set-key (kbd "s-s") #'spacemacs/helm-project-smart-do-search)
  (global-set-key (kbd "s-g") #'magit-status))

(my/projectile-bind-keys)

(provide 'projectile-config)

;;; projectile.el ends here

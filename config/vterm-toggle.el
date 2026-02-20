;;; vterm-toggle.el --- Multi-vterm project toggle -*- lexical-binding: t -*-

(require 'multi-vterm)

(defun my/vterm-project-bottom ()
  "Open project vterm at bottom, or a plain vterm if not in a project."
  (interactive)
  (let ((display-buffer-overriding-action '((display-buffer-in-side-window) (side . bottom) (window-height . 0.5))))
    (if (multi-vterm-project-root)
        (multi-vterm-project)
      (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
             (buf-name (format "*vterminal - %s*" (abbreviate-file-name dir)))
             (existing (get-buffer buf-name)))
        (if (and existing (buffer-live-p existing))
            (if (string-equal (buffer-name (current-buffer)) buf-name)
                (delete-window (selected-window))
              (switch-to-buffer-other-window existing))
          (let ((default-directory dir))
            (multi-vterm)
            (rename-buffer buf-name)))))))

;; Track all project vterm buffers: alist of (project-root . (buf1 buf2 ...))
(defvar my/vterm-project-buffers '()
  "Alist mapping project root to list of vterm buffers.")

(defun my/vterm-project-get-buffers (root)
  "Return live vterm buffers for ROOT, cleaning up dead ones."
  (let ((bufs (alist-get root my/vterm-project-buffers nil nil #'string-equal)))
    (setq bufs (seq-filter #'buffer-live-p bufs))
    (setf (alist-get root my/vterm-project-buffers nil 'remove #'string-equal) bufs)
    bufs))

(defun my/vterm-project-register (root buf)
  "Add BUF to the tracked vterm list for ROOT."
  (let ((bufs (my/vterm-project-get-buffers root)))
    (unless (memq buf bufs)
      (setf (alist-get root my/vterm-project-buffers nil 'remove #'string-equal)
            (append bufs (list buf))))))

(defun my/vterm-project-visible-windows (root)
  "Return windows displaying tracked vterm buffers for ROOT."
  (let ((bufs (my/vterm-project-get-buffers root)))
    (seq-filter (lambda (w) (memq (window-buffer w) bufs))
                (window-list))))

(defun my/vterm-project-toggle ()
  "Toggle all project vterms. With C-u prefix, split and add a new vterm."
  (interactive)
  (let ((root (multi-vterm-project-root)))
    (if (not root)
        ;; Fall back to original non-project behavior
        (my/vterm-project-bottom)
      (if current-prefix-arg
          (my/vterm-project-split root)
        (my/vterm-project-toggle-group root)))))

(defun my/vterm-project-show-buffers (bufs)
  "Display BUFS side-by-side in a bottom window."
  (let* ((display-buffer-overriding-action
          '((display-buffer-at-bottom) (window-height . 0.5)))
         (w (display-buffer (car bufs))))
    (dolist (buf (cdr bufs))
      (let ((new-w (split-window w nil 'right)))
        (set-window-buffer new-w buf)
        (setq w new-w)))
    w))

(defun my/vterm-project-toggle-group (root)
  "Toggle visibility of all tracked vterms for ROOT."
  (let ((visible (my/vterm-project-visible-windows root))
        (bufs (my/vterm-project-get-buffers root)))
    (cond
     ;; Vterms are visible → hide them all
     (visible
      (dolist (w visible) (delete-window w)))
     ;; Vterms exist but hidden → show them all side-by-side at bottom
     (bufs
      (select-window (my/vterm-project-show-buffers bufs)))
     ;; No vterms yet → create the first one
     (t
      (let ((display-buffer-overriding-action
             '((display-buffer-at-bottom) (window-height . 0.5))))
        (multi-vterm-project)
        (my/vterm-project-register root (current-buffer)))))))

(defun my/vterm-project-split (root)
  "Create a new vterm for ROOT and display all project vterms side-by-side."
  ;; Ensure at least one vterm exists
  (unless (my/vterm-project-get-buffers root)
    (let ((display-buffer-overriding-action
           '((display-buffer-at-bottom) (window-height . 0.5))))
      (multi-vterm-project)
      (my/vterm-project-register root (current-buffer))))
  ;; Hide existing vterm windows so we can rebuild the layout
  (let ((visible (my/vterm-project-visible-windows root)))
    (dolist (w visible) (delete-window w)))
  ;; Create the new vterm buffer (without displaying it yet)
  (let* ((default-directory root)
         (new-buf (save-window-excursion
                    (multi-vterm)
                    (current-buffer))))
    (my/vterm-project-register root new-buf))
  ;; Now show all tracked buffers side-by-side
  (let ((bufs (my/vterm-project-get-buffers root)))
    (select-window (my/vterm-project-show-buffers bufs))))

(global-set-key (kbd "s-i") 'my/vterm-project-toggle)

;;; vterm-toggle.el ends here

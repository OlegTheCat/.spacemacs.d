;;; ghostel.el --- Ghostel + Claude CLI sidebar -*- lexical-binding: t -*-

(require 'ghostel)

(defvar ghostel-claude--buffer-alist nil
  "Alist mapping project-root strings to ghostel+claude buffer objects.")

(defvar-local ghostel-claude--project-root nil
  "Project root this ghostel+claude buffer belongs to.")

(defvar ghostel-claude--last-window nil
  "Window that was selected before jumping to the sidebar.")

(defvar ghostel-claude-side 'right
  "Side of the frame for the Claude sidebar window.")

(defvar ghostel-claude-width 0.55
  "Width of the sidebar as a fraction of the frame.")

(defun ghostel-claude--project-root ()
  "Return the project root, or `default-directory' as fallback."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun ghostel-claude--get-buffer (root)
  "Return the live ghostel+claude buffer for ROOT, or nil."
  (let ((buf (alist-get root ghostel-claude--buffer-alist nil nil #'equal)))
    (when (and buf (buffer-live-p buf))
      buf)))

(defun ghostel-claude--get-window (root)
  "Return the window displaying the ghostel+claude buffer for ROOT, or nil."
  (let ((buf (ghostel-claude--get-buffer root)))
    (when buf (get-buffer-window buf t))))

(defun ghostel-claude--create (root &optional resume)
  "Create a new ghostel terminal running `claude' in ROOT.
When RESUME is non-nil, run `claude --resume' instead."
  (let* ((default-directory root)
         (cmd (if resume "claude --resume\n" "claude\n"))
         (buf (save-window-excursion
                (ghostel)
                (current-buffer))))
    (with-current-buffer buf
      (setq ghostel-claude--project-root root)
      ;; Send the claude command once the shell is ready.
      (run-at-time 0.3 nil
                   (lambda ()
                     (when (and (buffer-live-p buf)
                                (process-live-p (buffer-local-value 'ghostel--process buf)))
                       (with-current-buffer buf
                         (process-send-string ghostel--process cmd))))))
    (setf (alist-get root ghostel-claude--buffer-alist nil nil #'equal) buf)
    buf))

(defun ghostel-claude--show-sidebar (buf)
  "Display BUF in a side window."
  (let ((win (display-buffer-in-side-window
              buf `((side . ,ghostel-claude-side)
                    (slot . 0)
                    (window-width . ,ghostel-claude-width)
                    (window-parameters . ((no-delete-other-windows . t)))))))
    (set-window-dedicated-p win t)
    (window-preserve-size win t t)
    win))

(defun ghostel-claude-toggle (arg)
  "Smart toggle for ghostel+claude sidebar.
With prefix ARG (C-u), create with `claude --resume'.
1. Not visible → show (create if needed).
2. Visible + focused → hide.
3. Visible + not focused → focus."
  (interactive "P")
  ;; When inside a sidebar buffer, use its stored root to avoid cwd drift.
  (let* ((sidebar-root (and (derived-mode-p 'ghostel-mode)
                            ghostel-claude--project-root))
         (root (or sidebar-root (ghostel-claude--project-root)))
         (buf (ghostel-claude--get-buffer root))
         (win (ghostel-claude--get-window root))
         (in-sidebar (and buf (eq (current-buffer) buf))))
    (cond
     ;; Visible + focused → hide
     ((and win in-sidebar)
      (when (and ghostel-claude--last-window
                 (window-live-p ghostel-claude--last-window))
        (select-window ghostel-claude--last-window))
      (delete-window win))
     ;; Visible + not focused → focus
     (win
      (setq ghostel-claude--last-window (selected-window))
      (select-window win))
     ;; Buffer exists but not visible → show
     (buf
      (setq ghostel-claude--last-window (selected-window))
      (select-window (ghostel-claude--show-sidebar buf)))
     ;; No buffer → create + show
     (t
      (setq ghostel-claude--last-window (selected-window))
      (let ((new-buf (ghostel-claude--create root arg)))
        (select-window (ghostel-claude--show-sidebar new-buf)))))))

;; Replace ⏺ (U+23FA) with ● (U+25CF) in ghostel output before rendering,
;; because STIX Two Math renders ⏺ with broken descent metrics.
(defun my/ghostel-replace-bullet (orig-fun process output)
  "Replace ⏺ with ● before ghostel renders it.
Uses raw UTF-8 bytes because ghostel--filter receives unibyte strings."
  (funcall orig-fun process
           (string-replace (unibyte-string #xe2 #x8f #xba)
                           (unibyte-string #xe2 #x97 #x8f)
                           output)))
(advice-add 'ghostel--filter :around #'my/ghostel-replace-bullet)

(add-to-list 'golden-ratio-exclude-modes "ghostel-mode")

(global-set-key (kbd "s-l") #'ghostel-claude-toggle)

;;; ghostel.el ends here

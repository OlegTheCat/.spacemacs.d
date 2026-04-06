;;; agent-shell-config.el --- Agent-shell + sidebar setup -*- lexical-binding: t -*-

(require 'agent-shell)
(setq agent-shell-session-strategy 'prompt)
(setq agent-shell-prefer-session-resume nil)
(setq agent-shell-opencode-default-model-id "anthropic/claude-opus-4-6/high")
(setq agent-shell-anthropic-default-model-id "opus[1m]")
;; Ensure agent subprocess inherits PATH and other env vars.
(setq agent-shell-opencode-environment
      (agent-shell-make-environment-variables :inherit-env t))
(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))

(require 'agent-shell-sidebar)
(setq agent-shell-sidebar-locked nil)
(setq golden-ratio-exclude-modes (delete 'agent-shell-mode golden-ratio-exclude-modes))

(defun my/agent-shell-sidebar-smart-toggle ()
  "Smart toggle for agent-shell sidebar (Cursor-style s-l behavior).
1. Hidden → show (create if needed).
2. Visible + focused → hide.
3. Visible + unfocused → focus.
4. Visible + unfocused + active region → send region to sidebar & focus."
  (interactive)
  (let* ((project-root (agent-shell-sidebar--get-project-root))
         (sidebar-window (agent-shell-sidebar--get-window :project-root project-root))
         (sidebar-buffer (agent-shell-sidebar--get-buffer :project-root project-root))
         (in-sidebar (and sidebar-buffer (eq (current-buffer) sidebar-buffer))))
    (cond
     ;; Case 2: visible and focused → hide
     ((and sidebar-window in-sidebar)
      (agent-shell-sidebar--hide-sidebar
       :project-root project-root
       :window sidebar-window))
     ;; Case 4: visible, not focused, region active → send region & focus
     ((and sidebar-window (use-region-p))
      (let ((region-text (agent-shell--get-region-context
                          :deactivate t
                          :agent-cwd (with-current-buffer sidebar-buffer
                                       (agent-shell-cwd)))))
        (agent-shell-sidebar--save-last-window)
        (agent-shell-insert
         :text region-text
         :no-focus t
         :shell-buffer sidebar-buffer)
        (select-window sidebar-window)
        (golden-ratio)))
     ;; Case 3: visible, not focused → just focus
     (sidebar-window
      (agent-shell-sidebar--save-last-window)
      (select-window sidebar-window)
      (golden-ratio))
     ;; Case 1a: not visible but buffer exists → show
     (sidebar-buffer
      (agent-shell-sidebar--show-existing-sidebar
       :project-root project-root
       :buffer sidebar-buffer))
     ;; Case 1b: never created → create and show
     (t
      (agent-shell-sidebar--create-and-show-sidebar
       :project-root project-root)))))

;; (global-set-key (kbd "s-l") #'my/agent-shell-sidebar-smart-toggle)

;;; agent-shell-config.el ends here

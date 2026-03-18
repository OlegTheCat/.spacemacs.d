;; -*- lexical-binding: t -*-
;; ECA (Editor Code Assistant) configuration

(require 'eca)

(setq eca-chat-use-side-window nil
      eca-chat-focus-on-open t
      eca-chat-custom-model "anthropic/claude-opus-4-6")

(defun my/eca--display-right (buffer)
  "Display BUFFER in a right split, bypassing purpose-mode.
Locally overrides `display-buffer-overriding-action' so that
`display-buffer-in-direction' is actually consulted."
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . right)
           (window-width . 0.5))))
    (pop-to-buffer buffer)))

(defun my/eca-smart-toggle ()
  "Smart toggle for ECA chat window.
1. Hidden → show (create if needed).
2. Visible + focused → hide.
3. Visible + unfocused → focus."
  (interactive)
  (if-let* ((session (eca-session))
            (buffer (eca-chat--get-last-buffer session))
            ((buffer-live-p buffer)))
      (let ((window (get-buffer-window buffer t))
            (in-eca (eq (current-buffer) buffer)))
        (cond
         ;; Case 2: visible and focused → hide
         ((and window in-eca)
          (quit-window nil window))
         ;; Case 3: visible, not focused → focus
         (window
          (select-window window)
          (golden-ratio))
         ;; Case 1a: not visible → show in right split
         (t
          (my/eca--display-right buffer)
          (goto-char (point-max))
          (golden-ratio))))
    ;; Case 1b: no session or buffer → start eca in right split
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-in-direction)
             (direction . right)
             (window-width . 0.5))))
      (call-interactively #'eca))))

(global-set-key (kbd "s-l") #'my/eca-smart-toggle)

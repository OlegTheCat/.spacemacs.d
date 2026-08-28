;;; ghostel.el --- Generic Ghostel tweaks -*- lexical-binding: t -*-

(defun my/ghostel-keep-spinner-mode-line-as-list (&rest _)
  "Keep Ghostel's singleton spinner compatible with Spaceline.
Ghostel collapses a sole `spinner--mode-line-construct' component to a
bare symbol.  Restore spinner.el's sequence-form mode-line shape."
  (when (eq mode-line-process 'spinner--mode-line-construct)
    (setq mode-line-process '("" spinner--mode-line-construct))))

(defun my/ghostel-readonly-cancel-selection-or-exit ()
  "Cancel an active selection, or exit Ghostel's read-only mode.
When there is no active selection, preserve Ghostel's normal fast-exit
behavior."
  (interactive)
  (setq quit-flag nil)
  (if (use-region-p)
      (deactivate-mark)
    (ghostel-readonly-exit)))

(with-eval-after-load 'ghostel
  (require 'flash)

  ;; Preserve spinner.el's original list shape after Ghostel recomposes
  ;; `mode-line-process'; otherwise Spaceline passes the bare symbol to
  ;; `concat'.
  (advice-remove 'ghostel--mode-line-refresh
                 #'my/ghostel-keep-spinner-mode-line-as-list)
  (advice-add 'ghostel--mode-line-refresh :after
              #'my/ghostel-keep-spinner-mode-line-as-list)

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

  ;; Use adaptive scroll in copy mode instead of full-page jumps.
  (define-key ghostel-readonly-mode-map (kbd "C-v") #'adaptive-scroll-down)
  (define-key ghostel-readonly-mode-map (kbd "M-v") #'adaptive-scroll-up)

  ;; M-v in normal ghostel mode enters copy mode and scrolls up.
  (defun ghostel-enter-copy-mode-and-scroll-up ()
    "Enter copy mode and immediately scroll up one adaptive step."
    (interactive)
    (ghostel-copy-mode)
    (adaptive-scroll-up))
  (define-key ghostel-semi-char-mode-map (kbd "M-v") #'ghostel-enter-copy-mode-and-scroll-up)

  (defun ghostel-enter-copy-mode-and-mwheel-scroll (event)
    "Enter copy mode and forward the mouse-wheel EVENT."
    (interactive "e")
    (ghostel-copy-mode)
    (mwheel-scroll event))
  (define-key ghostel-semi-char-mode-map (kbd "<wheel-up>") #'ghostel-enter-copy-mode-and-mwheel-scroll)

  (defun ghostel-copy-mode-flash-jump ()
    "Enter Ghostel copy mode if needed, then start `my/flash-jump'."
    (interactive)
    (unless (eq ghostel--input-mode 'copy)
      (ghostel-copy-mode))
    (call-interactively #'my/flash-jump))

  (define-key ghostel-mode-map (kbd "s-j") #'ghostel-copy-mode-flash-jump)
  (define-key ghostel-readonly-mode-map (kbd "s-j") #'ghostel-copy-mode-flash-jump)

  ;; M-> in copy mode exits back to the live terminal.
  (define-key ghostel-readonly-mode-map (kbd "M->") #'ghostel-readonly-exit)

  ;; Copy in copy mode without auto-exiting.
  (defun ghostel-copy-mode-copy-stay ()
    "Copy the selected region but stay in copy mode."
    (interactive)
    (when (use-region-p)
      (let ((text (ghostel--clean-copy-text
                   (buffer-substring (region-beginning) (region-end)))))
        (kill-new text)
        (deactivate-mark)
        (message "Copied to kill ring"))))
  (define-key ghostel-readonly-mode-map (kbd "M-w") #'ghostel-copy-mode-copy-stay)
  (define-key ghostel-readonly-mode-map (kbd "C-w") #'ghostel-copy-mode-copy-stay)

  ;; Let the first C-g dismiss an active selection without leaving copy mode.
  (define-key ghostel-readonly-fast-exit-mode-map (kbd "C-g")
              #'my/ghostel-readonly-cancel-selection-or-exit)

  (add-to-list 'golden-ratio-exclude-modes "ghostel-mode"))

;;; ghostel.el ends here

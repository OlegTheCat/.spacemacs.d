;;; ghostel.el --- Generic Ghostel tweaks -*- lexical-binding: t -*-

(require 'ghostel)
(require 'flash)

(setq ghostel-copy-mode-auto-load-scrollback t)

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
;; ghostel calls scroll-up/down-command directly, bypassing the remap.
(define-key ghostel-copy-mode-map (kbd "C-v") #'adaptive-scroll-down)
(define-key ghostel-copy-mode-map (kbd "M-v") #'adaptive-scroll-up)

;; M-v in normal ghostel mode enters copy mode and scrolls up.
(defun ghostel-enter-copy-mode-and-scroll-up ()
  "Enter copy mode and immediately scroll up one adaptive step."
  (interactive)
  (ghostel-copy-mode)
  (adaptive-scroll-up))
(define-key ghostel-mode-map (kbd "M-v") #'ghostel-enter-copy-mode-and-scroll-up)

(defun ghostel-copy-mode-flash-jump ()
  "Enter Ghostel copy mode if needed, then start `flash-jump'."
  (interactive)
  (unless (bound-and-true-p ghostel--copy-mode-active)
    (ghostel-copy-mode))
  (call-interactively #'flash-jump))

(define-key ghostel-mode-map (kbd "s-j") #'ghostel-copy-mode-flash-jump)
(define-key ghostel-copy-mode-map (kbd "s-j") #'ghostel-copy-mode-flash-jump)

;; M-> in copy mode exits back to the live terminal.
(define-key ghostel-copy-mode-map (kbd "M->") #'ghostel-copy-mode-exit)

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
(define-key ghostel-copy-mode-map (kbd "M-w") #'ghostel-copy-mode-copy-stay)
(define-key ghostel-copy-mode-map (kbd "C-w") #'ghostel-copy-mode-copy-stay)

(add-to-list 'golden-ratio-exclude-modes "ghostel-mode")

;;; ghostel.el ends here

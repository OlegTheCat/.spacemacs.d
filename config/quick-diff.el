;;; quick-diff.el --- Quick text comparison with vdiff -*- lexical-binding: t -*-

(require 'vdiff)

;; Word-level (refined) diffs
(setq vdiff-auto-refine t)

(defvar my/qd/buffer-a-name "*my/quick-diff A*")
(defvar my/qd/buffer-b-name "*my/quick-diff B*")
(defvar my/qd/frame-name "Quick Diff")

;; Prevent golden-ratio from unbalancing the split
(add-to-list 'golden-ratio-exclude-modes "vdiff-mode")
(add-to-list 'golden-ratio-exclude-buffer-names my/qd/buffer-a-name)
(add-to-list 'golden-ratio-exclude-buffer-names my/qd/buffer-b-name)

(defun my/quick-diff ()
  "Open a new frame with two empty vdiff buffers for text comparison.
Paste text into each buffer — diffs update live."
  (interactive)
  ;; Kill leftover buffers from previous runs
  (dolist (name (list my/qd/buffer-a-name my/qd/buffer-b-name))
    (when-let ((old (get-buffer name)))
      (kill-buffer old)))
  (let* ((buf-a (get-buffer-create my/qd/buffer-a-name))
         (buf-b (get-buffer-create my/qd/buffer-b-name))
         (frame (make-frame (list
                             (cons 'name my/qd/frame-name)
                             (cons 'width 180)
                             (cons 'height 50)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    (switch-to-buffer buf-a)
    ;; Start vdiff immediately so diffs are live
    (vdiff-buffers buf-a buf-b)
    (balance-windows)))

(defun my/quick-diff-quit ()
  "Kill diff buffers and close the Quick Diff frame."
  (interactive)
  (dolist (name (list my/qd/buffer-a-name my/qd/buffer-b-name))
    (when-let ((buf (get-buffer name)))
      (with-current-buffer buf
        (when (bound-and-true-p vdiff-mode)
          (vdiff-quit)))
      (kill-buffer buf)))
  (when-let ((frame (car (filtered-frame-list
                          (lambda (f) (equal my/qd/frame-name (frame-parameter f 'name)))))))
    (delete-frame frame)))

;;; quick-diff.el ends here

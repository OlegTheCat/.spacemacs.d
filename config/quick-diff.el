;;; quick-diff.el --- Quick text comparison with vdiff -*- lexical-binding: t -*-

(require 'vdiff)

;; Word-level (refined) diffs
(setq vdiff-auto-refine t)

;; Prevent golden-ratio from unbalancing the split
(add-to-list 'golden-ratio-exclude-modes "vdiff-mode")
(add-to-list 'golden-ratio-exclude-buffer-names "*Diff A*")
(add-to-list 'golden-ratio-exclude-buffer-names "*Diff B*")

(defun my/quick-diff ()
  "Open a new frame with two empty vdiff buffers for text comparison.
Paste text into each buffer — diffs update live."
  (interactive)
  ;; Kill leftover buffers from previous runs
  (dolist (name '("*Diff A*" "*Diff B*"))
    (when-let ((old (get-buffer name)))
      (kill-buffer old)))
  (let* ((buf-a (get-buffer-create "*Diff A*"))
         (buf-b (get-buffer-create "*Diff B*"))
         (frame (make-frame '((name . "Quick Diff")
                              (width . 180)
                              (height . 50)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    (switch-to-buffer buf-a)
    ;; Start vdiff immediately so diffs are live
    (vdiff-buffers buf-a buf-b)
    (balance-windows)))

(defun my/quick-diff-quit ()
  "Kill diff buffers and close the Quick Diff frame."
  (interactive)
  (dolist (name '("*Diff A*" "*Diff B*"))
    (when-let ((buf (get-buffer name)))
      (with-current-buffer buf
        (when (bound-and-true-p vdiff-mode)
          (vdiff-quit)))
      (kill-buffer buf)))
  (when-let ((frame (car (filtered-frame-list
                          (lambda (f) (equal "Quick Diff" (frame-parameter f 'name)))))))
    (delete-frame frame)))

;;; quick-diff.el ends here

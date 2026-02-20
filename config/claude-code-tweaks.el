;;; claude-code-tweaks.el --- Claude Code IDE fixes -*- lexical-binding: t -*-

;; Skip terminal dimension sync when size hasn't changed,
;; to avoid a full re-render on toggle.
(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--sync-terminal-dimensions :around
              (lambda (orig-fun buffer window)
                (when (and buffer window (buffer-live-p buffer) (window-live-p window))
                  (with-current-buffer buffer
                    (when-let ((proc (get-buffer-process buffer)))
                      (let ((new-h (window-body-height window))
                            (new-w (window-body-width window))
                            (cur-h (process-get proc 'my/last-height))
                            (cur-w (process-get proc 'my/last-width)))
                        (unless (and (eql new-h cur-h) (eql new-w cur-w))
                          (process-put proc 'my/last-height new-h)
                          (process-put proc 'my/last-width new-w)
                          (funcall orig-fun buffer window)))))))))

;; Replace ⏺ (U+23FA) with ● (U+25CF) in vterm output before rendering,
;; because STIX Two Math renders ⏺ with broken descent metrics.
(with-eval-after-load 'vterm
  (defun my/vterm-replace-bullet (orig-fun process input)
    "Replace ⏺ with ● before vterm renders it.
Uses raw UTF-8 bytes because vterm--filter receives unibyte strings."
    (funcall orig-fun process
             (string-replace (unibyte-string #xe2 #x8f #xba)
                             (unibyte-string #xe2 #x97 #x8f)
                             input)))
  (advice-add 'vterm--filter :around #'my/vterm-replace-bullet))

;;; claude-code-tweaks.el ends here

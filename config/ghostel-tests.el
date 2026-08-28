;;; ghostel-tests.el --- ERT tests for generic Ghostel tweaks -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gtx-ghostel-spaceline-restores-spinner-sequence-shape ()
  (let ((mode-line-process 'spinner--mode-line-construct))
    (my/ghostel-keep-spinner-mode-line-as-list)
    (should (equal mode-line-process
                   '("" spinner--mode-line-construct)))))

(ert-deftest gtx-ghostel-spaceline-leaves-other-process-values-alone ()
  (dolist (value '(nil
                   " :Copy"
                   (" :Copy" spinner--mode-line-construct)))
    (let ((mode-line-process value))
      (my/ghostel-keep-spinner-mode-line-as-list)
      (should (equal mode-line-process value)))))

(ert-deftest gtx-ghostel-copy-mode-C-g-cancels-active-selection ()
  "C-g deactivates a copy-mode region without exiting copy mode."
  (with-temp-buffer
    (insert "selected text")
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (let ((exit-called nil)
          (transient-mark-mode t))
      (cl-letf (((symbol-function 'ghostel-readonly-exit)
                 (lambda () (setq exit-called t))))
        (my/ghostel-readonly-cancel-selection-or-exit))
      (should-not mark-active)
      (should-not exit-called))))

(ert-deftest gtx-ghostel-copy-mode-C-g-exits-without-selection ()
  "C-g preserves Ghostel's fast-exit behavior without a selection."
  (with-temp-buffer
    (let ((exit-called nil))
      (cl-letf (((symbol-function 'ghostel-readonly-exit)
                 (lambda () (setq exit-called t))))
        (my/ghostel-readonly-cancel-selection-or-exit))
      (should exit-called))))

;;; ghostel-tests.el ends here

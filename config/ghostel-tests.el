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

;;; ghostel-tests.el ends here

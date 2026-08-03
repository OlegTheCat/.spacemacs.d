;;; adaptive-scroll-tests.el --- Tests for adaptive-scroll -*- lexical-binding: t -*-

(require 'ert)

(defmacro adaptive-scroll-test--with-wrapped-buffer (&rest body)
  "Run BODY in a displayed buffer whose logical lines wrap on screen."
  (declare (indent 0) (debug t))
  `(let ((orig (current-window-configuration))
         (ignore-window-parameters t)
         (adaptive-scroll--level 0)
         (adaptive-scroll--last-direction nil)
         (buf (generate-new-buffer " *adaptive-scroll-test*")))
     (unwind-protect
         (progn
           (delete-other-windows)
           (switch-to-buffer buf)
           (setq-local truncate-lines nil)
           (setq-local word-wrap nil)
           (dotimes (i 100)
             (insert (format "%03d %s\n" i (make-string 150 ?x))))
           ,@body)
       (set-window-configuration orig)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

(ert-deftest adaptive-scroll-down-keeps-target-visible-with-wrapped-lines ()
  "Scrolling down counts screen rows, keeping a non-EOF target visible."
  (adaptive-scroll-test--with-wrapped-buffer
    (goto-char (point-min))
    (forward-line 8)
    (set-window-start (selected-window) (point-min) t)
    (redisplay t)
    (let ((before-point (point))
          (before-start (window-start)))
      (should (< (line-number-at-pos) (line-number-at-pos (point-max))))
      (adaptive-scroll-down)
      (should (> (point) before-point))
      (should (> (window-start) before-start))
      (should (< (count-screen-lines (window-start) (point) nil
                                     (selected-window))
                 (window-text-height))))))

;;; adaptive-scroll-tests.el ends here

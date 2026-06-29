;;; adaptive-scroll.el --- Binary-search scrolling -*- lexical-binding: t -*-
;;
;; Golden-ratio binary-search scrolling.
;; Each direction reversal shrinks the step by 0.618; each consecutive
;; same-direction scroll grows it back by one level (undoes one shrink).
;; The step is clamped to the base size (window-text-height / 1.618).

(defvar adaptive-scroll--level 0
  "Current nesting level.  0 = full golden-ratio step.
Each +1 multiplies the step by 0.618 again.")

(defvar adaptive-scroll--last-direction nil
  "Last scroll direction: `up' or `down', or nil if reset.")

(defun adaptive-scroll--reset ()
  "Reset adaptive scroll state to base level."
  (setq adaptive-scroll--level 0
        adaptive-scroll--last-direction nil))

(defun adaptive-scroll--step-lines ()
  "Compute scroll distance for current level."
  (let* ((base (round (/ (window-text-height) 1.618)))
         (ratio (expt 0.618 adaptive-scroll--level)))
    (max 1 (round (* base ratio)))))

(defun adaptive-scroll--scroll (direction)
  "Scroll in DIRECTION (`up' or `down'), adjusting level.
Reversal adds a level (shrinks step), same direction removes one (grows step)."
  (cond
   ;; First scroll or after reset
   ((null adaptive-scroll--last-direction)
    (setq adaptive-scroll--level 0))
   ;; Same direction — accelerate (remove one shrink level)
   ((eq direction adaptive-scroll--last-direction)
    (setq adaptive-scroll--level (max 0 (1- adaptive-scroll--level))))
   ;; Reversed — decelerate (add one shrink level)
   (t
    (setq adaptive-scroll--level (1+ adaptive-scroll--level))))
  (setq adaptive-scroll--last-direction direction)
  ;; Move point like a cursor; only scroll the window once point would leave the
  ;; visible page — and do that via `set-window-start' (redisplay's fast path),
  ;; not by moving point off-screen, which forces a full-window relayout every
  ;; press (~70ms, a hang on heavy-markup ghostel buffers under load).
  (let* ((lines (adaptive-scroll--step-lines))
         (target (save-excursion
                   (forward-visible-line (if (eq direction 'down) lines (- lines)))
                   (point))))
    (cond
     ;; Target still on-screen: just move the cursor, leave the window put.
     ((pos-visible-in-window-p target)
      (goto-char target))
     ;; Scrolling up past the top: put the target on the first line.
     ((eq direction 'up)
      (set-window-start (selected-window) target)
      (goto-char target))
     ;; Scrolling down past the bottom: put the target on the last line.
     (t
      (set-window-start (selected-window)
                        (save-excursion
                          (goto-char target)
                          (forward-visible-line (- (1- (window-text-height))))
                          (point)))
      (goto-char target)))))

;;;###autoload
(defun adaptive-scroll-down (&optional _arg)
  "Scroll down with adaptive golden-ratio step."
  (interactive "^P")
  (adaptive-scroll--scroll 'down))

;;;###autoload
(defun adaptive-scroll-up (&optional _arg)
  "Scroll up with adaptive golden-ratio step."
  (interactive "^P")
  (adaptive-scroll--scroll 'up))

;; Bind over the golden-ratio-scroll-screen commands
(global-set-key [remap scroll-down-command] #'adaptive-scroll-up)
(global-set-key [remap scroll-up-command] #'adaptive-scroll-down)

;;; adaptive-scroll.el ends here

;;; ghostel-terminals.el --- Project terminal drawer on ghostel-toggle -*- lexical-binding: t -*-

;; The `terminal' instantiation of the ghostel-toggle library: a plain
;; shell per project in a bottom drawer (`s-i'), with tabs (`s-t' inside
;; the drawer), per-project fullscreen (`s-<return>' while focused), and
;; a home-directory fullscreen terminal (`C-s-i').

(require 'ghostel-toggle)

(defvar ghostel-terminal-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-<left>") #'ghostel-terminal-previous-session)
    (define-key map (kbd "s-<right>") #'ghostel-terminal-next-session)
    ;; Shadows the global agent `s-t' in drawer buffers via minor-mode
    ;; keymap precedence (ghostel itself binds no super-modified keys, so
    ;; nothing outranks this).  Prefix args are deliberately ignored —
    ;; terminals have no variants.
    (define-key map (kbd "s-t") #'ghostel-terminal-new-session)
    map)
  "Keymap active in managed ghostel terminal buffers.")

(define-minor-mode ghostel-terminal-session-mode
  "Minor mode for ghostel terminal buffers managed by the drawer."
  :init-value nil
  :lighter nil
  :keymap ghostel-terminal-session-mode-map)

(ghostel-toggle-define-kind 'terminal
                            :side 'bottom
                            :size 0.5
                            :minor-mode 'ghostel-terminal-session-mode)

(defun ghostel-terminal--next-label (root)
  "Return the display label for a new terminal session in ROOT.
Reuses freed numbers: killing \"Terminal\" makes the next one \"Terminal\"."
  (let ((labels (mapcar (lambda (session)
                          (plist-get session :label))
                        (ghostel-toggle--sessions-for-root 'terminal root)))
        (n 1)
        label)
    (while (member (setq label (if (= n 1)
                                   "Terminal"
                                 (format "Terminal %d" n)))
                   labels)
      (setq n (1+ n)))
    label))

(defun ghostel-terminal--create (root)
  "Create a new terminal session in ROOT and return its window."
  (ghostel-toggle-create-session 'terminal root
                                 :label (ghostel-terminal--next-label root)))

(defun ghostel-terminal-new-session ()
  "Create a new ghostel terminal tab for the current project."
  (interactive)
  (let ((root (ghostel-toggle-command-root)))
    (ghostel-toggle--remember-last-window 'terminal)
    (select-window (ghostel-terminal--create root))))

(defun ghostel-terminal-toggle ()
  "Toggle the project terminal drawer.
With a prefix argument, create a new terminal tab for the project.
While this project is in terminal fullscreen mode, plain `s-i' instead
toggles the terminal's visibility: hide it (to the code) when shown, or
re-expand it to fullscreen when hidden or split.  Fullscreen mode is
sticky until `s-<return>' demotes it (see
`ghostel-toggle-fullscreen-command'); the `C-s-i' home fullscreen is
dismissed outright.  Otherwise: hide the drawer when it shows this
project and is focused; when it is visible but unfocused, focus it instead.
If it is hidden, show the default session, creating one when none exists."
  (interactive)
  (let* ((root (ghostel-toggle-command-root))
         (view (and (not current-prefix-arg)
                    (ghostel-toggle--view-for-root 'terminal root))))
    (cond
     (current-prefix-arg
      (ghostel-terminal-new-session))
     (view
      (ghostel-toggle--fullscreen-flip view))
     ((ghostel-toggle--root-visible-p 'terminal root)
      (let ((win (ghostel-toggle--panel-window 'terminal)))
        (if (eq (selected-window) win)
            (ghostel-toggle-hide-panel 'terminal)
          (ghostel-toggle--remember-last-window 'terminal)
          (select-window win))))
     (t
      (let ((session (ghostel-toggle--default-session 'terminal root)))
        (ghostel-toggle--remember-last-window 'terminal)
        (select-window
         (if session
             (ghostel-toggle--show-session session)
           (ghostel-terminal--create root))))))))

(defun ghostel-terminal-next-session ()
  "Switch to the next ghostel terminal session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'terminal 1))

(defun ghostel-terminal-previous-session ()
  "Switch to the previous ghostel terminal session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'terminal -1))

(defun ghostel-terminal--get-home-buffer (root)
  "Return a live home terminal buffer, creating the session if needed."
  (or (when-let* ((session (ghostel-toggle--default-session 'terminal root)))
        (ghostel-toggle--session-buffer session))
      (progn
        (ghostel-toggle--create-session-hidden
         'terminal root :label (ghostel-terminal--next-label root))
        (when-let* ((session (ghostel-toggle--default-session 'terminal root)))
          (ghostel-toggle--session-buffer session)))))

(defun ghostel-terminal-home-toggle ()
  "Toggle a fullscreen terminal session rooted in the home directory.
Promotes the home (~/) terminal to fullscreen from any project; press
again to restore the previous layout.  The home fullscreen also
dismisses with plain `s-i' (it is registered `:dismiss', so `s-i'
dismisses it and `C-s-i' re-fires it)."
  (interactive)
  (ghostel-toggle-home-toggle 'terminal #'ghostel-terminal--get-home-buffer))

(global-set-key (kbd "s-i") #'ghostel-terminal-toggle)
(global-set-key (kbd "C-s-i") #'ghostel-terminal-home-toggle)

;;; ghostel-terminals.el ends here

;;; ghostel.el --- Ghostel agent sidebars -*- lexical-binding: t -*-

(require 'ghostel)
(require 'cl-lib)

(setq ghostel-copy-mode-auto-load-scrollback t)

(defvar ghostel-agent-profiles
  '((claude
     :buffer-name "*ghostel-claude*"
     :program "claude"
     :args nil
     :resume-args ("--resume"))
    (codex
     :buffer-name "*ghostel-codex*"
     :program "codex"
     :args nil
     :resume-args ("resume")))
  "Agent profiles available through `ghostel-agent-toggle-command'.")

(defvar ghostel-agent--buffer-alist
  (when (boundp 'ghostel-claude--buffer-alist)
    (mapcar (lambda (entry)
              (cons (cons 'claude (car entry)) (cdr entry)))
            (symbol-value 'ghostel-claude--buffer-alist)))
  "Alist mapping (AGENT . PROJECT-ROOT) keys to ghostel agent buffers.")

(defvar ghostel-agent--selected-agent-alist nil
  "Alist mapping project-root strings to the selected ghostel agent.")

(defvar-local ghostel-agent--agent nil
  "Agent this ghostel buffer belongs to.")

(defvar-local ghostel-agent--project-root nil
  "Project root this ghostel agent buffer belongs to.")

(defvar ghostel-agent--last-window nil
  "Window that was selected before jumping to the sidebar.")

(defvar ghostel-agent-side 'right
  "Side of the frame for the agent sidebar window.")

(defvar ghostel-agent-width 0.55
  "Width of the sidebar as a fraction of the frame.")

(defvar ghostel-agent-command-delay 0.3
  "Seconds to wait before sending the agent command to a new shell.")

(defun ghostel-agent--profile (agent)
  "Return the profile plist for AGENT."
  (or (cdr (assq agent ghostel-agent-profiles))
      (user-error "Unknown ghostel agent: %S" agent)))

(defun ghostel-agent--key (agent root)
  "Return the buffer registry key for AGENT in ROOT."
  (cons agent (ghostel-agent--normalize-root root)))

(defun ghostel-agent--normalize-root (root)
  "Return ROOT as a canonical project directory string."
  (file-name-as-directory (expand-file-name root)))

(defun ghostel-agent--buffer-name (agent root)
  "Return the project-specific Ghostel buffer identity for AGENT in ROOT."
  ;; Ghostel reuses buffers by `ghostel--buffer-identity', even after
  ;; title tracking renames the visible buffer.
  (let* ((root (ghostel-agent--normalize-root root))
         (dir (directory-file-name root))
         (project-name (file-name-nondirectory dir))
         (project-label (if (string= project-name "") "root" project-name))
         (root-hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "*ghostel-%s:%s:%s*" agent project-label root-hash)))

(defun ghostel-agent--current-agent ()
  "Return the agent for the current ghostel buffer, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (or (bound-and-true-p ghostel-agent--agent)
        (and (bound-and-true-p ghostel-claude--project-root) 'claude))))

(defun ghostel-agent--current-root ()
  "Return the project root for the current ghostel agent buffer, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (let ((root (or (bound-and-true-p ghostel-agent--project-root)
                    (bound-and-true-p ghostel-claude--project-root))))
      (when root
        (ghostel-agent--normalize-root root)))))

(defun ghostel-agent--selected-agent (root)
  "Return the selected agent for ROOT, or nil."
  (let ((agent (alist-get (ghostel-agent--normalize-root root)
                          ghostel-agent--selected-agent-alist nil nil #'equal)))
    (when (assq agent ghostel-agent-profiles)
      agent)))

(defun ghostel-agent--select-agent (root agent)
  "Mark AGENT as the selected agent for ROOT."
  (setf (alist-get (ghostel-agent--normalize-root root)
                   ghostel-agent--selected-agent-alist nil nil #'equal)
        agent))

(defun ghostel-agent--project-root ()
  "Return the project root, or `default-directory' as fallback."
  (ghostel-agent--normalize-root
   (or (and (fboundp 'projectile-project-root)
            (ignore-errors (projectile-project-root)))
       default-directory)))

(defun ghostel-agent--buffer-matches-p (buf agent root)
  "Return non-nil when BUF is the live AGENT buffer for ROOT."
  (and (buffer-live-p buf)
       (let ((root (ghostel-agent--normalize-root root))
             (identity (ghostel-agent--buffer-name agent root)))
         (with-current-buffer buf
           (and (derived-mode-p 'ghostel-mode)
                (eq ghostel-agent--agent agent)
                (equal ghostel-agent--project-root root)
                (equal (bound-and-true-p ghostel--buffer-identity)
                       identity))))))

(defun ghostel-agent--find-buffer (agent root)
  "Find a live Ghostel AGENT buffer for ROOT by its project identity."
  (let ((identity (ghostel-agent--buffer-name agent root)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'ghostel-mode)
              (equal (bound-and-true-p ghostel--buffer-identity)
                     identity))))
     (buffer-list))))

(defun ghostel-agent--get-buffer (agent root)
  "Return the live ghostel AGENT buffer for ROOT, or nil."
  (let* ((root (ghostel-agent--normalize-root root))
         (key (ghostel-agent--key agent root))
         (buf (alist-get key ghostel-agent--buffer-alist nil nil #'equal)))
    (unless (ghostel-agent--buffer-matches-p buf agent root)
      (setf (alist-get key ghostel-agent--buffer-alist nil 'remove #'equal) nil)
      (setq buf nil))
    (or buf
        (let ((found (ghostel-agent--find-buffer agent root)))
          (when found
            (with-current-buffer found
              (setq ghostel-agent--agent agent
                    ghostel-agent--project-root root))
            (setf (alist-get key ghostel-agent--buffer-alist nil nil #'equal)
                  found)
            found)))))

(defun ghostel-agent--get-window (agent root)
  "Return the window displaying the ghostel AGENT buffer for ROOT, or nil."
  (let ((buf (ghostel-agent--get-buffer agent root)))
    (when buf (get-buffer-window buf t))))

(defun ghostel-agent--agents-for-root (root)
  "Return agents with live buffers for ROOT."
  (delq nil
        (mapcar (lambda (profile)
                  (let ((agent (car profile)))
                    (when (ghostel-agent--get-buffer agent root)
                      agent)))
                ghostel-agent-profiles)))

(defun ghostel-agent--default-agent (root)
  "Return the default agent for ROOT."
  (let ((root (ghostel-agent--normalize-root root)))
    (or (ghostel-agent--selected-agent root)
        (ghostel-agent--current-agent)
        (let ((agents (ghostel-agent--agents-for-root root)))
          (when (and agents (null (cdr agents)))
            (car agents)))
        'claude)))

(defun ghostel-agent--prune-buffer-alist ()
  "Drop stale agent buffer entries from `ghostel-agent--buffer-alist'."
  (setq ghostel-agent--buffer-alist
        (delq nil
              (mapcar
               (lambda (entry)
                 (let ((key (car entry))
                       (buf (cdr entry)))
                   (when (and (consp key)
                              (ghostel-agent--buffer-matches-p
                               buf (car key) (cdr key)))
                     (cons (ghostel-agent--key (car key) (cdr key)) buf))))
               ghostel-agent--buffer-alist))))

(defun ghostel-agent--command-line (profile resume)
  "Return the shell command for PROFILE.
When RESUME is non-nil, include the profile's resume arguments."
  (mapconcat #'shell-quote-argument
             (cons (plist-get profile :program)
                   (if resume
                       (plist-get profile :resume-args)
                     (plist-get profile :args)))
             " "))

(defun ghostel-agent--send-command (buf command)
  "Send COMMAND to the shell running in BUF."
  (when (and (buffer-live-p buf)
             (process-live-p (buffer-local-value 'ghostel--process buf)))
    (with-current-buffer buf
      (process-send-string ghostel--process (concat command "\n")))))

(defun ghostel-agent--display-sidebar-window (buf)
  "Display BUF in a side window and return that window."
  (display-buffer-in-side-window
   buf `((side . ,ghostel-agent-side)
         (slot . 0)
         (window-width . ,ghostel-agent-width)
         (window-parameters . ((no-delete-other-windows . t))))))

(defun ghostel-agent--finish-sidebar-window (win)
  "Apply sidebar window settings to WIN."
  (when (window-live-p win)
    (set-window-dedicated-p win t)
    (window-preserve-size win t t))
  win)

(defun ghostel-agent--create (agent root &optional resume)
  "Create a new ghostel terminal running AGENT in ROOT and return its window.
When RESUME is non-nil, use the agent profile's resume arguments."
  (let* ((root (ghostel-agent--normalize-root root))
         (profile (ghostel-agent--profile agent))
         (default-directory root)
         (command (ghostel-agent--command-line profile resume))
         (identity (ghostel-agent--buffer-name agent root))
         (buf (get-buffer-create identity)))
    (with-current-buffer buf
      (setq default-directory root))
    (let ((win (ghostel-agent--display-sidebar-window buf)))
      (select-window win)
      (let ((ghostel-buffer-name identity))
        (setq buf (ghostel nil)))
      (ghostel-agent--finish-sidebar-window win)
      (with-current-buffer buf
        (setq ghostel-agent--agent agent
              ghostel-agent--project-root root))
      (run-at-time ghostel-agent-command-delay nil
                   #'ghostel-agent--send-command buf command)
      (setf (alist-get (ghostel-agent--key agent root)
                       ghostel-agent--buffer-alist nil nil #'equal) buf)
      win)))

(defun ghostel-agent--show-sidebar (buf)
  "Display BUF in a side window."
  (let ((win (ghostel-agent--display-sidebar-window buf)))
    (ghostel-agent--finish-sidebar-window win)
    win))

(defun ghostel-agent--send-region (buf)
  "Send the active region with file context to the ghostel agent BUF."
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (file (let ((f (or (buffer-file-name) (buffer-name)))
                     (root (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))))
                 (if root (file-relative-name f root) f)))
         (line-beg (line-number-at-pos beg))
         (line-end (line-number-at-pos end))
         (formatted (format "%s:%d-%d\n```\n%s\n```" file line-beg line-end text)))
    (deactivate-mark)
    (with-current-buffer buf
      (ghostel-paste-string formatted))))

(defun ghostel-agent-toggle (agent resume &optional force-show root)
  "Smart toggle for AGENT's ghostel sidebar.
When RESUME is non-nil, create the buffer with the resume command.
When FORCE-SHOW is non-nil, focus the sidebar instead of hiding it.
1. Not visible → show (create if needed).
2. Visible + focused → hide.
3. Visible + not focused + region → send region & focus.
4. Visible + not focused → focus."
  ;; When inside a sidebar buffer, use its stored root to avoid cwd drift.
  (let* ((root (or root
                   (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root))
         (buf (ghostel-agent--get-buffer agent root))
         (win (ghostel-agent--get-window agent root))
         (in-sidebar (and buf (eq (current-buffer) buf))))
    (ghostel-agent--select-agent root agent)
    (cond
     ;; Visible + focused → hide
     ((and win in-sidebar (not force-show))
      (when (and ghostel-agent--last-window
                 (window-live-p ghostel-agent--last-window))
        (select-window ghostel-agent--last-window))
      (delete-window win))
     ;; Visible + focused + explicit selection → keep focus.
     ((and win in-sidebar force-show)
      (select-window win))
     ;; Visible + not focused + region → send region & focus
     ((and win (use-region-p))
      (ghostel-agent--send-region buf)
      (setq ghostel-agent--last-window (selected-window))
      (select-window win))
     ;; Visible + not focused → focus
     (win
      (setq ghostel-agent--last-window (selected-window))
      (select-window win))
     ;; Buffer exists but not visible → show
     (buf
      (setq ghostel-agent--last-window (selected-window))
      (select-window (ghostel-agent--show-sidebar buf)))
     ;; No buffer → create + show
     (t
      (setq ghostel-agent--last-window (selected-window))
      (select-window (ghostel-agent--create agent root resume))))))

(defun ghostel-agent--parse-prefix (arg)
  "Return (AGENT RESUME EXPLICIT) for raw prefix ARG.
Plain `s-l' toggles the selected agent for this project.
`C-u s-l' selects Claude and starts it with `--resume'.
`C-2 s-l' selects Codex.
`C-3 s-l' selects Codex and starts it with `resume'."
  (cond
   ((null arg) '(nil nil nil))
   ((equal arg '(4)) '(claude t t))
   ((equal arg 2) '(codex nil t))
   ((equal arg 3) '(codex t t))
   (t (user-error "Unknown ghostel agent prefix: %S" arg))))

(defun ghostel-agent-toggle-command (arg)
  "Toggle a ghostel agent sidebar based on prefix ARG.
Plain `s-l' toggles the selected agent for this project, `C-u s-l'
selects and resumes Claude, `C-2 s-l' selects Codex, and `C-3 s-l'
selects and resumes Codex."
  (interactive "P")
  (let* ((parsed (ghostel-agent--parse-prefix arg))
         (agent (car parsed))
         (resume (cadr parsed))
         (explicit (caddr parsed))
         (root (or (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root)))
    (unless agent
      (setq agent (ghostel-agent--default-agent root)))
    (ghostel-agent-toggle agent resume explicit root)))

(defun ghostel-claude-toggle (arg)
  "Backward-compatible wrapper for `ghostel-agent-toggle-command'."
  (interactive "P")
  (ghostel-agent-toggle-command arg))

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

(ghostel-agent--prune-buffer-alist)

(global-set-key (kbd "s-l") #'ghostel-agent-toggle-command)

;;; ghostel.el ends here

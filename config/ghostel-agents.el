;;; ghostel-agents.el --- Ghostel coding agent sidebar sessions -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)

(defvar ghostel-buffer-name)

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
  "Agent profiles available through ghostel agent commands.")

(defvar ghostel-agent--sessions nil
  "Alist mapping ghostel agent session ids to session plists.")

(defvar ghostel-agent--selected-session-alist nil
  "Alist mapping project-root strings to the selected session id.")

(defvar ghostel-agent--last-session-alist nil
  "Alist mapping (AGENT . PROJECT-ROOT) keys to selected session ids.")

(defvar ghostel-agent--session-counter 0
  "Monotonic counter used to allocate ghostel agent session ids.")

(defvar-local ghostel-agent--session-id nil
  "Session id this ghostel buffer belongs to.")

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

(defvar ghostel-agent--fullscreen-config nil
  "Saved window configuration to restore when leaving fullscreen.
Non-nil only while a ghostel agent session fills the frame; see
`ghostel-agent-toggle-fullscreen'.")

(defvar ghostel-agent-command-delay 0.3
  "Seconds to wait before sending the agent command to a new shell.")

(defface ghostel-agent-tab-current
  '((t :inherit tab-line-tab-current :weight bold :underline nil))
  "Face for the selected ghostel agent session tab.")

(defface ghostel-agent-tab
  '((t :inherit tab-line-tab))
  "Face for inactive ghostel agent session tabs.")

(set-face-attribute 'ghostel-agent-tab-current nil
                    :inherit 'tab-line-tab-current
                    :weight 'bold
                    :underline nil)

(defvar ghostel-agent-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-<left>") #'ghostel-agent-previous-session)
    (define-key map (kbd "s-<right>") #'ghostel-agent-next-session)
    map)
  "Keymap active in managed ghostel agent session buffers.")

(define-minor-mode ghostel-agent-session-mode
  "Minor mode for ghostel buffers managed by the agent sidebar."
  :init-value nil
  :lighter nil
  :keymap ghostel-agent-session-mode-map)

(defun ghostel-agent--profile (agent)
  "Return the profile plist for AGENT."
  (or (cdr (assq agent ghostel-agent-profiles))
      (user-error "Unknown ghostel agent: %S" agent)))

(defun ghostel-agent--normalize-root (root)
  "Return ROOT as a canonical project directory string."
  (file-name-as-directory (expand-file-name root)))

(defun ghostel-agent--key (agent root)
  "Return the registry key for AGENT in ROOT."
  (cons agent (ghostel-agent--normalize-root root)))

(defun ghostel-agent--buffer-name (agent root session-id)
  "Return the project/session-specific Ghostel identity for AGENT in ROOT."
  ;; Ghostel reuses buffers by `ghostel--buffer-identity', even after
  ;; title tracking renames the visible buffer.
  (let* ((root (ghostel-agent--normalize-root root))
         (dir (directory-file-name root))
         (project-name (file-name-nondirectory dir))
         (project-label (if (string= project-name "") "root" project-name))
         (root-hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "*ghostel-%s:%s:%s:%s*"
            agent project-label root-hash session-id)))

(defun ghostel-agent--next-session-id ()
  "Return a fresh ghostel agent session id."
  (setq ghostel-agent--session-counter
        (1+ ghostel-agent--session-counter))
  (format "ghostel-agent-session-%d" ghostel-agent--session-counter))

(defun ghostel-agent--session-by-id (id)
  "Return the session plist for ID, or nil."
  (cdr (assoc id ghostel-agent--sessions)))

(defun ghostel-agent--session-buffer (session)
  "Return SESSION's live buffer, or nil."
  (let ((buf (plist-get session :buffer)))
    (when (buffer-live-p buf)
      buf)))

(defun ghostel-agent--session-live-p (session)
  "Return non-nil when SESSION has a live buffer."
  (and (plist-get session :id)
       (ghostel-agent--session-buffer session)))

(defun ghostel-agent--live-session-by-id (id)
  "Return the live session plist for ID, or nil."
  (let ((session (ghostel-agent--session-by-id id)))
    (when (and session (ghostel-agent--session-live-p session))
      session)))

(defun ghostel-agent--cleanup-sessions ()
  "Drop registry entries whose buffers were killed."
  (let (live-ids)
    (setq ghostel-agent--sessions
          (cl-remove-if-not
           (lambda (entry)
             (when (ghostel-agent--session-live-p (cdr entry))
               (push (car entry) live-ids)
               t))
           ghostel-agent--sessions))
    (setq ghostel-agent--selected-session-alist
          (cl-remove-if-not
           (lambda (entry) (member (cdr entry) live-ids))
           ghostel-agent--selected-session-alist))
    (setq ghostel-agent--last-session-alist
          (cl-remove-if-not
           (lambda (entry) (member (cdr entry) live-ids))
           ghostel-agent--last-session-alist))))

(defun ghostel-agent--session-for-buffer (buf)
  "Return the session plist for BUF, or nil."
  (seq-find (lambda (session)
              (eq (plist-get session :buffer) buf))
            (mapcar #'cdr ghostel-agent--sessions)))

(defun ghostel-agent--agent-name (agent)
  "Return a display name for AGENT."
  (capitalize (symbol-name agent)))

(defun ghostel-agent--current-agent ()
  "Return the agent for the current ghostel buffer, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (or (when-let* ((id (bound-and-true-p ghostel-agent--session-id))
                    (session (ghostel-agent--live-session-by-id id)))
          (plist-get session :agent))
        (bound-and-true-p ghostel-agent--agent)
        (and (bound-and-true-p ghostel-claude--project-root) 'claude))))

(defun ghostel-agent--current-root ()
  "Return the project root for the current ghostel agent buffer, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (let ((root (or (when-let* ((id (bound-and-true-p ghostel-agent--session-id))
                                (session (ghostel-agent--live-session-by-id id)))
                      (plist-get session :root))
                    (bound-and-true-p ghostel-agent--project-root)
                    (bound-and-true-p ghostel-claude--project-root))))
      (when root
        (ghostel-agent--normalize-root root)))))

(defun ghostel-agent--current-session ()
  "Return the current ghostel agent session, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (or (when-let* ((id (bound-and-true-p ghostel-agent--session-id))
                    (session (ghostel-agent--live-session-by-id id)))
          session)
        (ghostel-agent--session-for-buffer (current-buffer)))))

(defun ghostel-agent--sessions-for-root (root &optional agent)
  "Return live sessions for ROOT.
When AGENT is non-nil, restrict the result to that agent."
  (let ((root (ghostel-agent--normalize-root root)))
    (ghostel-agent--cleanup-sessions)
    (seq-filter (lambda (session)
                  (and (equal root (plist-get session :root))
                       (or (null agent)
                           (eq agent (plist-get session :agent)))))
                (mapcar #'cdr ghostel-agent--sessions))))

(defun ghostel-agent--last (items)
  "Return the last element of ITEMS."
  (car (last items)))

(defun ghostel-agent--selected-session (root)
  "Return the selected live session for ROOT, or nil."
  (when-let* ((id (alist-get (ghostel-agent--normalize-root root)
                             ghostel-agent--selected-session-alist
                             nil nil #'equal)))
    (ghostel-agent--live-session-by-id id)))

(defun ghostel-agent--selected-agent (root)
  "Return the selected agent for ROOT, or nil."
  (when-let* ((session (ghostel-agent--selected-session root)))
    (plist-get session :agent)))

(defun ghostel-agent--select-agent (root agent)
  "Mark AGENT's latest session as selected for ROOT."
  (when-let* ((session (ghostel-agent--last-session-for-agent agent root)))
    (ghostel-agent--select-session session)))

(defun ghostel-agent--project-root ()
  "Return the project root, or `default-directory' as fallback."
  (ghostel-agent--normalize-root
   (or (and (fboundp 'projectile-project-root)
            (ignore-errors (projectile-project-root)))
       default-directory)))

(defun ghostel-agent--next-label (agent root)
  "Return the display label for a new AGENT session in ROOT."
  (let* ((base (ghostel-agent--agent-name agent))
         (count (1+ (length (ghostel-agent--sessions-for-root root agent)))))
    (if (= count 1)
        base
      (format "%s %d" base count))))

(defun ghostel-agent--install-buffer-locals (session)
  "Install ghostel agent buffer-local state for SESSION."
  (when-let* ((buf (ghostel-agent--session-buffer session)))
    (with-current-buffer buf
      (setq ghostel-agent--session-id (plist-get session :id)
            ghostel-agent--agent (plist-get session :agent)
            ghostel-agent--project-root (plist-get session :root))
      (ghostel-agent-session-mode 1)
      (setq-local tab-line-format '(:eval (ghostel-agent--tab-line))))))

(defun ghostel-agent--register-session (agent root buf &optional resume label id)
  "Register BUF as an AGENT session in ROOT.
When RESUME is non-nil, the session was started in resume mode.
LABEL, when non-nil, overrides the generated tab label.
ID, when non-nil, is used as the session id."
  (let* ((root (ghostel-agent--normalize-root root))
         (id (or id (ghostel-agent--next-session-id)))
         (session (list :id id
                        :agent agent
                        :root root
                        :buffer buf
                        :resume resume
                        :label (or label (ghostel-agent--next-label agent root))
                        :created-at (float-time)
                        :last-selected nil)))
    (setq ghostel-agent--sessions
          (append ghostel-agent--sessions (list (cons id session))))
    (ghostel-agent--install-buffer-locals session)
    session))

(defun ghostel-agent--refresh-tab-lines (&optional root)
  "Refresh tab lines for ghostel agent sessions.
When ROOT is non-nil, refresh only sessions in that project."
  (let ((root (and root (ghostel-agent--normalize-root root))))
    (ghostel-agent--cleanup-sessions)
    (dolist (session (mapcar #'cdr ghostel-agent--sessions))
      (when (or (null root)
                (equal root (plist-get session :root)))
        (ghostel-agent--install-buffer-locals session)))
    (force-mode-line-update t)))

(defun ghostel-agent--select-session (session)
  "Mark SESSION as the selected session for its project."
  (when (ghostel-agent--session-live-p session)
    (let ((id (plist-get session :id))
          (agent (plist-get session :agent))
          (root (ghostel-agent--normalize-root (plist-get session :root))))
      (plist-put session :root root)
      (setf (alist-get root ghostel-agent--selected-session-alist
                       nil nil #'equal)
            id)
      (setf (alist-get (ghostel-agent--key agent root)
                       ghostel-agent--last-session-alist nil nil #'equal)
            id)
      (plist-put session :last-selected (float-time))
      (ghostel-agent--install-buffer-locals session)
      (ghostel-agent--refresh-tab-lines root)
      session)))

(defun ghostel-agent--last-session-for-agent (agent root)
  "Return AGENT's latest selected live session in ROOT, or nil."
  (or (when-let* ((id (alist-get (ghostel-agent--key agent root)
                                 ghostel-agent--last-session-alist
                                 nil nil #'equal)))
        (ghostel-agent--live-session-by-id id))
      (ghostel-agent--last (ghostel-agent--sessions-for-root root agent))))

(defun ghostel-agent--get-buffer (agent root)
  "Return AGENT's latest live ghostel buffer for ROOT, or nil."
  (when-let* ((session (ghostel-agent--last-session-for-agent agent root)))
    (ghostel-agent--session-buffer session)))

(defun ghostel-agent--get-window (agent root)
  "Return a window displaying AGENT's latest buffer for ROOT, or nil."
  (when-let* ((buf (ghostel-agent--get-buffer agent root)))
    (get-buffer-window buf t)))

(defun ghostel-agent--agents-for-root (root)
  "Return agents with live buffers for ROOT."
  (delete-dups (mapcar (lambda (session)
                         (plist-get session :agent))
                       (ghostel-agent--sessions-for-root root))))

(defun ghostel-agent--default-agent (root)
  "Return the default agent for ROOT."
  (or (when-let* ((session (ghostel-agent--default-session root)))
        (plist-get session :agent))
      'claude))

(defun ghostel-agent--default-session (root)
  "Return the default session for a plain `s-l' in ROOT, or nil."
  (let ((root (ghostel-agent--normalize-root root)))
    (or (ghostel-agent--selected-session root)
        (let ((current (ghostel-agent--current-session)))
          (when (and current
                     (equal root (plist-get current :root)))
            current))
        (let ((sessions (ghostel-agent--sessions-for-root root)))
          (cond
           ((null sessions) nil)
           ((null (cdr sessions)) (car sessions))
           (t (ghostel-agent--last sessions)))))))

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
  "Send COMMAND to the shell running in BUF, then press Return.
Uses the public ghostel input API rather than writing to
`ghostel--process' directly: since v0.26.0 that variable is the
native event pipe (not the shell) whenever Ghostel owns the PTY, so
`process-send-string' no longer reaches the shell.  Typing the
command as keystrokes also avoids bracketed-paste protection, which
would otherwise insert the trailing newline literally instead of
executing the command."
  (when (and (buffer-live-p buf)
             (process-live-p (buffer-local-value 'ghostel--process buf)))
    (with-current-buffer buf
      (ghostel-send-string command)
      (ghostel-send-key "return"))))

(defun ghostel-agent--sidebar-window-p (win)
  "Return non-nil when WIN is the ghostel agent side window."
  (and (window-live-p win)
       (eq (window-parameter win 'window-side) ghostel-agent-side)
       (eql (window-parameter win 'window-slot) 0)))

(defun ghostel-agent--sidebar-window ()
  "Return the ghostel agent side window for the selected frame, or nil."
  (seq-find #'ghostel-agent--sidebar-window-p
            (window-list (selected-frame) 'no-minibuf)))

(defun ghostel-agent--remember-last-window ()
  "Remember the current non-sidebar window."
  (unless (ghostel-agent--sidebar-window-p (selected-window))
    (setq ghostel-agent--last-window (selected-window))))

(defun ghostel-agent--display-sidebar-window (buf)
  "Display BUF in a side window and return that window."
  (let ((win (ghostel-agent--sidebar-window)))
    (if (window-live-p win)
        (progn
          (set-window-dedicated-p win nil)
          (set-window-buffer win buf)
          win)
      (display-buffer-in-side-window
       buf `((side . ,ghostel-agent-side)
             (slot . 0)
             (window-width . ,ghostel-agent-width)
             (window-parameters . ((no-delete-other-windows . t))))))))

(defun ghostel-agent--display-fullscreen-window (buf)
  "Display BUF in the current full-frame window and return it.
Used while a session is shown fullscreen so creating or switching
sessions reuses that window instead of spawning a side window."
  (let ((win (selected-window)))
    (set-window-dedicated-p win nil)
    (set-window-buffer win buf)
    win))

(defun ghostel-agent--display-buffer-in-sidebar (buf _alist)
  "Display BUF for `display-buffer'.
Targets the side window, or the current full-frame window while a
session is shown fullscreen."
  (let ((buffer (get-buffer buf)))
    (when (and buffer
               (with-current-buffer buffer
                 (derived-mode-p 'ghostel-mode)))
      (if ghostel-agent--fullscreen-config
          (ghostel-agent--display-fullscreen-window buffer)
        (ghostel-agent--display-sidebar-window buffer)))))

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
         (id (ghostel-agent--next-session-id))
         (identity (ghostel-agent--buffer-name
                    agent root id))
         buf
         session)
    (let ((display-buffer-overriding-action
           '((ghostel-agent--display-buffer-in-sidebar))))
      (let ((ghostel-buffer-name identity)
            (default-directory root))
        (setq buf (ghostel nil))))
    (setq session (ghostel-agent--register-session agent root buf resume nil id))
    (ghostel-agent--select-session session)
    (let ((win (or (get-buffer-window buf t)
                   (if ghostel-agent--fullscreen-config
                       (ghostel-agent--display-fullscreen-window buf)
                     (ghostel-agent--display-sidebar-window buf)))))
      (unless ghostel-agent--fullscreen-config
        (ghostel-agent--finish-sidebar-window win))
      (ghostel-agent--install-buffer-locals session)
      (run-at-time ghostel-agent-command-delay nil
                   #'ghostel-agent--send-command buf command)
      (ghostel-agent--refresh-tab-lines root)
      win)))

(defun ghostel-agent--show-sidebar (buf)
  "Display BUF in a side window."
  (let ((win (ghostel-agent--display-sidebar-window buf)))
    (ghostel-agent--finish-sidebar-window win)
    win))

(defun ghostel-agent--show-session (session)
  "Display SESSION and return its window.
Normally this targets the side window, but while a session is shown
fullscreen (see `ghostel-agent-toggle-fullscreen') it reuses the
current full-frame window so switching sessions stays fullscreen."
  (unless (ghostel-agent--session-live-p session)
    (user-error "Ghostel agent session is no longer live"))
  (ghostel-agent--select-session session)
  (let ((buf (plist-get session :buffer)))
    (if ghostel-agent--fullscreen-config
        (ghostel-agent--display-fullscreen-window buf)
      (ghostel-agent--show-sidebar buf))))

(defun ghostel-agent--show-session-by-id (id)
  "Display ghostel agent session ID."
  (interactive)
  (let ((session (ghostel-agent--live-session-by-id id)))
    (unless session
      (user-error "Ghostel agent session is no longer live"))
    (ghostel-agent--remember-last-window)
    (select-window (ghostel-agent--show-session session))))

(defun ghostel-agent--previous-session (session)
  "Return the live session before SESSION in the same project, or nil."
  (let* ((root (plist-get session :root))
         (id (plist-get session :id))
         (sessions (ghostel-agent--sessions-for-root root))
         (others (seq-remove (lambda (candidate)
                               (equal id (plist-get candidate :id)))
                             sessions))
         (index (cl-position id sessions
                             :key (lambda (candidate)
                                    (plist-get candidate :id))
                             :test #'equal)))
    (when others
      (if (and index (> index 0))
          (nth (1- index) sessions)
        (ghostel-agent--last others)))))

(defun ghostel-agent--show-session-in-window (session win)
  "Show SESSION in WIN and return WIN."
  (unless (ghostel-agent--session-live-p session)
    (user-error "Ghostel agent session is no longer live"))
  (ghostel-agent--select-session session)
  (if (window-live-p win)
      (let ((buf (ghostel-agent--session-buffer session)))
        (set-window-dedicated-p win nil)
        (set-window-buffer win buf)
        (ghostel-agent--finish-sidebar-window win))
    (ghostel-agent--show-session session)))

(defun ghostel-agent--after-exit (buf _event)
  "Keep the sidebar open on BUF exit when another session is live."
  (when-let* ((session (ghostel-agent--session-for-buffer buf)))
    (let ((root (plist-get session :root))
          (win (get-buffer-window buf t))
          (next (ghostel-agent--previous-session session)))
      (when next
        (ghostel-agent--show-session-in-window next win))
      (run-at-time 0 nil
                   (lambda (root)
                     (ghostel-agent--cleanup-sessions)
                     (ghostel-agent--refresh-tab-lines root))
                   root))))

(defun ghostel-agent--tab-line-tab (session selected-id)
  "Return a tab-line button for SESSION.
SELECTED-ID is the selected session id for the current root."
  (let* ((id (plist-get session :id))
         (label (plist-get session :label))
         (selected (equal id selected-id))
         (map (make-sparse-keymap))
         (text (if selected
                   (format " [%s] " label)
                 (format "  %s  " label))))
    (define-key map [tab-line mouse-1]
                (lambda ()
                  (interactive)
                  (ghostel-agent--show-session-by-id id)))
    (define-key map [mouse-1]
                (lambda ()
                  (interactive)
                  (ghostel-agent--show-session-by-id id)))
    (propertize text
                'face (if selected
                          'ghostel-agent-tab-current
                        'ghostel-agent-tab)
                'mouse-face 'tab-line-highlight
                'local-map map
                'help-echo "mouse-1: switch ghostel agent session")))

(defun ghostel-agent--tab-line ()
  "Return the ghostel agent tab line for the current buffer."
  (let* ((root (ghostel-agent--current-root))
         (sessions (and root (ghostel-agent--sessions-for-root root)))
         (selected (and root (ghostel-agent--selected-session root)))
         (current (ghostel-agent--current-session))
         (selected-id (or (plist-get current :id)
                          (plist-get selected :id))))
    (when (> (length sessions) 1)
      (apply #'concat
             " "
             (mapcar (lambda (session)
                       (ghostel-agent--tab-line-tab session selected-id))
                     sessions)))))

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

(defun ghostel-agent-toggle-session (session)
  "Smart toggle for SESSION's ghostel sidebar.
1. Not visible → show (create if needed).
2. Visible + focused → hide.
3. Visible + not focused + region → send region & focus.
4. Visible + not focused → focus."
  (let* ((buf (ghostel-agent--session-buffer session))
         (win (and buf (get-buffer-window buf t)))
         (in-sidebar (and buf (eq (current-buffer) buf))))
    (ghostel-agent--select-session session)
    (cond
     ;; Fullscreen → collapse back to the sidebar.
     (ghostel-agent--fullscreen-config
      (ghostel-agent--exit-fullscreen))
     ;; Visible + focused → hide
     ((and win in-sidebar)
      (when (and ghostel-agent--last-window
                 (window-live-p ghostel-agent--last-window))
        (select-window ghostel-agent--last-window))
      (delete-window win))
     ;; Visible + not focused + region → send region & focus
     ((and win (use-region-p))
      (ghostel-agent--send-region buf)
      (ghostel-agent--remember-last-window)
      (select-window win))
     ;; Visible + not focused → focus
     (win
      (ghostel-agent--remember-last-window)
      (select-window win))
     ;; Buffer exists but not visible → show
     (buf
      (ghostel-agent--remember-last-window)
      (select-window (ghostel-agent--show-sidebar buf)))
     (t
      (user-error "Ghostel agent session is no longer live")))))

(defun ghostel-agent--fullscreen-target ()
  "Return the ghostel agent buffer to fullscreen, or nil.
Prefer the focused buffer, then the sidebar buffer, then this
project's default session."
  (or (and (derived-mode-p 'ghostel-mode) (current-buffer))
      (when-let* ((win (ghostel-agent--sidebar-window)))
        (window-buffer win))
      (when-let* ((root (or (ghostel-agent--current-root)
                            (ghostel-agent--project-root)))
                  (session (ghostel-agent--default-session root)))
        (ghostel-agent--session-buffer session))))

(defun ghostel-agent--enter-fullscreen (buf)
  "Expand BUF to fill the frame, saving the layout for later restore."
  (setq ghostel-agent--fullscreen-config (current-window-configuration))
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer buf)
  ;; Bind `ignore-window-parameters' so the dedicated sidebar (and any
  ;; other side windows) collapse too, giving a truly full-frame buffer.
  (let ((ignore-window-parameters t))
    (delete-other-windows)))

(defun ghostel-agent--exit-fullscreen ()
  "Restore the window layout saved before going fullscreen."
  (let ((config ghostel-agent--fullscreen-config)
        (buf (and (derived-mode-p 'ghostel-mode) (current-buffer))))
    (setq ghostel-agent--fullscreen-config nil)
    (when (window-configuration-p config)
      (set-window-configuration config))
    ;; Reflect any session switch made while fullscreen back into the sidebar.
    (when-let* ((live (and (buffer-live-p buf) buf))
                (session (ghostel-agent--session-for-buffer live)))
      (ghostel-agent--show-session session))))

(defun ghostel-agent-toggle-fullscreen ()
  "Toggle the active ghostel agent session between the sidebar and fullscreen.
When the session is shown in the sidebar, expand it to fill the frame for
distraction-free reading; invoke again to restore the previous layout and
return the session to the sidebar.  Creating (`s-t') and switching
(`s-<left>'/`s-<right>') sessions stay fullscreen, with the tab line listing
every session."
  (interactive)
  (if ghostel-agent--fullscreen-config
      (ghostel-agent--exit-fullscreen)
    (let ((buf (ghostel-agent--fullscreen-target)))
      (unless buf
        (user-error "No ghostel agent session to show fullscreen"))
      (ghostel-agent--enter-fullscreen buf))))

(defun ghostel-agent-toggle (agent resume &optional _force-show root)
  "Compatibility wrapper for toggling AGENT in ROOT.
When no AGENT session exists, create one.  RESUME controls only creation."
  (let* ((root (or root
                   (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root))
         (session (ghostel-agent--last-session-for-agent agent root)))
    (if session
        (ghostel-agent-toggle-session session)
      (ghostel-agent--remember-last-window)
      (select-window (ghostel-agent--create agent root resume)))))

(defun ghostel-agent--parse-prefix (arg)
  "Return (AGENT RESUME) for raw prefix ARG.
Plain commands target the selected session for this project.
`C-u' targets Claude, creating with `--resume' when needed.
`C-2' targets Codex.
`C-3' targets Codex, creating with `resume' when needed."
  (cond
   ((null arg) '(nil nil))
   ((equal arg '(4)) '(claude t))
   ((equal arg 2) '(codex nil))
   ((equal arg 3) '(codex t))
   (t (user-error "Unknown ghostel agent prefix: %S" arg))))

(defun ghostel-agent-toggle-command (arg)
  "Toggle a ghostel agent sidebar based on prefix ARG.
Plain `s-l' toggles the selected session for this project.
`C-u s-l' toggles the latest Claude session, creating a resume
session if none exists.  `C-2 s-l' toggles the latest Codex session,
and `C-3 s-l' creates a Codex resume session only when no Codex
session exists yet."
  (interactive "P")
  (let* ((parsed (ghostel-agent--parse-prefix arg))
         (agent (car parsed))
         (resume (cadr parsed))
         (root (or (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root))
         (session (if agent
                      (ghostel-agent--last-session-for-agent agent root)
                    (ghostel-agent--default-session root))))
    (if session
        (ghostel-agent-toggle-session session)
      (ghostel-agent--remember-last-window)
      (select-window (ghostel-agent--create (or agent 'claude)
                                            root resume)))))

(defun ghostel-agent-new-session-command (arg)
  "Create and show a new ghostel agent session based on prefix ARG.
Plain `s-t' creates Claude, `C-u s-t' creates Claude resume,
`C-2 s-t' creates Codex, and `C-3 s-t' creates Codex resume."
  (interactive "P")
  (let* ((parsed (ghostel-agent--parse-prefix arg))
         (agent (or (car parsed) 'claude))
         (resume (cadr parsed))
         (root (or (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root)))
    (ghostel-agent--remember-last-window)
    (select-window (ghostel-agent--create agent root resume))))

(defun ghostel-agent-cycle-session (delta)
  "Cycle the selected ghostel agent session by DELTA."
  (let* ((root (or (ghostel-agent--current-root)
                   (ghostel-agent--project-root)))
         (root (ghostel-agent--normalize-root root))
         (sessions (ghostel-agent--sessions-for-root root))
         (selected (or (ghostel-agent--selected-session root)
                       (ghostel-agent--current-session)
                       (car sessions))))
    (unless sessions
      (user-error "No ghostel agent sessions for this project"))
    (let* ((len (length sessions))
           (index (or (cl-position (plist-get selected :id)
                                   sessions
                                   :key (lambda (session)
                                          (plist-get session :id))
                                   :test #'equal)
                      0))
           (next (nth (mod (+ index delta) len) sessions)))
      (ghostel-agent--remember-last-window)
      (select-window (ghostel-agent--show-session next)))))

(defun ghostel-agent-next-session ()
  "Switch to the next ghostel agent session for this project."
  (interactive)
  (ghostel-agent-cycle-session 1))

(defun ghostel-agent-previous-session ()
  "Switch to the previous ghostel agent session for this project."
  (interactive)
  (ghostel-agent-cycle-session -1))

(defun ghostel-claude-toggle (arg)
  "Backward-compatible wrapper for `ghostel-agent-toggle-command'."
  (interactive "P")
  (ghostel-agent-toggle-command arg))

(add-hook 'ghostel-exit-functions #'ghostel-agent--after-exit)

(global-set-key (kbd "s-l") #'ghostel-agent-toggle-command)
(global-set-key (kbd "s-t") #'ghostel-agent-new-session-command)
(global-set-key (kbd "s-<return>") #'ghostel-agent-toggle-fullscreen)

;;; ghostel-agents.el ends here

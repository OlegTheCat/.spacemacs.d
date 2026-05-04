;;; ghostel-toggle.el --- Project Ghostel terminal toggle -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'ghostel)

(defvar ghostel-toggle--sessions nil
  "Alist mapping Ghostel terminal session ids to session plists.")

(defvar ghostel-toggle--selected-session-alist nil
  "Alist mapping project-root strings to selected terminal session ids.")

(defvar ghostel-toggle--session-counter 0
  "Monotonic counter used to allocate Ghostel terminal session ids.")

(defvar-local ghostel-toggle--session-id nil
  "Session id this Ghostel terminal buffer belongs to.")

(defvar-local ghostel-toggle--project-root nil
  "Project root this Ghostel terminal buffer belongs to.")

(defvar ghostel-toggle--last-window nil
  "Window that was selected before jumping to the terminal drawer.")

(defvar ghostel-toggle-side 'bottom
  "Side of the frame for the terminal drawer window.")

(defvar ghostel-toggle-height 0.5
  "Height of the terminal drawer as a fraction of the frame.")

(defface ghostel-toggle-tab-current
  '((t :inherit tab-line-tab-current :weight bold :underline t))
  "Face for the selected Ghostel terminal tab.")

(defface ghostel-toggle-tab
  '((t :inherit tab-line-tab))
  "Face for inactive Ghostel terminal tabs.")

(defvar ghostel-toggle-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-<left>") #'ghostel-toggle-previous-session)
    (define-key map (kbd "s-<right>") #'ghostel-toggle-next-session)
    (define-key map (kbd "s-t") #'ghostel-toggle-new-session)
    map)
  "Keymap active in managed Ghostel terminal buffers.")

(define-minor-mode ghostel-toggle-session-mode
  "Minor mode for Ghostel terminal buffers managed by `ghostel-toggle'."
  :init-value nil
  :lighter nil
  :keymap ghostel-toggle-session-mode-map)

(defun ghostel-toggle--normalize-root (root)
  "Return ROOT as a canonical project directory string."
  (file-name-as-directory (expand-file-name root)))

(defun ghostel-toggle--project-root ()
  "Return the project root, or `default-directory' as fallback."
  (ghostel-toggle--normalize-root
   (or (and (fboundp 'projectile-project-root)
            (ignore-errors (projectile-project-root)))
       (and (fboundp 'project-current)
            (when-let* ((project (project-current nil)))
              (project-root project)))
       default-directory)))

(defun ghostel-toggle--buffer-name (root session-id)
  "Return the project/session-specific Ghostel identity for ROOT."
  (let* ((root (ghostel-toggle--normalize-root root))
         (dir (directory-file-name root))
         (project-name (file-name-nondirectory dir))
         (project-label (if (string= project-name "") "root" project-name))
         (root-hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "*ghostel-terminal:%s:%s:%s*" project-label root-hash session-id)))

(defun ghostel-toggle--next-session-id ()
  "Return a fresh Ghostel terminal session id."
  (setq ghostel-toggle--session-counter
        (1+ ghostel-toggle--session-counter))
  (format "ghostel-terminal-session-%d" ghostel-toggle--session-counter))

(defun ghostel-toggle--session-by-id (id)
  "Return the session plist for ID, or nil."
  (cdr (assoc id ghostel-toggle--sessions)))

(defun ghostel-toggle--session-buffer (session)
  "Return SESSION's live buffer, or nil."
  (let ((buf (plist-get session :buffer)))
    (when (buffer-live-p buf)
      buf)))

(defun ghostel-toggle--session-live-p (session)
  "Return non-nil when SESSION has a live buffer."
  (and (plist-get session :id)
       (ghostel-toggle--session-buffer session)))

(defun ghostel-toggle--live-session-by-id (id)
  "Return the live session plist for ID, or nil."
  (let ((session (ghostel-toggle--session-by-id id)))
    (when (and session (ghostel-toggle--session-live-p session))
      session)))

(defun ghostel-toggle--cleanup-sessions ()
  "Drop registry entries whose buffers were killed."
  (let (live-ids)
    (setq ghostel-toggle--sessions
          (cl-remove-if-not
           (lambda (entry)
             (when (ghostel-toggle--session-live-p (cdr entry))
               (push (car entry) live-ids)
               t))
           ghostel-toggle--sessions))
    (setq ghostel-toggle--selected-session-alist
          (cl-remove-if-not
           (lambda (entry) (member (cdr entry) live-ids))
           ghostel-toggle--selected-session-alist))))

(defun ghostel-toggle--session-for-buffer (buf)
  "Return the session plist for BUF, or nil."
  (seq-find (lambda (session)
              (eq (plist-get session :buffer) buf))
            (mapcar #'cdr ghostel-toggle--sessions)))

(defun ghostel-toggle--current-root ()
  "Return the project root for the current managed Ghostel terminal, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (let ((root (or (when-let* ((id (bound-and-true-p ghostel-toggle--session-id))
                                (session (ghostel-toggle--live-session-by-id id)))
                      (plist-get session :root))
                    (bound-and-true-p ghostel-toggle--project-root))))
      (when root
        (ghostel-toggle--normalize-root root)))))

(defun ghostel-toggle--current-session ()
  "Return the current managed Ghostel terminal session, or nil."
  (when (derived-mode-p 'ghostel-mode)
    (or (when-let* ((id (bound-and-true-p ghostel-toggle--session-id))
                    (session (ghostel-toggle--live-session-by-id id)))
          session)
        (ghostel-toggle--session-for-buffer (current-buffer)))))

(defun ghostel-toggle--sessions-for-root (root)
  "Return live terminal sessions for ROOT."
  (let ((root (ghostel-toggle--normalize-root root)))
    (ghostel-toggle--cleanup-sessions)
    (seq-filter (lambda (session)
                  (equal root (plist-get session :root)))
                (mapcar #'cdr ghostel-toggle--sessions))))

(defun ghostel-toggle--last (items)
  "Return the last element of ITEMS."
  (car (last items)))

(defun ghostel-toggle--selected-session (root)
  "Return the selected live terminal session for ROOT, or nil."
  (when-let* ((id (alist-get (ghostel-toggle--normalize-root root)
                             ghostel-toggle--selected-session-alist
                             nil nil #'equal)))
    (ghostel-toggle--live-session-by-id id)))

(defun ghostel-toggle--default-session (root)
  "Return the default session for a plain `s-i' in ROOT, or nil."
  (let ((root (ghostel-toggle--normalize-root root)))
    (or (ghostel-toggle--selected-session root)
        (let ((current (ghostel-toggle--current-session)))
          (when (and current
                     (equal root (plist-get current :root)))
            current))
        (ghostel-toggle--last (ghostel-toggle--sessions-for-root root)))))

(defun ghostel-toggle--next-label (root)
  "Return the display label for a new terminal session in ROOT."
  (let ((labels (mapcar (lambda (session)
                          (plist-get session :label))
                        (ghostel-toggle--sessions-for-root root)))
        (n 1)
        label)
    (while (member (setq label (if (= n 1)
                                   "Terminal"
                                 (format "Terminal %d" n)))
                   labels)
      (setq n (1+ n)))
    label))

(defun ghostel-toggle--install-buffer-locals (session)
  "Install Ghostel terminal buffer-local state for SESSION."
  (when-let* ((buf (ghostel-toggle--session-buffer session)))
    (with-current-buffer buf
      (setq ghostel-toggle--session-id (plist-get session :id)
            ghostel-toggle--project-root (plist-get session :root))
      (ghostel-toggle-session-mode 1)
      (setq-local tab-line-format '(:eval (ghostel-toggle--tab-line))))))

(defun ghostel-toggle--register-session (root buf &optional label)
  "Register BUF as a terminal session in ROOT.
LABEL, when non-nil, overrides the generated tab label."
  (let* ((root (ghostel-toggle--normalize-root root))
         (id (ghostel-toggle--next-session-id))
         (session (list :id id
                        :root root
                        :buffer buf
                        :label (or label (ghostel-toggle--next-label root))
                        :created-at (float-time)
                        :last-selected nil)))
    (setq ghostel-toggle--sessions
          (append ghostel-toggle--sessions (list (cons id session))))
    (ghostel-toggle--install-buffer-locals session)
    session))

(defun ghostel-toggle--refresh-tab-lines (&optional root)
  "Refresh tab lines for managed Ghostel terminals.
When ROOT is non-nil, refresh only sessions in that project."
  (let ((root (and root (ghostel-toggle--normalize-root root))))
    (ghostel-toggle--cleanup-sessions)
    (dolist (session (mapcar #'cdr ghostel-toggle--sessions))
      (when (or (null root)
                (equal root (plist-get session :root)))
        (ghostel-toggle--install-buffer-locals session)))
    (force-mode-line-update t)))

(defun ghostel-toggle--select-session (session)
  "Mark SESSION as the selected terminal session for its project."
  (when (ghostel-toggle--session-live-p session)
    (let ((id (plist-get session :id))
          (root (ghostel-toggle--normalize-root (plist-get session :root))))
      (plist-put session :root root)
      (setf (alist-get root ghostel-toggle--selected-session-alist
                       nil nil #'equal)
            id)
      (plist-put session :last-selected (float-time))
      (ghostel-toggle--install-buffer-locals session)
      (ghostel-toggle--refresh-tab-lines root)
      session)))

(defun ghostel-toggle--terminal-window-p (win)
  "Return non-nil when WIN is the Ghostel terminal drawer window."
  (and (window-live-p win)
       (window-parameter win 'ghostel-toggle-window)))

(defun ghostel-toggle--terminal-window ()
  "Return the Ghostel terminal drawer window for the selected frame, or nil."
  (seq-find #'ghostel-toggle--terminal-window-p
            (window-list (selected-frame) 'no-minibuf)))

(defun ghostel-toggle--remember-last-window ()
  "Remember the current non-terminal-drawer window."
  (unless (ghostel-toggle--terminal-window-p (selected-window))
    (setq ghostel-toggle--last-window (selected-window))))

(defun ghostel-toggle--display-terminal-window (buf)
  "Display BUF in the terminal drawer and return its window."
  (let ((win (ghostel-toggle--terminal-window)))
    (if (window-live-p win)
        (progn
          (set-window-dedicated-p win nil)
          (set-window-buffer win buf)
          (set-window-parameter win 'ghostel-toggle-window t)
          win)
      (display-buffer-in-side-window
       buf `((side . ,ghostel-toggle-side)
             (slot . 0)
             (window-height . ,ghostel-toggle-height)
             (window-parameters . ((ghostel-toggle-window . t)
                                   (no-delete-other-windows . t))))))))

(defun ghostel-toggle--finish-terminal-window (win)
  "Apply terminal drawer window settings to WIN."
  (when (window-live-p win)
    (set-window-dedicated-p win t)
    (window-preserve-size win nil t))
  win)

(defun ghostel-toggle--show-session-in-window (session win)
  "Display SESSION in WIN when possible, falling back to the drawer."
  (unless (ghostel-toggle--session-live-p session)
    (user-error "Ghostel terminal session is no longer live"))
  (ghostel-toggle--select-session session)
  (if (window-live-p win)
      (let ((buf (ghostel-toggle--session-buffer session)))
        (set-window-dedicated-p win nil)
        (set-window-buffer win buf)
        (set-window-parameter win 'ghostel-toggle-window t)
        (ghostel-toggle--finish-terminal-window win))
    (ghostel-toggle--show-session session)))

(defun ghostel-toggle--create (root)
  "Create a new Ghostel terminal in ROOT and return its window."
  (let* ((root (ghostel-toggle--normalize-root root))
         (default-directory root)
         (buf (generate-new-buffer "*ghostel-terminal*"))
         (session (ghostel-toggle--register-session root buf))
         (identity (ghostel-toggle--buffer-name root (plist-get session :id)))
         (setup-locals (lambda ()
                         (when (eq (current-buffer) buf)
                           (ghostel-toggle--install-buffer-locals session)))))
    (with-current-buffer buf
      (setq default-directory root)
      (rename-buffer identity t))
    (ghostel-toggle--select-session session)
    (let ((win (ghostel-toggle--display-terminal-window buf)))
      (select-window win)
      (unwind-protect
          (progn
            (add-hook 'ghostel-mode-hook setup-locals)
            (let ((ghostel-buffer-name identity)
                  (default-directory root))
              (setq buf (ghostel nil))))
        (remove-hook 'ghostel-mode-hook setup-locals))
      (plist-put session :buffer buf)
      (ghostel-toggle--finish-terminal-window win)
      (ghostel-toggle--install-buffer-locals session)
      (ghostel-toggle--refresh-tab-lines root)
      win)))

(defun ghostel-toggle--show-session (session)
  "Display SESSION in the Ghostel terminal drawer and return its window."
  (unless (ghostel-toggle--session-live-p session)
    (user-error "Ghostel terminal session is no longer live"))
  (ghostel-toggle--select-session session)
  (let ((win (ghostel-toggle--display-terminal-window
              (ghostel-toggle--session-buffer session))))
    (ghostel-toggle--finish-terminal-window win)))

(defun ghostel-toggle--show-session-by-id (id)
  "Display Ghostel terminal session ID."
  (interactive)
  (let ((session (ghostel-toggle--live-session-by-id id)))
    (unless session
      (user-error "Ghostel terminal session no longer exists"))
    (ghostel-toggle--remember-last-window)
    (select-window (ghostel-toggle--show-session session))))

(defun ghostel-toggle--previous-session (session)
  "Return the live session before SESSION in the same project, or nil."
  (when (ghostel-toggle--session-live-p session)
    (let* ((root (plist-get session :root))
           (session-id (plist-get session :id))
           (sessions (ghostel-toggle--sessions-for-root root))
           (ids (mapcar (lambda (candidate)
                          (plist-get candidate :id))
                        sessions))
           (position (cl-position session-id ids :test #'equal)))
      (cond
       ((null (cdr sessions)) nil)
       ((and position (> position 0))
        (nth (1- position) sessions))
       (t
        (ghostel-toggle--last (remove session sessions)))))))

(defun ghostel-toggle--after-exit (buf _event)
  "Keep the terminal drawer visible when BUF exits and another tab exists."
  (when-let* ((session (ghostel-toggle--session-for-buffer buf)))
    (let* ((root (plist-get session :root))
           (win (get-buffer-window buf t))
           (next (ghostel-toggle--previous-session session)))
      (if next
          (ghostel-toggle--show-session-in-window next win)
        (run-at-time 0 nil
                     (lambda (root)
                       (ghostel-toggle--cleanup-sessions)
                       (ghostel-toggle--refresh-tab-lines root))
                     root)))))

(defun ghostel-toggle--kill-buffer-hook ()
  "Refresh terminal session state after a managed buffer is killed."
  (when-let* ((session (ghostel-toggle--session-for-buffer (current-buffer)))
              (root (plist-get session :root)))
    (run-at-time 0 nil
                 (lambda (root)
                   (ghostel-toggle--cleanup-sessions)
                   (ghostel-toggle--refresh-tab-lines root))
                 root)))

(defun ghostel-toggle--tab-line-tab (session selected-id)
  "Return a tab-line button for SESSION.
SELECTED-ID is the selected session id for this project."
  (let* ((id (plist-get session :id))
         (selected (equal id selected-id))
         (label (plist-get session :label))
         (map (let ((map (make-sparse-keymap)))
                (define-key map [tab-line mouse-1]
                            (lambda ()
                              (interactive)
                              (ghostel-toggle--show-session-by-id id)))
                (define-key map [mouse-1]
                            (lambda ()
                              (interactive)
                              (ghostel-toggle--show-session-by-id id)))
                map)))
    (propertize (if selected
                    (format " [%s] " label)
                  (format " %s " label))
                'face (if selected
                          'ghostel-toggle-tab-current
                        'ghostel-toggle-tab)
                'mouse-face 'tab-line-highlight
                'local-map map
                'help-echo "mouse-1: switch Ghostel terminal tab")))

(defun ghostel-toggle--tab-line ()
  "Return the Ghostel terminal tab line for the current buffer."
  (let* ((root (ghostel-toggle--current-root))
         (sessions (and root (ghostel-toggle--sessions-for-root root)))
         (selected (and root (ghostel-toggle--selected-session root)))
         (current (ghostel-toggle--current-session))
         (selected-id (or (and selected (plist-get selected :id))
                          (and current (plist-get current :id)))))
    (when (cdr sessions)
      (append
       (list " ")
       (mapcan (lambda (session)
                 (list (ghostel-toggle--tab-line-tab session selected-id)
                       " "))
               sessions)))))

(defun ghostel-toggle--root-visible-p (root)
  "Return non-nil when ROOT has a session visible in the terminal drawer."
  (when-let* ((win (ghostel-toggle--terminal-window))
              (session (ghostel-toggle--session-for-buffer
                        (window-buffer win))))
    (equal (ghostel-toggle--normalize-root root)
           (plist-get session :root))))

(defun ghostel-toggle--hide-terminal-window ()
  "Hide the terminal drawer and restore the previous window when possible."
  (when-let* ((win (ghostel-toggle--terminal-window)))
    (unless (one-window-p t)
      (delete-window win))
    (when (and ghostel-toggle--last-window
               (window-live-p ghostel-toggle--last-window))
      (select-window ghostel-toggle--last-window))))

(defun ghostel-toggle-new-session ()
  "Create a new Ghostel terminal tab for the current project."
  (interactive)
  (let* ((root (or (ghostel-toggle--current-root)
                   (ghostel-toggle--project-root)))
         (root (ghostel-toggle--normalize-root root)))
    (ghostel-toggle--remember-last-window)
    (select-window (ghostel-toggle--create root))))

(defun ghostel-toggle ()
  "Toggle the project Ghostel terminal drawer.
With a prefix argument, create a new terminal tab for the project."
  (interactive)
  (let* ((root (or (ghostel-toggle--current-root)
                   (ghostel-toggle--project-root)))
         (root (ghostel-toggle--normalize-root root)))
    (if current-prefix-arg
        (ghostel-toggle-new-session)
      (let ((session (ghostel-toggle--default-session root)))
        (cond
         ((ghostel-toggle--root-visible-p root)
          (ghostel-toggle--hide-terminal-window))
         (session
          (ghostel-toggle--remember-last-window)
          (select-window (ghostel-toggle--show-session session)))
         (t
          (ghostel-toggle--remember-last-window)
          (select-window (ghostel-toggle--create root))))))))

(defun ghostel-toggle-cycle-session (delta)
  "Cycle the selected Ghostel terminal session by DELTA."
  (let* ((root (or (ghostel-toggle--current-root)
                   (ghostel-toggle--project-root)))
         (root (ghostel-toggle--normalize-root root))
         (sessions (ghostel-toggle--sessions-for-root root))
         (selected (or (ghostel-toggle--selected-session root)
                       (ghostel-toggle--current-session)
                       (car sessions)))
         (selected-id (and selected (plist-get selected :id)))
         (ids (mapcar (lambda (session)
                        (plist-get session :id))
                      sessions)))
    (unless sessions
      (user-error "No Ghostel terminal sessions for this project"))
    (let* ((position (or (cl-position selected-id ids :test #'equal) 0))
           (next (nth (mod (+ position delta) (length sessions)) sessions)))
      (ghostel-toggle--remember-last-window)
      (select-window (ghostel-toggle--show-session next)))))

(defun ghostel-toggle-next-session ()
  "Switch to the next Ghostel terminal session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 1))

(defun ghostel-toggle-previous-session ()
  "Switch to the previous Ghostel terminal session for this project."
  (interactive)
  (ghostel-toggle-cycle-session -1))

(add-hook 'ghostel-exit-functions #'ghostel-toggle--after-exit)
(add-hook 'kill-buffer-hook #'ghostel-toggle--kill-buffer-hook)

(define-key ghostel-toggle-session-mode-map (kbd "s-t") #'ghostel-toggle-new-session)

(global-set-key (kbd "s-i") #'ghostel-toggle)

;;; ghostel-toggle.el ends here

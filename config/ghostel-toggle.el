;;; ghostel-toggle.el --- Generic ghostel panel session library -*- lexical-binding: t -*-

;; The shared machinery behind the project panels built on ghostel terminals:
;; the agent sidebar (`ghostel-agents.el') and the terminal drawer
;; (`ghostel-terminals.el').  Each panel family is a KIND registered with
;; `ghostel-toggle-define-kind'; sessions, selection, the side window, the
;; tab line, and the fullscreen state machine are all keyed by kind so the
;; two families never cross-talk: the drawer can never surface an agent and
;; the sidebar can never surface a terminal.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar ghostel-buffer-name)

;;; Kind registry

(defvar ghostel-toggle--kinds nil
  "Alist mapping kind symbols to kind config plists.
Config keys: `:side' (side window side), `:size' (fraction of the frame),
`:minor-mode' (the instantiation's minor mode symbol), `:on-select'
(optional function called with the session on every select).")

(defun ghostel-toggle-define-kind (kind &rest config)
  "Register panel KIND with CONFIG.  Re-registration replaces."
  (setf (alist-get kind ghostel-toggle--kinds) config)
  kind)

(defun ghostel-toggle--kind-get (kind prop)
  "Return PROP from KIND's config."
  (plist-get (alist-get kind ghostel-toggle--kinds) prop))

;;; Session registry

(defvar ghostel-toggle--sessions nil
  "Alist mapping session ids to session plists.
Session plists carry :id :kind :root :buffer :label :created-at
:last-selected plus any extra keys the instantiation registered.  The
registry stores the same plist object handed to callers — mutation via
`plist-put' is the contract, so every mutable key exists from
construction and lookups never copy.")

(defvar ghostel-toggle--selected-session-alist nil
  "Alist mapping (KIND . PROJECT-ROOT) keys to selected session ids.")

(defvar ghostel-toggle--session-counter 0
  "Monotonic counter used to allocate session ids across all kinds.")

(defvar ghostel-toggle--last-window-alist nil
  "Alist mapping kinds to the window selected before jumping to their panel.")

(defvar-local ghostel-toggle--session-id nil
  "Session id this managed ghostel buffer belongs to.")

(defvar-local ghostel-toggle--kind nil
  "Panel kind this managed ghostel buffer belongs to.")

(defvar-local ghostel-toggle--project-root nil
  "Project root this managed ghostel buffer belongs to.")

(defun ghostel-toggle--normalize-root (root)
  "Return ROOT as a canonical project directory string."
  (file-name-as-directory (expand-file-name root)))

(defun ghostel-toggle--project-root ()
  "Return the project root, or `default-directory' as fallback."
  (ghostel-toggle--normalize-root
   (or (and (fboundp 'projectile-project-root)
            (ignore-errors (projectile-project-root)))
       default-directory)))

(defun ghostel-toggle--key (kind root)
  "Return the selected-session key for KIND in ROOT."
  (cons kind (ghostel-toggle--normalize-root root)))

(defun ghostel-toggle--selected-id (kind root)
  "Return the selected session id for KIND in ROOT, or nil."
  (alist-get (ghostel-toggle--key kind root)
             ghostel-toggle--selected-session-alist nil nil #'equal))

(defun ghostel-toggle--set-selected-id (kind root id)
  "Record ID as the selected session for KIND in ROOT."
  (setf (alist-get (ghostel-toggle--key kind root)
                   ghostel-toggle--selected-session-alist nil nil #'equal)
        id))

(defun ghostel-toggle--buffer-name (name root session-id)
  "Return the project/session-specific ghostel identity for NAME in ROOT."
  ;; Ghostel reuses buffers by `ghostel--buffer-identity', even after
  ;; title tracking renames the visible buffer.
  (let* ((root (ghostel-toggle--normalize-root root))
         (dir (directory-file-name root))
         (project-name (file-name-nondirectory dir))
         (project-label (if (string= project-name "") "root" project-name))
         (root-hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "*ghostel-%s:%s:%s:%s*" name project-label root-hash session-id)))

(defun ghostel-toggle--next-session-id (kind)
  "Return a fresh session id for KIND."
  (setq ghostel-toggle--session-counter
        (1+ ghostel-toggle--session-counter))
  (format "ghostel-%s-session-%d" kind ghostel-toggle--session-counter))

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

(defun ghostel-toggle--current-session (&optional kind)
  "Return the current managed session, or nil.
When KIND is non-nil, only return a session of that kind."
  (when (derived-mode-p 'ghostel-mode)
    (let ((session
           (or (when-let* ((id ghostel-toggle--session-id))
                 (ghostel-toggle--live-session-by-id id))
               (ghostel-toggle--session-for-buffer (current-buffer)))))
      (when (and session
                 (or (null kind) (eq kind (plist-get session :kind))))
        session))))

(defun ghostel-toggle--current-root (&optional kind)
  "Return the project root for the current managed buffer, or nil.
When KIND is non-nil, only when the current buffer belongs to that kind."
  (when (derived-mode-p 'ghostel-mode)
    (let ((root (or (when-let* ((session (ghostel-toggle--current-session kind)))
                      (plist-get session :root))
                    (and (or (null kind) (eq kind ghostel-toggle--kind))
                         ghostel-toggle--project-root))))
      (when root
        (ghostel-toggle--normalize-root root)))))

(defun ghostel-toggle-command-root ()
  "Return the project root for a ghostel toggle command.
The current managed buffer's root (any kind — a command fired from the
other kind's panel targets that panel's project), else the project of
`default-directory'."
  (or (ghostel-toggle--current-root)
      (ghostel-toggle--project-root)))

(defun ghostel-toggle--sessions-for-root (kind root)
  "Return live KIND sessions for ROOT."
  (let ((root (ghostel-toggle--normalize-root root)))
    (ghostel-toggle--cleanup-sessions)
    (seq-filter (lambda (session)
                  (and (eq kind (plist-get session :kind))
                       (equal root (plist-get session :root))))
                (mapcar #'cdr ghostel-toggle--sessions))))

(defun ghostel-toggle--last (items)
  "Return the last element of ITEMS."
  (car (last items)))

(defun ghostel-toggle--selected-session (kind root)
  "Return the selected live KIND session for ROOT, or nil."
  (when-let* ((id (ghostel-toggle--selected-id kind root)))
    (ghostel-toggle--live-session-by-id id)))

(defun ghostel-toggle--default-session (kind root)
  "Return the default KIND session for a plain toggle in ROOT, or nil."
  (let ((root (ghostel-toggle--normalize-root root)))
    (or (ghostel-toggle--selected-session kind root)
        (let ((current (ghostel-toggle--current-session kind)))
          (when (and current
                     (equal root (plist-get current :root)))
            current))
        (ghostel-toggle--last (ghostel-toggle--sessions-for-root kind root)))))

(defun ghostel-toggle--default-label (kind root)
  "Return the default display label for a new KIND session in ROOT."
  (let* ((base (capitalize (symbol-name kind)))
         (count (1+ (length (ghostel-toggle--sessions-for-root kind root)))))
    (if (= count 1)
        base
      (format "%s %d" base count))))

(defun ghostel-toggle--install-buffer-locals (session)
  "Install managed buffer-local state for SESSION.
Skips sessions whose kind has no registered config."
  (when-let* ((buf (ghostel-toggle--session-buffer session))
              (kind (plist-get session :kind))
              (config (alist-get kind ghostel-toggle--kinds)))
    (with-current-buffer buf
      (setq ghostel-toggle--session-id (plist-get session :id)
            ghostel-toggle--kind kind
            ghostel-toggle--project-root (plist-get session :root))
      (when-let* ((mode (plist-get config :minor-mode)))
        (funcall mode 1))
      (setq-local tab-line-format '(:eval (ghostel-toggle--tab-line))))))

(cl-defun ghostel-toggle--register-session (kind root buf &key label extra id)
  "Register BUF as a KIND session in ROOT and return the session plist.
LABEL overrides the default label; EXTRA is a plist of instantiation
keys appended to the session; ID overrides the generated session id."
  (let* ((root (ghostel-toggle--normalize-root root))
         (id (or id (ghostel-toggle--next-session-id kind)))
         (session (append (list :id id
                                :kind kind
                                :root root
                                :buffer buf
                                :label (or label
                                           (ghostel-toggle--default-label kind root))
                                :created-at (float-time)
                                :last-selected nil)
                          extra)))
    (setq ghostel-toggle--sessions
          (append ghostel-toggle--sessions (list (cons id session))))
    (ghostel-toggle--install-buffer-locals session)
    session))

(defun ghostel-toggle--refresh-tab-lines (&optional root)
  "Refresh tab lines for managed sessions.
When ROOT is non-nil, refresh only sessions in that project."
  (let ((root (and root (ghostel-toggle--normalize-root root))))
    (ghostel-toggle--cleanup-sessions)
    (dolist (session (mapcar #'cdr ghostel-toggle--sessions))
      (when (or (null root)
                (equal root (plist-get session :root)))
        (ghostel-toggle--install-buffer-locals session)))
    (force-mode-line-update t)))

(defun ghostel-toggle--select-session (session)
  "Mark SESSION as the selected session of its kind for its project."
  (when (ghostel-toggle--session-live-p session)
    (let ((id (plist-get session :id))
          (kind (plist-get session :kind))
          (root (ghostel-toggle--normalize-root (plist-get session :root))))
      (plist-put session :root root)
      (ghostel-toggle--set-selected-id kind root id)
      (plist-put session :last-selected (float-time))
      (ghostel-toggle--install-buffer-locals session)
      (when-let* ((hook (ghostel-toggle--kind-get kind :on-select)))
        (funcall hook session))
      (ghostel-toggle--refresh-tab-lines root)
      session)))

(defun ghostel-toggle--previous-session (session)
  "Return the live session before SESSION in the same kind and project."
  (let* ((kind (plist-get session :kind))
         (root (plist-get session :root))
         (id (plist-get session :id))
         (sessions (ghostel-toggle--sessions-for-root kind root))
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
        (ghostel-toggle--last others)))))

;;; Panel (side) windows

(defun ghostel-toggle--panel-window-p (win kind)
  "Return non-nil when WIN is KIND's panel window."
  (and (window-live-p win)
       (eq (window-parameter win 'ghostel-toggle-kind) kind)))

(defun ghostel-toggle--panel-window (kind)
  "Return KIND's panel window on the selected frame, or nil."
  (seq-find (lambda (win) (ghostel-toggle--panel-window-p win kind))
            (window-list (selected-frame) 'no-minibuf)))

(defun ghostel-toggle--remember-last-window (kind)
  "Remember the selected window as the one to return to from KIND's panel."
  (unless (ghostel-toggle--panel-window-p (selected-window) kind)
    (setf (alist-get kind ghostel-toggle--last-window-alist)
          (selected-window))))

(defun ghostel-toggle--last-window (kind)
  "Return the live window to return to from KIND's panel, or nil."
  (let ((win (alist-get kind ghostel-toggle--last-window-alist)))
    (when (window-live-p win)
      win)))

(defun ghostel-toggle--display-panel-window (kind buf)
  "Display BUF in KIND's panel window and return that window."
  (let ((win (ghostel-toggle--panel-window kind))
        (side (ghostel-toggle--kind-get kind :side))
        (size (ghostel-toggle--kind-get kind :size)))
    (if (window-live-p win)
        (progn
          (set-window-dedicated-p win nil)
          (set-window-buffer win buf)
          (set-window-parameter win 'ghostel-toggle-kind kind)
          win)
      (let ((dimension (if (memq side '(left right))
                           'window-width
                         'window-height)))
        (display-buffer-in-side-window
         buf `((side . ,side)
               (slot . 0)
               (,dimension . ,size)
               (window-parameters . ((ghostel-toggle-kind . ,kind)
                                     (no-delete-other-windows . t)))))))))

(defun ghostel-toggle--finish-panel-window (kind win)
  "Apply KIND's panel window settings to WIN."
  (when (window-live-p win)
    (set-window-dedicated-p win t)
    (if (memq (ghostel-toggle--kind-get kind :side) '(left right))
        (window-preserve-size win t t)
      (window-preserve-size win nil t)))
  win)

(defun ghostel-toggle--show-panel (kind buf)
  "Display BUF in KIND's panel window and return that window."
  (let ((win (ghostel-toggle--display-panel-window kind buf)))
    (ghostel-toggle--finish-panel-window kind win)))

(defun ghostel-toggle--root-visible-p (kind root)
  "Return non-nil when ROOT has a session visible in KIND's panel."
  (when-let* ((win (ghostel-toggle--panel-window kind))
              (session (ghostel-toggle--session-for-buffer
                        (window-buffer win))))
    (equal (ghostel-toggle--normalize-root root)
           (plist-get session :root))))

(defun ghostel-toggle-hide-panel (kind)
  "Hide KIND's panel and restore the previous window when possible."
  (when-let* ((win (ghostel-toggle--panel-window kind)))
    (unless (one-window-p t)
      (delete-window win))
    (when-let* ((last (ghostel-toggle--last-window kind)))
      (select-window last))))

;;; Fullscreen views

(defvar ghostel-toggle--fullscreen-views nil
  "List of fullscreen views, one per (kind, project root) in fullscreen mode.
Each entry is a plist (:kind KIND :buffer BUF :root ROOT :config
WINDOW-CONFIG :hidden BOOL :dismiss BOOL :under VIEW): the session
buffer, the project root that keys the view, whether the session is
currently hidden while fullscreen mode persists, and whether hiding
dismisses the view outright (the home fullscreens).

Stacked fullscreens form an explicit chain: `:under' is the registered
view directly beneath this one (compared by `eq', nil at the bottom),
and `:config' is a snapshot of the last real, non-fullscreen layout —
never of a sibling fullscreen.  Hiding or demoting a view reveals its
`:under' view when one is live, and restores `:config' only at the
bottom of the stack; flipping a covered view back on top restacks the
chain (`ghostel-toggle--show-fullscreen'), and removing a view rebases
the view above it (`ghostel-toggle--rebase-child').  This keeps the
chain acyclic and the real layout recoverable no matter the order views
are shown, hidden, or demoted in.

Fullscreen mode is sticky — it lasts until `s-<return>' demotes it back
to the panel; the plain toggle key only flips the session's visibility
within that mode.  Keying by (kind, root) lets several projects, and
both kinds within one project, be in fullscreen mode independently.")

(defvar ghostel-toggle--fullscreen-display nil
  "When non-nil, the view whose full-frame window should receive a display.
Bound around session creation so the new buffer takes over the frame
instead of opening a side window.")

(defvar ghostel-toggle--display-kind nil
  "Kind whose panel should receive a `display-buffer' during creation.")

(defun ghostel-toggle--prune-fullscreen-views ()
  "Drop fullscreen views whose session buffer was killed."
  (setq ghostel-toggle--fullscreen-views
        (seq-filter (lambda (v) (buffer-live-p (plist-get v :buffer)))
                    ghostel-toggle--fullscreen-views)))

(defun ghostel-toggle--view-for-buffer (buffer)
  "Return the fullscreen view whose session buffer is BUFFER, or nil."
  (seq-find (lambda (v) (eq buffer (plist-get v :buffer)))
            ghostel-toggle--fullscreen-views))

(defun ghostel-toggle--view-for-root (kind root)
  "Return the fullscreen view (shown or hidden) for KIND in ROOT, or nil.
Keyed by kind and root rather than the current buffer so the plain
toggle key and `s-<return>' find a project's fullscreen session even
while it is hidden and point is on a code buffer."
  (ghostel-toggle--prune-fullscreen-views)
  (let ((root (ghostel-toggle--normalize-root root)))
    (seq-find (lambda (v) (and (eq kind (plist-get v :kind))
                               (equal root (plist-get v :root))))
              ghostel-toggle--fullscreen-views)))

(defun ghostel-toggle--current-fullscreen-view (kind)
  "Return the KIND view shown full-frame in the selected window, or nil.
The session must be the selected buffer and fill the frame, the view
must not be hidden, and it must belong to KIND.  Display routing
\(session switching, new sessions) uses this so a split pane, a hidden
session, or the other kind's fullscreen is never mistaken for the
fullscreen window."
  (ghostel-toggle--prune-fullscreen-views)
  (and (one-window-p t)
       (let ((v (ghostel-toggle--view-for-buffer (current-buffer))))
         (and v
              (eq kind (plist-get v :kind))
              (not (plist-get v :hidden))
              v))))

(defun ghostel-toggle--shown-view ()
  "Return the view whose buffer fills the frame's main window, or nil.
Unlike `ghostel-toggle--current-fullscreen-view' this is keyed by the
main window rather than point and ignores side windows, so a drawer
punched over a fullscreen doesn't unmake it.  Used for the `:under'
nesting bookkeeping, not for display routing."
  (ghostel-toggle--prune-fullscreen-views)
  (let ((main (window-main-window)))
    (and (window-live-p main)
         (let ((v (ghostel-toggle--view-for-buffer (window-buffer main))))
           (and v (not (plist-get v :hidden)) v)))))

(defun ghostel-toggle--normalize-full-frame-window (win)
  "Make WIN an ordinary full-frame window fit to host a session buffer.
Undedicate it and strip the `window-side'/`window-slot'/
`no-delete-other-windows'/`ghostel-toggle-kind' parameters a former
panel leaves behind; left in place they make Emacs refuse
`switch-to-buffer', can trap the session in a phantom side slot, and
keep the full-frame window answering as the panel window.  Returns WIN."
  (when (window-live-p win)
    (set-window-dedicated-p win nil)
    (set-window-parameter win 'window-side nil)
    (set-window-parameter win 'window-slot nil)
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-parameter win 'ghostel-toggle-kind nil))
  win)

(defun ghostel-toggle--display-fullscreen-window (buf &optional view)
  "Display session BUF in the current full-frame window and return it.
Repoints VIEW's session side to BUF (defaulting to the shown view of
BUF's kind) so creating or switching sessions stays fullscreen."
  (when-let* ((view (or view
                        (when-let* ((session (ghostel-toggle--session-for-buffer buf)))
                          (ghostel-toggle--current-fullscreen-view
                           (plist-get session :kind))))))
    (plist-put view :buffer buf))
  (let ((win (selected-window)))
    (set-window-dedicated-p win nil)
    (set-window-buffer win buf)
    (ghostel-toggle--normalize-full-frame-window win)))

(defun ghostel-toggle--fill-frame (buf)
  "Show BUF in the selected window and expand it to fill the frame."
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer buf)
  ;; Bind `ignore-window-parameters' so the dedicated panels (and any
  ;; other side windows) collapse too, giving a truly full-frame buffer.
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  ;; A promoted panel keeps its side-window parameters, which make Emacs
  ;; refuse `switch-to-buffer'; normalize so the full-frame window behaves
  ;; like an ordinary one (exit restores the real panel from the saved
  ;; config).
  (ghostel-toggle--normalize-full-frame-window (selected-window)))

(defun ghostel-toggle--capture-restore-config (buf)
  "Return the window configuration to restore when BUF's fullscreen ends.
Normally the current layout, but if BUF already fills the frame as the sole
window (e.g. a home key re-enters after the view was lost), that layout
would just re-show BUF — dismissing it could never reveal the code.
Snapshot the window's previous buffer instead, so demoting/hiding returns
to real content."
  (if (and (one-window-p t) (eq (current-buffer) buf))
      (save-window-excursion
        (let ((win (selected-window)))
          (set-window-dedicated-p win nil)
          (switch-to-prev-buffer win))
        (current-window-configuration))
    (current-window-configuration)))

(defun ghostel-toggle--enter-fullscreen (buf &optional dismiss)
  "Expand session BUF to fill the frame, registering a fullscreen view.
The view is keyed by BUF's session kind and root so the plain toggle key
and `s-<return>' can find it later even while it is hidden and point is
on a code buffer.  When DISMISS is non-nil, hiding the view removes it
outright instead of sticky-hiding it — used for the home fullscreens, so
the plain toggle key dismisses them and the home key re-fires them fresh."
  (let* ((session (ghostel-toggle--session-for-buffer buf))
         (kind (plist-get session :kind))
         (root (plist-get session :root))
         (covering (ghostel-toggle--shown-view)))
    ;; Entering over another fullscreen stacks on top of it: the covered
    ;; view is the reveal target, and the real-layout snapshot is
    ;; inherited from it rather than pointing at its full-frame buffer.
    (push (list :kind kind
                :buffer buf
                :root (and root (ghostel-toggle--normalize-root root))
                :config (if covering
                            (plist-get covering :config)
                          (ghostel-toggle--capture-restore-config buf))
                :hidden nil
                :dismiss dismiss
                :under covering)
          ghostel-toggle--fullscreen-views))
  (ghostel-toggle--fill-frame buf))

(defun ghostel-toggle--show-fullscreen (view)
  "Show VIEW's session fullscreen and mark the view visible.
Fullscreen is sticky: the plain toggle key uses this to (re-)expand the
session after it was hidden, split, or covered.  Over a real (non-
fullscreen) layout the snapshot is re-taken so hiding/demoting returns
to the current splits; over another shown fullscreen the view is
restacked on top of it instead — snapshots never point at a sibling
fullscreen, which would lose the underlying layout and resurrect
demoted sessions when unwinding."
  (let ((buf (plist-get view :buffer))
        (covering (ghostel-toggle--shown-view)))
    (unless (buffer-live-p buf)
      (user-error "Fullscreen session buffer is no longer live"))
    (cond
     ((and covering (not (eq covering view)))
      ;; Restack: unlink from the old stack position, push on top.
      (ghostel-toggle--rebase-child view)
      (plist-put view :under covering))
     ((not (eq (current-buffer) buf))
      (plist-put view :under nil)
      (plist-put view :config (current-window-configuration))))
    (plist-put view :hidden nil)
    (ghostel-toggle--fill-frame buf)))

(defun ghostel-toggle--under-view (view)
  "Return VIEW's `:under' view when it is still a live reveal target.
The target must still be registered, not hidden, and have a live buffer;
a stale link (its view was removed) is skipped."
  (let ((under (plist-get view :under)))
    (and under
         (memq under ghostel-toggle--fullscreen-views)
         (not (plist-get under :hidden))
         (buffer-live-p (plist-get under :buffer))
         under)))

(defun ghostel-toggle--rebase-child (view)
  "Unlink VIEW from the fullscreen stack.
The view entered over VIEW (`:under' eq VIEW) inherits VIEW's `:under'
and real-layout `:config', so unwinding it later skips VIEW instead of
revealing a removed session."
  (when-let* ((child (seq-find (lambda (v) (eq (plist-get v :under) view))
                               ghostel-toggle--fullscreen-views)))
    (plist-put child :config (plist-get view :config))
    (plist-put child :under (plist-get view :under))))

(defun ghostel-toggle--sweep-view-windows (view buf)
  "Delete windows showing BUF or any hidden view's buffer other than VIEW's.
A restored configuration may predate another view's hiding and resurrect
its buffer full-frame; sweeping keeps hidden views hidden.  Windows that
cannot be deleted (the frame's sole/main window) show their previous
buffer instead of a wedged session."
  (dolist (b (cons buf
                   (mapcar (lambda (v) (plist-get v :buffer))
                           (seq-filter (lambda (v)
                                         (and (not (eq v view))
                                              (plist-get v :hidden)))
                                       ghostel-toggle--fullscreen-views))))
    (dolist (win (get-buffer-window-list b nil nil))
      (if (window-deletable-p win)
          (delete-window win)
        (set-window-dedicated-p win nil)
        (switch-to-prev-buffer win)))))

(defun ghostel-toggle--remove-view (view)
  "Deregister VIEW and unwind the frame appropriately.
The view above VIEW in the stack (if any) is rebased so unwinding skips
VIEW.  When VIEW is on top (its buffer fills the frame's main window),
reveal the view beneath it, or — at the bottom of the stack — restore
the real-layout snapshot, sweeping buffers of still-hidden views it may
resurrect.  When VIEW is buried (a covered session dying), the current
layout is left alone."
  (let ((top (eq (ghostel-toggle--shown-view) view))
        (under (ghostel-toggle--under-view view)))
    (setq ghostel-toggle--fullscreen-views
          (delq view ghostel-toggle--fullscreen-views))
    (ghostel-toggle--rebase-child view)
    (when top
      (if under
          (ghostel-toggle--fill-frame (plist-get under :buffer))
        (when (window-configuration-p (plist-get view :config))
          (set-window-configuration (plist-get view :config)))
        (ghostel-toggle--sweep-view-windows view (plist-get view :buffer))))))

(defun ghostel-toggle--exit-fullscreen (view)
  "Unwind and deregister VIEW (see `ghostel-toggle--remove-view')."
  (let ((buf (plist-get view :buffer)))
    (ghostel-toggle--remove-view view)
    ;; Reflect the session that was active in fullscreen (it may differ from
    ;; the saved snapshot after cycling) back into its panel — except for
    ;; `:dismiss' (home) views, which must never leak into a project panel.
    (unless (plist-get view :dismiss)
      (when-let* ((live (and (buffer-live-p buf) buf))
                  (session (ghostel-toggle--session-for-buffer live)))
        (ghostel-toggle--show-session session)))))

(defun ghostel-toggle--hide-fullscreen (view)
  "Hide VIEW's session, collapsing to whatever it covered.
Reveals the fullscreen beneath VIEW when there is one, else restores the
real-layout snapshot and removes the session's window(s).  A sticky VIEW
is marked hidden so the next plain toggle re-expands it (only
`s-<return>' leaves that mode); a `:dismiss' VIEW (the home fullscreens)
is removed outright so the plain toggle dismisses it and the home key
re-fires it.  The restored layout may predate another view's hiding and
resurrect that view's buffer full-frame; windows of every still-hidden
view are swept too so hidden views stay hidden."
  (let ((buf (plist-get view :buffer))
        (under (ghostel-toggle--under-view view)))
    (if (plist-get view :dismiss)
        (progn
          (setq ghostel-toggle--fullscreen-views
                (delq view ghostel-toggle--fullscreen-views))
          (ghostel-toggle--rebase-child view))
      (plist-put view :hidden t))
    (if under
        (ghostel-toggle--fill-frame (plist-get under :buffer))
      (when (window-configuration-p (plist-get view :config))
        (set-window-configuration (plist-get view :config)))
      (ghostel-toggle--sweep-view-windows view buf))
    (when-let* ((last (ghostel-toggle--last-window (plist-get view :kind))))
      (select-window last))))

(defun ghostel-toggle--fullscreen-flip (view &optional before-show)
  "Plain-toggle-key action for fullscreen VIEW (sticky fullscreen mode).
On the session shown full-frame, hide it (collapse to the code).
Otherwise (hidden, split, or on another buffer) re-expand the session to
fullscreen, calling BEFORE-SHOW first when non-nil (only on the show
path — instantiations use it to send an active region)."
  (if (and (not (plist-get view :hidden))
           (eq (current-buffer) (plist-get view :buffer))
           (one-window-p t))
      (ghostel-toggle--hide-fullscreen view)
    (when before-show
      (funcall before-show))
    (ghostel-toggle--show-fullscreen view)))

(defun ghostel-toggle-fullscreen-command ()
  "Toggle fullscreen for the focused ghostel session.
Promotion is focus-based: the current buffer must be a managed session
buffer — its kind decides which fullscreen is toggled, so a terminal and
an agent fullscreen can coexist for one project.  From any other buffer
this errors; focus the panel first (`s-l' / `s-i').  When the session's
\(kind, root) already holds a fullscreen view, demote it back to the
panel and restore the saved layout; otherwise promote the session to
fill the frame.

Fullscreen is a sticky mode: once promoted, the kind's plain toggle key
only flips the session's visibility (hide to the code / re-expand), and
this command is the only way to demote the session back to the panel."
  (interactive)
  (let ((session (ghostel-toggle--current-session)))
    (unless session
      (user-error "No ghostel session focused (use s-l / s-i first)"))
    (let* ((kind (plist-get session :kind))
           (root (plist-get session :root))
           (view (ghostel-toggle--view-for-root kind root)))
      (if view
          (ghostel-toggle--exit-fullscreen view)
        (ghostel-toggle--enter-fullscreen (current-buffer))))))

(defun ghostel-toggle-home-toggle (kind get-buffer)
  "Toggle a dismissable fullscreen KIND session in the home directory.
GET-BUFFER is called with the normalized home root and must return a
live session buffer, creating the session if needed.  The home
fullscreen is registered `:dismiss': the kind's plain toggle key
dismisses it outright and the home key re-fires it fresh.  Keyed by the
home root through the same machinery as the project fullscreens, so the
two cannot collide."
  (let* ((root (ghostel-toggle--normalize-root "~/"))
         (view (ghostel-toggle--view-for-root kind root)))
    (cond
     ;; On top → demote.  :dismiss views restore the layout without the
     ;; trailing panel re-show, so demoting cannot leak the home session
     ;; into the current project's panel.
     ((and view (eq (ghostel-toggle--shown-view) view))
      (ghostel-toggle--exit-fullscreen view))
     ;; Covered by the other home fullscreen (or otherwise not shown) →
     ;; one press brings it back on top instead of dismissing it
     ;; invisibly.
     (view
      (ghostel-toggle--show-fullscreen view))
     (t
      (let ((buf (funcall get-buffer root)))
        (unless (buffer-live-p buf)
          (user-error "Could not start home %s session" kind))
        (ghostel-toggle--remember-last-window kind)
        (ghostel-toggle--enter-fullscreen buf t))))))

;;; Session display

(defun ghostel-toggle--show-session (session)
  "Display SESSION and return its window.
Normally this targets the kind's panel window, but while a session of
the same kind is shown fullscreen it reuses the current full-frame
window so switching sessions stays fullscreen."
  (unless (ghostel-toggle--session-live-p session)
    (user-error "Ghostel session is no longer live"))
  (ghostel-toggle--select-session session)
  (let* ((kind (plist-get session :kind))
         (buf (plist-get session :buffer))
         (view (ghostel-toggle--current-fullscreen-view kind)))
    (if view
        (ghostel-toggle--display-fullscreen-window buf view)
      (ghostel-toggle--show-panel kind buf))))

(defun ghostel-toggle--show-session-by-id (id)
  "Display ghostel session ID."
  (let ((session (ghostel-toggle--live-session-by-id id)))
    (unless session
      (user-error "Ghostel session is no longer live"))
    (ghostel-toggle--remember-last-window (plist-get session :kind))
    (select-window (ghostel-toggle--show-session session))))

(defun ghostel-toggle--show-session-in-window (session win)
  "Show SESSION in WIN and return WIN."
  (unless (ghostel-toggle--session-live-p session)
    (user-error "Ghostel session is no longer live"))
  (ghostel-toggle--select-session session)
  (if (window-live-p win)
      (let ((kind (plist-get session :kind))
            (buf (ghostel-toggle--session-buffer session)))
        (set-window-dedicated-p win nil)
        (set-window-buffer win buf)
        ;; Only a real panel window gets panel finishing; dedicating an
        ;; exited fullscreen session's sole window would wedge the frame.
        (when (ghostel-toggle--panel-window-p win kind)
          (ghostel-toggle--finish-panel-window kind win))
        win)
    (ghostel-toggle--show-session session)))

(defun ghostel-toggle-cycle-session (kind delta)
  "Cycle the selected KIND session for this project by DELTA."
  (let* ((root (or (ghostel-toggle--current-root kind)
                   (ghostel-toggle--project-root)))
         (root (ghostel-toggle--normalize-root root))
         (sessions (ghostel-toggle--sessions-for-root kind root))
         (selected (or (ghostel-toggle--selected-session kind root)
                       (ghostel-toggle--current-session kind)
                       (car sessions))))
    (unless sessions
      (user-error "No ghostel %s sessions for this project" kind))
    (let* ((len (length sessions))
           (index (or (cl-position (plist-get selected :id)
                                   sessions
                                   :key (lambda (session)
                                          (plist-get session :id))
                                   :test #'equal)
                      0))
           (next (nth (mod (+ index delta) len) sessions)))
      (ghostel-toggle--remember-last-window kind)
      (select-window (ghostel-toggle--show-session next)))))

;;; Creation

(defun ghostel-toggle--display-buffer-in-panel (buf _alist)
  "Display BUF for `display-buffer' during managed session creation.
Targets the current full-frame window when
`ghostel-toggle--fullscreen-display' names the active view, else the
panel window of `ghostel-toggle--display-kind'."
  (let ((buffer (get-buffer buf)))
    (when (and buffer
               (with-current-buffer buffer
                 (derived-mode-p 'ghostel-mode)))
      (if ghostel-toggle--fullscreen-display
          (ghostel-toggle--display-fullscreen-window
           buffer ghostel-toggle--fullscreen-display)
        (ghostel-toggle--display-panel-window
         ghostel-toggle--display-kind buffer)))))

(cl-defun ghostel-toggle-create-session (kind root &key name label extra setup)
  "Create a new ghostel KIND session in ROOT and return its window.
NAME is the buffer-identity component (defaults to KIND's name); LABEL
overrides the default tab label; EXTRA is a plist of instantiation keys
stored on the session; SETUP, when non-nil, is called with the new
buffer after registration and display (e.g. to type an agent command)."
  (let* ((root (ghostel-toggle--normalize-root root))
         (default-directory root)
         (id (ghostel-toggle--next-session-id kind))
         (identity (ghostel-toggle--buffer-name
                    (or name (symbol-name kind)) root id))
         (fs-view (ghostel-toggle--current-fullscreen-view kind))
         buf
         session)
    (let ((display-buffer-overriding-action
           '((ghostel-toggle--display-buffer-in-panel)))
          (ghostel-toggle--display-kind kind)
          (ghostel-toggle--fullscreen-display fs-view))
      (let ((ghostel-buffer-name identity))
        (setq buf (ghostel nil))))
    (setq session (ghostel-toggle--register-session kind root buf
                                                    :label label
                                                    :extra extra
                                                    :id id))
    (ghostel-toggle--select-session session)
    (let ((win (or (get-buffer-window buf t)
                   (if fs-view
                       (ghostel-toggle--display-fullscreen-window buf fs-view)
                     (ghostel-toggle--display-panel-window kind buf)))))
      (unless fs-view
        (ghostel-toggle--finish-panel-window kind win))
      (ghostel-toggle--install-buffer-locals session)
      (when setup
        (funcall setup buf))
      (ghostel-toggle--refresh-tab-lines root)
      win)))

(cl-defun ghostel-toggle--create-session-hidden (kind root &rest args)
  "Create a KIND session in ROOT without touching the current layout.
ARGS are passed to `ghostel-toggle-create-session'.  Isolated from any
current fullscreen view so e.g. promoting the home session from inside a
project's fullscreen cannot hijack that project's view."
  (save-window-excursion
    (let ((ghostel-toggle--fullscreen-views nil))
      (apply #'ghostel-toggle-create-session kind root args))))

;;; Exit / kill handling

(defun ghostel-toggle--schedule-refresh (root)
  "Cleanup dead sessions and refresh ROOT's tab lines after this command."
  (run-at-time 0 nil
               (lambda (root)
                 (ghostel-toggle--cleanup-sessions)
                 (ghostel-toggle--refresh-tab-lines root))
               root))

(defun ghostel-toggle--after-exit (buf _event)
  "Keep BUF's window on the next live session when BUF's process exits."
  (when-let* ((session (ghostel-toggle--session-for-buffer buf)))
    (let ((root (plist-get session :root))
          (win (get-buffer-window buf t))
          (next (ghostel-toggle--previous-session session))
          (view (ghostel-toggle--view-for-buffer buf)))
      (when next
        ;; Exiting while fullscreen: repoint the view at the successor so
        ;; fullscreen mode survives instead of dying with the killed buffer.
        (when view
          (plist-put view :buffer (ghostel-toggle--session-buffer next)))
        (ghostel-toggle--show-session-in-window next win))
      (ghostel-toggle--schedule-refresh root))))

(defun ghostel-toggle--kill-buffer-hook ()
  "Restore state when a managed buffer is killed.
A buffer that still owns a fullscreen view here had no successor for
`ghostel-toggle--after-exit' to repoint the view to (or was killed by
hand); unwind the view so its saved layout isn't silently discarded
with the buffer, stranding the frame on a fallback buffer.  Panels need
no such handling — their windows are dedicated, so killing the buffer
deletes the window."
  (when-let* ((session (ghostel-toggle--session-for-buffer (current-buffer)))
              (root (plist-get session :root)))
    (when-let* ((view (ghostel-toggle--view-for-buffer (current-buffer))))
      (ghostel-toggle--remove-view view))
    (ghostel-toggle--schedule-refresh root)))

;;; Tab line

(defface ghostel-toggle-tab-current
  '((t :inherit tab-line-tab-current :weight bold :underline nil))
  "Face for the selected ghostel session tab.")

(defface ghostel-toggle-tab
  '((t :inherit tab-line-tab))
  "Face for inactive ghostel session tabs.")

(set-face-attribute 'ghostel-toggle-tab-current nil
                    :inherit 'tab-line-tab-current
                    :weight 'bold
                    :underline nil)

(defun ghostel-toggle--tab-line-tab (session selected-id)
  "Return a tab-line button for SESSION.
SELECTED-ID is the selected session id for the current kind and root."
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
                  (ghostel-toggle--show-session-by-id id)))
    (define-key map [mouse-1]
                (lambda ()
                  (interactive)
                  (ghostel-toggle--show-session-by-id id)))
    (propertize text
                'face (if selected
                          'ghostel-toggle-tab-current
                        'ghostel-toggle-tab)
                'mouse-face 'tab-line-highlight
                'local-map map
                'help-echo "mouse-1: switch ghostel session")))

(defun ghostel-toggle--tab-line ()
  "Return the ghostel session tab line for the current buffer.
Lists only sessions of this buffer's kind in this buffer's project."
  (let* ((kind ghostel-toggle--kind)
         (root (and kind (ghostel-toggle--current-root kind)))
         (sessions (and root (ghostel-toggle--sessions-for-root kind root)))
         (selected (and root (ghostel-toggle--selected-session kind root)))
         (current (ghostel-toggle--current-session kind))
         (selected-id (or (plist-get current :id)
                          (plist-get selected :id))))
    (when (> (length sessions) 1)
      (apply #'concat
             " "
             (mapcar (lambda (session)
                       (ghostel-toggle--tab-line-tab session selected-id))
                     sessions)))))

(add-hook 'ghostel-exit-functions #'ghostel-toggle--after-exit)
(add-hook 'kill-buffer-hook #'ghostel-toggle--kill-buffer-hook)

(global-set-key (kbd "s-<return>") #'ghostel-toggle-fullscreen-command)

(provide 'ghostel-toggle)

;;; ghostel-toggle.el ends here

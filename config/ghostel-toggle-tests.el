;;; ghostel-toggle-tests.el --- ERT tests for ghostel-toggle + instantiations -*- lexical-binding: t -*-

;; Not loaded at startup (absent from the `dotspacemacs/user-config' dolist).
;;
;; Run headless:   ./run-tests.sh              (everything)
;;                 ./run-tests.sh "gtl-.*"     (library: registry, panels, fullscreen)
;;                 ./run-tests.sh "gta-.*"     (agents instantiation)
;;                 ./run-tests.sh "gtt-.*"     (terminals instantiation)
;;                 ./run-tests.sh "gtx-.*"     (cross-kind isolation)
;;
;; Sections:
;;   A. gtl-      library: pure helpers, registry, selection
;;   B. gtl-win-  library: panel (side window) behavior
;;   C. gtl-fs-   library: fullscreen state machine + s-<return> command
;;   D. gta-      agents instantiation
;;   E. gtt-      terminals instantiation
;;   F. gtx-      cross-kind isolation
;;
;; Fake, process-less buffers in `ghostel-mode' stand in for real shells —
;; no claude/codex/zsh is spawned.  Batch Emacs runs split-window /
;; display-buffer-in-side-window / set-window-configuration faithfully, so
;; window geometry is asserted just like in a GUI.

(require 'ert)
(require 'cl-lib)

;;; --- shared harness ----------------------------------------------------------

(defvar gt--test-buffers nil
  "Buffers created by the running test, killed on cleanup.")

(defvar gt--last-paste nil
  "Last string handed to the stubbed `ghostel-paste-string'.")

(defmacro gt-with-env (&rest body)
  "Run BODY with fresh ghostel-toggle state, a real window tree, and stubs.
Let-binds the complete library state (plus the agents' last-session
alist), runs BODY from a single clean window, restores the window
configuration and kills created buffers afterwards.
`projectile-project-root' is forced to nil so project roots resolve
deterministically via `default-directory'."
  (declare (indent 0) (debug t))
  `(let ((ghostel-toggle--kinds ghostel-toggle--kinds)
         (ghostel-toggle--sessions nil)
         (ghostel-toggle--selected-session-alist nil)
         (ghostel-toggle--session-counter 0)
         (ghostel-toggle--fullscreen-views nil)
         (ghostel-toggle--last-window-alist nil)
         (ghostel-agent--last-session-alist nil)
         (gt--test-buffers nil)
         (gt--last-paste nil)
         (window-min-width 2)
         (window-min-height 2)
         (orig (current-window-configuration)))
     (cl-letf (((symbol-function 'ghostel)
                (lambda (&rest _) (gt--fake-buffer)))
               ((symbol-function 'ghostel-agent--send-command) #'ignore)
               ((symbol-function 'ghostel-send-string) #'ignore)
               ((symbol-function 'ghostel-send-key) #'ignore)
               ((symbol-function 'ghostel-paste-string)
                (lambda (s) (setq gt--last-paste s)))
               ((symbol-function 'projectile-project-root) (lambda (&rest _) nil)))
       (unwind-protect
           (progn (delete-other-windows) ,@body)
         (set-window-configuration orig)
         (dolist (buf gt--test-buffers)
           (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun gt--fake-buffer ()
  "Return a fresh buffer that masquerades as a ghostel session buffer.
Setting `major-mode' is enough for `derived-mode-p' — no process needed."
  (let ((buf (generate-new-buffer " *gt-session*")))
    (with-current-buffer buf (setq major-mode 'ghostel-mode))
    (push buf gt--test-buffers)
    buf))

(defun gta--register (agent root)
  "Register a fake AGENT session in ROOT through the real code path."
  (ghostel-toggle--register-session
   'agent root (gt--fake-buffer)
   :label (ghostel-agent--next-label agent root)
   :extra (list :agent agent :resume nil)))

(defun gtt--register (root)
  "Register a fake terminal session in ROOT through the real code path."
  (ghostel-toggle--register-session
   'terminal root (gt--fake-buffer)
   :label (ghostel-terminal--next-label root)))

(defun gt--id (session) (and session (plist-get session :id)))

(defun gt--code-buffer (root &optional file)
  "Return a fresh non-session buffer whose `default-directory' is ROOT.
With FILE, also set `buffer-file-name' to ROOT/FILE."
  (let ((buf (generate-new-buffer " *gt-code*"))
        (dir (ghostel-toggle--normalize-root root)))
    (with-current-buffer buf
      (setq default-directory dir)
      (when file (setq buffer-file-name (expand-file-name file dir))))
    (push buf gt--test-buffers)
    buf))

(defun gt--show-code (root &optional file)
  "Show a code buffer for ROOT in the selected window; make it current."
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer (gt--code-buffer root file))
  (current-buffer))

(defun gt--win-count ()
  "Number of live non-minibuffer windows in the selected frame."
  (length (window-list nil 'no-minibuf)))

(defun gt--session-buffer-p (buf)
  "Return non-nil when BUF is a registered session buffer."
  (and (buffer-live-p buf) (ghostel-toggle--session-for-buffer buf) t))

(defun gt--panel-shows-session-p (kind)
  "Return non-nil when KIND's panel window shows a session buffer."
  (let ((w (ghostel-toggle--panel-window kind)))
    (and w (gt--session-buffer-p (window-buffer w)))))

(defun gt--fullscreen-now-p (buf)
  "Return non-nil when BUF fills the frame as the sole window."
  (and (one-window-p t) (eq (window-buffer (selected-window)) buf)))

(defun gt--select-region (buf beg end)
  "Activate a region BEG..END in BUF (transient-mark bound by the caller)."
  (with-current-buffer buf
    (set-mark beg)
    (goto-char end)
    (setq mark-active t)))

;;; ============================================================================
;;; A. gtl- — library: pure helpers, registry, selection
;;; ============================================================================

(ert-deftest gtl-normalize-root ()
  (should (string-suffix-p "/" (ghostel-toggle--normalize-root "/tmp/foo")))
  (should (equal (ghostel-toggle--normalize-root "/tmp/foo")
                 (ghostel-toggle--normalize-root "/tmp/foo/"))))

(ert-deftest gtl-define-kind-registers-and-overwrites ()
  (gt-with-env
    (ghostel-toggle-define-kind 'gt-scratch :side 'left :size 0.3)
    (should (eq (ghostel-toggle--kind-get 'gt-scratch :side) 'left))
    (should (= (ghostel-toggle--kind-get 'gt-scratch :size) 0.3))
    (ghostel-toggle-define-kind 'gt-scratch :side 'top :size 0.2)
    (should (eq (ghostel-toggle--kind-get 'gt-scratch :side) 'top))))

(ert-deftest gtl-buffer-name-embeds-name-project-hash-id ()
  (should (string-match
           "\\`\\*ghostel-claude:projA:[0-9a-f]\\{8\\}:my-id\\*\\'"
           (ghostel-toggle--buffer-name "claude" "/tmp/projA" "my-id")))
  ;; the filesystem root gets a stable label
  (should (string-match ":root:" (ghostel-toggle--buffer-name "terminal" "/" "x"))))

(ert-deftest gtl-session-ids-unique-across-kinds ()
  "One counter feeds every kind; ids are kind-qualified and never collide."
  (gt-with-env
    (let ((t1 (gtt--register "/tmp/projA/"))
          (a1 (gta--register 'claude "/tmp/projA/")))
      (should (string-prefix-p "ghostel-terminal-session-" (gt--id t1)))
      (should (string-prefix-p "ghostel-agent-session-" (gt--id a1)))
      (should-not (equal (gt--id t1) (gt--id a1))))))

(ert-deftest gtl-register-stores-kind-and-extra ()
  "Sessions carry :kind plus instantiation extras, and the registry stores
the very plist object handed back (aliasing is the mutation contract)."
  (gt-with-env
    (let* ((s (ghostel-toggle--register-session
               'agent "/tmp/projA/" (gt--fake-buffer)
               :extra '(:agent claude :resume t)))
           (entry (ghostel-toggle--session-by-id (gt--id s))))
      (should (eq (plist-get s :kind) 'agent))
      (should (eq (plist-get s :agent) 'claude))
      (should (eq (plist-get s :resume) t))
      (should (eq s entry)))))

(ert-deftest gtl-select-session-mutates-in-place ()
  (gt-with-env
    (let* ((s (gtt--register "/tmp/projA/"))
           (entry (ghostel-toggle--session-by-id (gt--id s))))
      (should-not (plist-get entry :last-selected))
      (ghostel-toggle--select-session s)
      (should (plist-get entry :last-selected)))))

(ert-deftest gtl-selected-keyed-by-kind-and-root ()
  "Each (kind . root) pair holds its own selection."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (a1 (gta--register 'claude root)))
      (should-not (ghostel-toggle--selected-session 'terminal root))
      (ghostel-toggle--select-session t1)
      (ghostel-toggle--select-session a1)
      (should (equal (gt--id (ghostel-toggle--selected-session 'terminal root))
                     (gt--id t1)))
      (should (equal (gt--id (ghostel-toggle--selected-session 'agent root))
                     (gt--id a1))))))

(ert-deftest gtl-selected-and-default-session ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root))
           (s2 (gta--register 'codex root)))
      ;; Nothing selected, not on a session buffer → default is the last one.
      (should (equal (gt--id (ghostel-toggle--default-session 'agent root))
                     (gt--id s2)))
      (ghostel-toggle--select-session s1)
      (should (equal (gt--id (ghostel-toggle--selected-session 'agent root))
                     (gt--id s1)))
      (should (equal (gt--id (ghostel-toggle--default-session 'agent root))
                     (gt--id s1))))))

(ert-deftest gtl-cleanup-prunes-sessions-and-selected ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (_s1 (gta--register 'claude root))
           (s2 (gta--register 'codex root)))
      (ghostel-toggle--select-session s2)
      (kill-buffer (plist-get s2 :buffer))
      (should (= (length (ghostel-toggle--sessions-for-root 'agent root)) 1))
      (should (null (ghostel-toggle--selected-session 'agent root))))))

(ert-deftest gtl-previous-session-orders-and-wraps ()
  "Previous in order, wrapping the first session to the last."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gtt--register root))
           (s2 (gtt--register root))
           (s3 (gtt--register root)))
      (should (equal (gt--id (ghostel-toggle--previous-session s2)) (gt--id s1)))
      (should (equal (gt--id (ghostel-toggle--previous-session s1)) (gt--id s3))))))

(ert-deftest gtl-sessions-for-root-filters-kind ()
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gtt--register root)
      (gta--register 'claude root)
      (gta--register 'claude "/tmp/projB/")
      (should (= (length (ghostel-toggle--sessions-for-root 'terminal root)) 1))
      (should (= (length (ghostel-toggle--sessions-for-root 'agent root)) 1)))))

(ert-deftest gtl-current-session-requires-matching-kind ()
  (gt-with-env
    (let ((s (gtt--register "/tmp/projA/")))
      (with-current-buffer (plist-get s :buffer)
        (should (eq (ghostel-toggle--current-session 'terminal) s))
        (should-not (ghostel-toggle--current-session 'agent))
        (should (eq (ghostel-toggle--current-session) s))))))

(ert-deftest gtl-on-select-hook-called ()
  (gt-with-env
    (let (seen)
      (ghostel-toggle-define-kind 'gt-scratch
                                  :on-select (lambda (s) (push s seen)))
      (let ((s (ghostel-toggle--register-session
                'gt-scratch "/tmp/projA/" (gt--fake-buffer))))
        (ghostel-toggle--select-session s)
        (should (memq s seen))))))

(ert-deftest gtl-label-default-counts-per-kind ()
  "The library's fallback label is the capitalized kind, counted per kind."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (ghostel-toggle--register-session 'terminal root (gt--fake-buffer)))
           (s2 (ghostel-toggle--register-session 'terminal root (gt--fake-buffer))))
      (should (equal (plist-get s1 :label) "Terminal"))
      (should (equal (plist-get s2 :label) "Terminal 2")))))

;;; ============================================================================
;;; B. gtl-win- — library: panel (side window) behavior
;;; ============================================================================

(ert-deftest gtl-win-panel-window-p-uses-kind-param ()
  (gt-with-env
    (let ((s (gtt--register "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((win (ghostel-toggle--show-panel 'terminal (plist-get s :buffer))))
        (should (eq (window-parameter win 'ghostel-toggle-kind) 'terminal))
        (should (ghostel-toggle--panel-window-p win 'terminal))
        (should-not (ghostel-toggle--panel-window-p win 'agent))
        (should (eq (ghostel-toggle--panel-window 'terminal) win))
        (should-not (ghostel-toggle--panel-window 'agent))))))

(ert-deftest gtl-win-display-panel-stamps-param-both-branches ()
  "Both the fresh-side-window branch and the reuse branch stamp the param."
  (gt-with-env
    (let ((s1 (gtt--register "/tmp/projA/"))
          (s2 (gtt--register "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((win (ghostel-toggle--display-panel-window
                  'terminal (plist-get s1 :buffer))))
        (should (eq (window-parameter win 'ghostel-toggle-kind) 'terminal))
        ;; reuse branch
        (let ((again (ghostel-toggle--display-panel-window
                      'terminal (plist-get s2 :buffer))))
          (should (eq again win))
          (should (eq (window-buffer win) (plist-get s2 :buffer)))
          (should (eq (window-parameter win 'ghostel-toggle-kind) 'terminal)))))))

(ert-deftest gtl-win-two-panels-coexist ()
  "The drawer (bottom) and the sidebar (right) live in one frame without
either predicate matching the other's window."
  (gt-with-env
    (let ((ts (gtt--register "/tmp/projA/"))
          (as (gta--register 'claude "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((tw (ghostel-toggle--show-panel 'terminal (plist-get ts :buffer)))
            (aw (ghostel-toggle--show-panel 'agent (plist-get as :buffer))))
        (should-not (eq tw aw))
        (should (eq (window-parameter tw 'window-side) 'bottom))
        (should (eq (window-parameter aw 'window-side) 'right))
        (should (eq (ghostel-toggle--panel-window 'terminal) tw))
        (should (eq (ghostel-toggle--panel-window 'agent) aw))
        (should (= (gt--win-count) 3))))))

(ert-deftest gtl-win-finish-preserves-correct-dimension ()
  "Bottom panels preserve height, side panels preserve width."
  (gt-with-env
    (let ((ts (gtt--register "/tmp/projA/"))
          (as (gta--register 'claude "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((tw (ghostel-toggle--show-panel 'terminal (plist-get ts :buffer)))
            (aw (ghostel-toggle--show-panel 'agent (plist-get as :buffer))))
        (should (window-preserved-size tw nil))     ; height preserved
        (should-not (window-preserved-size tw t))
        (should (window-preserved-size aw t))       ; width preserved
        (should-not (window-preserved-size aw nil))))))

(ert-deftest gtl-win-show-session-in-window-dedicate-guard ()
  "Panel windows get re-dedicated; ordinary (e.g. full-frame) windows must
not — dedicating a sole window would wedge the frame."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gtt--register root))
           (s2 (gtt--register root)))
      (gt--show-code root)
      ;; panel polarity
      (let ((win (ghostel-toggle--show-panel 'terminal (plist-get s1 :buffer))))
        (ghostel-toggle--show-session-in-window s2 win)
        (should (eq (window-buffer win) (plist-get s2 :buffer)))
        (should (window-dedicated-p win)))
      ;; ordinary-window polarity
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer (plist-get s1 :buffer))
      (ghostel-toggle--show-session-in-window s2 (selected-window))
      (should (eq (window-buffer (selected-window)) (plist-get s2 :buffer)))
      (should-not (window-dedicated-p (selected-window))))))

(ert-deftest gtl-win-hide-panel-restores-last-window ()
  (gt-with-env
    (let ((s (gtt--register "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((code-win (selected-window)))
        (ghostel-toggle--remember-last-window 'terminal)
        (select-window (ghostel-toggle--show-panel 'terminal (plist-get s :buffer)))
        (ghostel-toggle-hide-panel 'terminal)
        (should-not (ghostel-toggle--panel-window 'terminal))
        (should (eq (selected-window) code-win))))))

(ert-deftest gtl-win-root-visible-p-per-kind ()
  (gt-with-env
    (let ((s (gtt--register "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (ghostel-toggle--show-panel 'terminal (plist-get s :buffer))
      (should (ghostel-toggle--root-visible-p 'terminal "/tmp/projA/"))
      (should-not (ghostel-toggle--root-visible-p 'terminal "/tmp/projB/"))
      (should-not (ghostel-toggle--root-visible-p 'agent "/tmp/projA/")))))

(ert-deftest gtl-win-remember-last-window-skips-own-kind-panel ()
  "Remembering from the kind's own panel is a no-op; another kind's panel
window is a legitimate return target (hiding the agent goes back to the
drawer you came from)."
  (gt-with-env
    (let ((s (gtt--register "/tmp/projA/")))
      (gt--show-code "/tmp/projA/")
      (let ((code-win (selected-window)))
        (ghostel-toggle--remember-last-window 'terminal)
        (select-window (ghostel-toggle--show-panel 'terminal (plist-get s :buffer)))
        (let ((panel-win (selected-window)))
          (ghostel-toggle--remember-last-window 'terminal)   ; own kind → skip
          (should (eq (ghostel-toggle--last-window 'terminal) code-win))
          (ghostel-toggle--remember-last-window 'agent)      ; other kind → record
          (should (eq (ghostel-toggle--last-window 'agent) panel-win)))))))

(ert-deftest gtl-win-create-session-returns-window-and-runs-setup ()
  (gt-with-env
    (gt--show-code "/tmp/projA/")
    (let* (setup-buf
           (win (ghostel-toggle-create-session
                 'terminal "/tmp/projA/"
                 :setup (lambda (b) (setq setup-buf b)))))
      (should (window-live-p win))
      (should (ghostel-toggle--panel-window-p win 'terminal))
      (should (buffer-live-p setup-buf))
      (should (eq setup-buf
                  (plist-get (ghostel-toggle--selected-session
                              'terminal "/tmp/projA/")
                             :buffer))))))

;;; ============================================================================
;;; C. gtl-fs- — library: fullscreen state machine + s-<return> command
;;; ============================================================================

(ert-deftest gtl-fs-sticky-lifecycle ()
  "enter → find → hide (sticky, view survives) → re-show → exit (removed)."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf)
      (let ((view (ghostel-toggle--view-for-root 'terminal root)))
        (should view)
        (should (eq (plist-get view :buffer) buf))
        (should (null (plist-get view :hidden)))
        (ghostel-toggle--hide-fullscreen view)
        (should (ghostel-toggle--view-for-root 'terminal root))
        (should (plist-get view :hidden))
        (ghostel-toggle--show-fullscreen view)
        (should (null (plist-get view :hidden)))
        (ghostel-toggle--exit-fullscreen view)
        (should (null (ghostel-toggle--view-for-root 'terminal root)))))))

(ert-deftest gtl-fs-current-view-requires-shown-buffer ()
  "Display routing must not mistake a hidden view or a code buffer for the
fullscreen window."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf)
      (with-current-buffer buf
        (should (ghostel-toggle--current-fullscreen-view 'terminal))
        (plist-put (ghostel-toggle--view-for-root 'terminal root) :hidden t)
        (should (null (ghostel-toggle--current-fullscreen-view 'terminal))))
      (with-current-buffer (gt--code-buffer root)
        (should (null (ghostel-toggle--current-fullscreen-view 'terminal)))))))

(ert-deftest gtl-fs-current-view-filters-kind ()
  "A shown terminal fullscreen is never the agent's current fullscreen."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf)
      (should (ghostel-toggle--current-fullscreen-view 'terminal))
      (should-not (ghostel-toggle--current-fullscreen-view 'agent)))))

(ert-deftest gtl-fs-view-for-root-filters-kind ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen (plist-get s :buffer))
      (should (ghostel-toggle--view-for-root 'terminal root))
      (should-not (ghostel-toggle--view-for-root 'agent root)))))

(ert-deftest gtl-fs-pruned-when-buffer-killed ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf)
      (should (ghostel-toggle--view-for-root 'terminal root))
      (kill-buffer buf)
      (should (null (ghostel-toggle--view-for-root 'terminal root))))))

(ert-deftest gtl-fs-enter-from-fullscreen-keeps-escapable-config ()
  "Re-entering fullscreen while the session already fills the frame must not
save a self-referential config, even when the sole window is dedicated, or
demoting/hiding could never reveal the code."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)              ; window's prev-buffer becomes the code
      (switch-to-buffer buf)
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) t)
      (should (gt--fullscreen-now-p buf))
      (ghostel-toggle--enter-fullscreen buf)
      (should-not (window-dedicated-p (selected-window)))
      (let* ((view (ghostel-toggle--view-for-root 'terminal root))
             (shown (save-window-excursion
                      (set-window-configuration (plist-get view :config))
                      (mapcar #'window-buffer (window-list nil 'no-minibuf)))))
        (should-not (memq buf shown))))))

(ert-deftest gtl-fs-display-fullscreen-strips-side-params ()
  "Showing a session full-frame must strip window-side/slot/
no-delete-other-windows and the kind param, even when the selected window
carried them (a former panel promoted into the full-frame role)."
  (gt-with-env
    (let* ((s (gtt--register "/tmp/projA/"))
           (buf (plist-get s :buffer))
           (win (selected-window)))
      (set-window-parameter win 'window-side 'bottom)
      (set-window-parameter win 'window-slot 0)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'ghostel-toggle-kind 'terminal)
      (ghostel-toggle--display-fullscreen-window buf)
      (let ((w (selected-window)))
        (should (null (window-parameter w 'window-side)))
        (should (null (window-parameter w 'window-slot)))
        (should (null (window-parameter w 'no-delete-other-windows)))
        (should (null (window-parameter w 'ghostel-toggle-kind)))))))

(ert-deftest gtl-fs-normalize-strips-kind-param ()
  "Load-bearing: a full-frame window still carrying the kind param would
answer as the panel window and get hijacked while the view is hidden."
  (gt-with-env
    (let ((win (selected-window)))
      (set-window-parameter win 'ghostel-toggle-kind 'agent)
      (ghostel-toggle--normalize-full-frame-window win)
      (should (null (window-parameter win 'ghostel-toggle-kind))))))

(ert-deftest gtl-fs-hide-dismiss-removes-view ()
  "A :dismiss (home) view is removed by hiding, not sticky-hidden."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf t)
      (let ((view (ghostel-toggle--view-for-root 'terminal root)))
        (should (plist-get view :dismiss))
        (ghostel-toggle--hide-fullscreen view)
        (should-not (ghostel-toggle--view-for-root 'terminal root))))))

(ert-deftest gtl-fs-flip-hides-and-reexpands ()
  "The plain-key flip hides a shown+focused view and re-expands otherwise;
BEFORE-SHOW runs only on the show path."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer))
           (shows 0))
      (gt--show-code root)
      (ghostel-toggle--enter-fullscreen buf)
      (let ((view (ghostel-toggle--view-for-root 'terminal root)))
        (ghostel-toggle--fullscreen-flip view (lambda () (cl-incf shows)))
        (should (plist-get view :hidden))
        (should (= shows 0))
        (should-not (get-buffer-window buf))
        (ghostel-toggle--fullscreen-flip view (lambda () (cl-incf shows)))
        (should-not (plist-get view :hidden))
        (should (= shows 1))
        (should (gt--fullscreen-now-p buf))))))

(ert-deftest gtl-fs-create-hidden-leaves-layout ()
  (gt-with-env
    (gt--show-code "/tmp/projA/")
    (let ((count (gt--win-count))
          (cur (current-buffer)))
      (ghostel-toggle--create-session-hidden 'terminal "/tmp/projA/")
      (should (= (gt--win-count) count))
      (should (eq (current-buffer) cur))
      (should (= (length (ghostel-toggle--sessions-for-root
                          'terminal "/tmp/projA/"))
                 1)))))

(ert-deftest gtl-fs-home-toggle-generic-dismiss ()
  (gt-with-env
    (let* ((home (ghostel-toggle--normalize-root "~/"))
           (s (gtt--register home))
           (buf (plist-get s :buffer)))
      (gt--show-code "/tmp/projA/")
      (ghostel-toggle-home-toggle 'terminal (lambda (_) buf))
      (should (gt--fullscreen-now-p buf))
      (should (plist-get (ghostel-toggle--view-for-root 'terminal home) :dismiss))
      (ghostel-toggle-home-toggle 'terminal (lambda (_) buf))
      (should-not (ghostel-toggle--view-for-root 'terminal home))
      (should-not (get-buffer-window buf)))))

(ert-deftest gtl-fs-command-enters-from-session-buffer ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (select-window (ghostel-toggle--show-panel 'terminal buf))
      (ghostel-toggle-fullscreen-command)
      (should (gt--fullscreen-now-p buf))
      (should (ghostel-toggle--view-for-root 'terminal root)))))

(ert-deftest gtl-fs-command-exits-by-kind-and-root ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gtt--register root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (select-window (ghostel-toggle--show-panel 'terminal buf))
      (ghostel-toggle-fullscreen-command)
      (ghostel-toggle-fullscreen-command)
      (should-not (ghostel-toggle--view-for-root 'terminal root))
      (should (gt--panel-shows-session-p 'terminal)))))

(ert-deftest gtl-fs-command-errors-on-code-buffer ()
  "Promotion is focus-based: from a code buffer s-<return> errors and the
layout is untouched."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gtt--register root)
      (gt--show-code root)
      (let ((count (gt--win-count)))
        (should-error (ghostel-toggle-fullscreen-command) :type 'user-error)
        (should (= (gt--win-count) count))
        (should-not ghostel-toggle--fullscreen-views)))))

(ert-deftest gtl-fs-command-errors-on-unmanaged-ghostel-buffer ()
  (gt-with-env
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer (gt--fake-buffer))          ; ghostel-mode, unregistered
    (should-error (ghostel-toggle-fullscreen-command) :type 'user-error)))

(ert-deftest gtl-fs-command-exit-skips-panel-leak-for-dismiss ()
  "s-<return> on a home (:dismiss) fullscreen restores the layout without
re-showing the home session in a project panel."
  (gt-with-env
    (let* ((home (ghostel-toggle--normalize-root "~/"))
           (s (gtt--register home))
           (buf (plist-get s :buffer)))
      (gt--show-code "/tmp/projA/")
      (ghostel-toggle-home-toggle 'terminal (lambda (_) buf))
      (should (gt--fullscreen-now-p buf))
      (ghostel-toggle-fullscreen-command)
      (should-not (ghostel-toggle--view-for-root 'terminal home))
      (should-not (get-buffer-window buf))
      (should-not (ghostel-toggle--panel-window 'terminal)))))

;;; ============================================================================
;;; D. gta- — agents instantiation
;;; ============================================================================

;;; --- pure helpers ------------------------------------------------------------

(ert-deftest gta-parse-prefix ()
  (should (equal (ghostel-agent--parse-prefix nil)   '(nil nil)))
  (should (equal (ghostel-agent--parse-prefix '(4))  '(claude t)))
  (should (equal (ghostel-agent--parse-prefix 2)     '(codex nil)))
  (should (equal (ghostel-agent--parse-prefix 3)     '(codex t)))
  (should-error (ghostel-agent--parse-prefix 99)))

(ert-deftest gta-command-line ()
  (let ((claude (ghostel-agent--profile 'claude))
        (codex  (ghostel-agent--profile 'codex)))
    (should (equal (ghostel-agent--command-line claude nil) "claude"))
    (should (equal (ghostel-agent--command-line claude t)   "claude --resume"))
    (should (equal (ghostel-agent--command-line codex  t)   "codex resume"))))

;;; --- session registry & labels ------------------------------------------------

(ert-deftest gta-labels-increment-per-agent ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root))
           (s2 (gta--register 'claude root))
           (c1 (gta--register 'codex  root)))
      (should (equal (plist-get s1 :label) "Claude"))
      (should (equal (plist-get s2 :label) "Claude 2"))
      (should (equal (plist-get c1 :label) "Codex")))))

(ert-deftest gta-last-session-is-per-agent ()
  "`C-2 s-l' returns to the codex you last used, independent of the
project-wide selection set by a later claude/codex pick."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (c1 (gta--register 'claude root))
           (_c2 (gta--register 'claude root))
           (x1 (gta--register 'codex  root)))
      (ghostel-toggle--select-session c1)   ; claude's last → c1
      (ghostel-toggle--select-session x1)   ; codex's last → x1 (and project selection)
      (should (equal (gt--id (ghostel-agent--last-session-for-agent 'claude root))
                     (gt--id c1)))
      (should (equal (gt--id (ghostel-agent--last-session-for-agent 'codex root))
                     (gt--id x1))))))

(ert-deftest gta-cleanup-on-kill-prunes-everything ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (_s1 (gta--register 'claude root))
           (s2  (gta--register 'codex  root)))
      (ghostel-toggle--select-session s2)
      (kill-buffer (plist-get s2 :buffer))
      (should (= (length (ghostel-toggle--sessions-for-root 'agent root)) 1))
      (should (null (ghostel-toggle--selected-session 'agent root)))
      (should (null (ghostel-agent--last-session-for-agent 'codex root))))))

;;; --- A. fullscreen / split geometry -------------------------------------------

(ert-deftest gta-win-split-roundtrip-preserves-split ()
  "vsplit → s-l → s-<return> → s-l keeps the 2-pane split, hides the agent,
and stays in (sticky) fullscreen mode."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (split-window-right)                          ; 2 code panes
      (ghostel-agent-toggle-command nil)            ; s-l → sidebar (3 windows)
      (should (= (gt--win-count) 3))
      (ghostel-toggle-fullscreen-command)           ; s-<return> → fullscreen
      (should (= (gt--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; s-l → hide (collapse to code)
      (should (= (gt--win-count) 2))
      (should (null (gt--panel-shows-session-p 'agent)))
      (let ((view (ghostel-toggle--view-for-root 'agent root)))
        (should view)                               ; sticky: still in fullscreen mode
        (should (plist-get view :hidden))))))

(ert-deftest gta-win-reexpand-from-split-is-fullscreen ()
  "s-l → s-<return> → s-l → vsplit → s-l re-expands the agent to fill the
frame despite the split."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gta--register 'claude root))
           (buf (plist-get s :buffer)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (ghostel-toggle-fullscreen-command)           ; fullscreen
      (ghostel-agent-toggle-command nil)            ; hide → just code
      (split-window-right)                          ; 2 code panes
      (should (> (gt--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; s-l → re-expand
      (should (gt--fullscreen-now-p buf))
      (should (null (plist-get (ghostel-toggle--view-for-root 'agent root)
                               :hidden))))))

(ert-deftest gta-win-home-then-back-to-local-fullscreen ()
  "s-l → s-<return> → C-s-l → C-s-l ends back at the local project agent,
fullscreen, with the project view intact and the home view gone."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (home (ghostel-toggle--normalize-root "~/"))
           (sa (gta--register 'claude root))
           (bufA (plist-get sa :buffer)))
      (gta--register 'claude home)                  ; pre-exists → no spawn
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; A sidebar
      (ghostel-toggle-fullscreen-command)           ; A fullscreen
      (ghostel-agent-home-toggle nil)               ; C-s-l → home fullscreen
      (should (ghostel-toggle--view-for-root 'agent home))
      (should (ghostel-toggle--view-for-root 'agent root))
      (ghostel-agent-home-toggle nil)               ; C-s-l → demote home
      (should (null (ghostel-toggle--view-for-root 'agent home)))
      (should (ghostel-toggle--view-for-root 'agent root))
      (should (gt--fullscreen-now-p bufA)))))

(ert-deftest gta-win-s-l-dismisses-home-fullscreen ()
  "A home (C-s-l) fullscreen hides with plain s-l and is dismissed (not
sticky-hidden), so C-s-l re-fires it afterwards."
  (gt-with-env
    (let* ((home (ghostel-toggle--normalize-root "~/"))
           (proj "/tmp/projA/")
           (hs (gta--register 'claude home))
           (homebuf (plist-get hs :buffer)))
      (gta--register 'claude proj)
      (gt--show-code proj)
      (ghostel-agent-home-toggle nil)               ; C-s-l → home fullscreen
      (should (gt--fullscreen-now-p homebuf))
      (ghostel-agent-toggle-command nil)            ; s-l → hide home
      (should-not (get-buffer-window homebuf))
      (should-not (ghostel-toggle--view-for-root 'agent home))
      (ghostel-agent-home-toggle nil)               ; C-s-l re-fires it
      (should (gt--fullscreen-now-p homebuf)))))

(ert-deftest gta-win-exit-fullscreen-restores-splits ()
  "s-<return> twice (enter then exit) restores the pre-fullscreen splits and
returns the agent to the sidebar (exit, unlike hide, keeps the agent shown)."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (split-window-right)
      (ghostel-agent-toggle-command nil)            ; sidebar (3 windows)
      (ghostel-toggle-fullscreen-command)           ; fullscreen
      (ghostel-toggle-fullscreen-command)           ; exit
      (should (= (gt--win-count) 3))
      (should (gt--panel-shows-session-p 'agent)))))

(ert-deftest gta-win-new-session-in-fullscreen-takes-frame ()
  "s-t while fullscreen shows the new session full-frame (not a stray
sidebar) and repoints the view at it; fullscreen stays sticky."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; show s1 sidebar
      (ghostel-toggle-fullscreen-command)           ; s1 fullscreen
      (ghostel-agent-new-session-command nil)       ; s-t → new session s2
      (let ((new (current-buffer))
            (view (ghostel-toggle--view-for-root 'agent root)))
        (should (gt--fullscreen-now-p new))
        (should (not (eq new (plist-get s1 :buffer))))
        (should (eq (plist-get view :buffer) new))
        (should (= (length (ghostel-toggle--sessions-for-root 'agent root)) 2))))))

(ert-deftest gta-win-cycle-in-fullscreen-stays-fullscreen ()
  "s-<right> while fullscreen switches the shown session but stays full-frame."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root))
           (s2 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-toggle--select-session s2)
      (ghostel-agent-toggle-command nil)            ; show s2 sidebar
      (ghostel-toggle-fullscreen-command)           ; s2 fullscreen
      (ghostel-agent-next-session)                  ; s-<right> → s1
      (let ((view (ghostel-toggle--view-for-root 'agent root)))
        (should (gt--fullscreen-now-p (plist-get s1 :buffer)))
        (should (eq (plist-get view :buffer) (plist-get s1 :buffer)))))))

(ert-deftest gta-win-enter-fullscreen-strips-side-params ()
  "Promoting the sidebar (a side window) to fullscreen strips its side and
kind parameters so the full-frame window behaves like an ordinary window."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; focus is the side window
      (ghostel-toggle-fullscreen-command)
      (let ((win (selected-window)))
        (should (null (window-parameter win 'window-side)))
        (should (null (window-parameter win 'window-slot)))
        (should (null (window-parameter win 'no-delete-other-windows)))
        (should (null (window-parameter win 'ghostel-toggle-kind)))))))

(ert-deftest gta-win-show-fullscreen-resnapshots-after-split ()
  "Hiding then splitting then re-showing re-snapshots the layout, so the next
hide returns to the split that existed at re-show time, not the original."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; sidebar
      (ghostel-toggle-fullscreen-command)           ; fullscreen
      (ghostel-agent-toggle-command nil)            ; hide → 1 code window
      (split-window-right)                          ; 2 code windows
      (ghostel-agent-toggle-command nil)            ; re-show (re-snapshots split)
      (should (= (gt--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; hide again
      (should (= (gt--win-count) 2)))))             ; back to the 2-pane split

;;; --- B. sidebar 5-state toggle + region send -----------------------------------

(ert-deftest gta-win-toggle-not-visible-shows-and-focuses ()
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)
      (should (gt--panel-shows-session-p 'agent))
      (should (ghostel-toggle--panel-window-p (selected-window) 'agent)))))

(ert-deftest gta-win-toggle-focused-hides ()
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; show, focus sidebar
      (ghostel-agent-toggle-command nil)            ; focused → hide
      (should (null (ghostel-toggle--panel-window 'agent)))
      (should (= (gt--win-count) 1)))))

(ert-deftest gta-win-toggle-unfocused-focuses-not-hides ()
  (gt-with-env
    (let ((root "/tmp/projA/")
          (code-win (selected-window)))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (select-window code-win)                      ; focus the code window
      (let ((n (gt--win-count)))
        (ghostel-agent-toggle-command nil)          ; unfocused → focus sidebar
        (should (ghostel-toggle--panel-window-p (selected-window) 'agent))
        (should (= (gt--win-count) n))))))

(ert-deftest gta-win-toggle-hidden-with-region-sends-and-shows ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (code (gt--show-code root "foo.js")))
      (gta--register 'claude root)
      (with-current-buffer code (insert "alpha\nbeta\n"))
      (gt--select-region code (point-min) (point-max))
      (let ((transient-mark-mode t))
        (ghostel-agent-toggle-command nil))         ; hidden + region → send & show
      (should gt--last-paste)
      (should (string-match-p "alpha" gt--last-paste))
      (should (gt--panel-shows-session-p 'agent)))))

(ert-deftest gta-win-toggle-visible-with-region-sends-and-focuses ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (code (gt--show-code root "foo.js"))
           (code-win (selected-window)))
      (gta--register 'claude root)
      (with-current-buffer code (insert "gamma\ndelta\n"))
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (select-window code-win)
      (gt--select-region code (point-min) (point-max))
      (let ((n (gt--win-count))
            (transient-mark-mode t))
        (ghostel-agent-toggle-command nil)          ; visible + region → send & focus
        (should (string-match-p "gamma" gt--last-paste))
        (should (ghostel-toggle--panel-window-p (selected-window) 'agent))
        (should (= (gt--win-count) n))))))

(ert-deftest gta-win-send-region-formats-with-file-context ()
  "`--send-region' prefixes the project-relative path and line range, then
fences the text."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s (gta--register 'claude root))
           (code (gt--show-code root "foo.js")))
      (with-current-buffer code
        (insert "line1\nline2\nline3\n")
        (set-mark (point-min))
        (goto-char (point-min))
        (forward-line 2)                            ; region = lines 1..3 start
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda (&rest _) "/tmp/projA/")))
          (ghostel-agent--send-region (plist-get s :buffer))))
      (should (string-prefix-p "foo.js:1-3\n```\n" gt--last-paste))
      (should (string-match-p "line1" gt--last-paste)))))

;;; --- C. multi-project independence ---------------------------------------------

(ert-deftest gta-win-two-projects-independent-fullscreen ()
  "Fullscreen views are per (kind, root); promotion is focus-based, so each
project's panel is focused before s-<return>."
  (gt-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (sa (gta--register 'claude rootA))
           (sb (gta--register 'claude rootB)))
      (gt--show-code rootA)
      (ghostel-agent-toggle-command nil)            ; A sidebar (focused)
      (ghostel-toggle-fullscreen-command)           ; A fullscreen
      (switch-to-buffer (gt--code-buffer rootB))    ; move to project B code
      (ghostel-agent-toggle-command nil)            ; B sidebar (focused)
      (ghostel-toggle-fullscreen-command)           ; B fullscreen
      (should (ghostel-toggle--view-for-root 'agent rootA))
      (should (ghostel-toggle--view-for-root 'agent rootB))
      (should (= (length ghostel-toggle--fullscreen-views) 2))
      ;; exiting B (current buffer is B's agent) leaves A's view intact
      (should (eq (current-buffer) (plist-get sb :buffer)))
      (ghostel-toggle-fullscreen-command)
      (should (null (ghostel-toggle--view-for-root 'agent rootB)))
      (should (ghostel-toggle--view-for-root 'agent rootA))
      (ignore sa))))

(ert-deftest gta-win-selection-persists-per-root ()
  (gt-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (a1 (gta--register 'claude rootA))
           (a2 (gta--register 'claude rootA))
           (b1 (gta--register 'claude rootB))
           (b2 (gta--register 'claude rootB)))
      (ghostel-toggle--select-session a2)
      (ghostel-toggle--select-session b1)
      (should (equal (gt--id (ghostel-toggle--selected-session 'agent rootA))
                     (gt--id a2)))
      (should (equal (gt--id (ghostel-toggle--selected-session 'agent rootB))
                     (gt--id b1)))
      (ignore a1 b2))))

(ert-deftest gta-win-cycling-respects-project-boundary ()
  (gt-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (a1 (gta--register 'claude rootA))
           (a2 (gta--register 'claude rootA))
           (a-ids (list (gt--id a1) (gt--id a2))))
      (gta--register 'claude rootB)
      (gta--register 'claude rootB)
      (gta--register 'claude rootB)
      (gt--show-code rootA)
      (ghostel-toggle--select-session a1)
      (ghostel-agent-next-session)
      (should (member (gt--id (ghostel-toggle--selected-session 'agent rootA))
                      a-ids))
      (ghostel-agent-next-session)
      (should (member (gt--id (ghostel-toggle--selected-session 'agent rootA))
                      a-ids)))))

;;; --- D. cycling / after-exit ----------------------------------------------------

(ert-deftest gta-win-after-exit-shows-previous-in-same-window ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (a1 (gta--register 'claude root))
           (a2 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-toggle--select-session a2)
      (ghostel-agent-toggle-command nil)            ; show a2 in sidebar
      (ghostel-toggle--after-exit (plist-get a2 :buffer) nil)
      (let ((w (ghostel-toggle--panel-window 'agent)))
        (should w)
        (should (eq (window-buffer w) (plist-get a1 :buffer)))))))

(ert-deftest gta-win-after-exit-in-fullscreen-not-dedicated ()
  "Exit while fullscreen must not panel-dedicate the full-frame window.
`--after-exit' reuses the dead session's window for the successor;
blindly applying panel finishing would leave the frame's sole window
strongly dedicated."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root))
           (s2 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-toggle--select-session s2)
      (ghostel-agent-toggle-command nil)            ; show s2 sidebar
      (ghostel-toggle-fullscreen-command)           ; s2 fullscreen
      (let ((buf2 (plist-get s2 :buffer)))
        (ghostel-toggle--after-exit buf2 nil)       ; s2's shell dies...
        (kill-buffer buf2))                         ; ...and ghostel kills it
      (let ((win (selected-window)))
        (should (eq (window-buffer win) (plist-get s1 :buffer)))
        (should-not (window-dedicated-p win))))))

(ert-deftest gta-win-after-exit-in-fullscreen-stays-removable ()
  "Exit while fullscreen repoints the view at the successor session, so
fullscreen mode survives and plain s-l still hides the agent instead of
erroring on a sole window."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gta--register 'claude root))
           (s2 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-toggle--select-session s2)
      (ghostel-agent-toggle-command nil)            ; show s2 sidebar
      (ghostel-toggle-fullscreen-command)           ; s2 fullscreen
      (let ((buf2 (plist-get s2 :buffer)))
        (ghostel-toggle--after-exit buf2 nil)
        (kill-buffer buf2))
      ;; Killing the current buffer leaves *scratch* current in batch; the
      ;; interactive command loop would sync to the shown buffer before s-l.
      (set-buffer (window-buffer (selected-window)))
      (let ((view (ghostel-toggle--view-for-root 'agent root)))
        (should view)
        (should (eq (plist-get view :buffer) (plist-get s1 :buffer)))
        (ghostel-agent-toggle-command nil)          ; s-l → hide, not error
        (should (plist-get view :hidden))
        (should-not (get-buffer-window (plist-get s1 :buffer)))))))

(ert-deftest gta-win-cycle-skips-dead-buffer ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (a1 (gta--register 'claude root))
           (a2 (gta--register 'claude root))
           (a3 (gta--register 'claude root)))
      (gt--show-code root)
      (ghostel-toggle--select-session a1)
      (kill-buffer (plist-get a2 :buffer))          ; a2 dies
      (ghostel-agent-next-session)                  ; should skip a2 → a3
      (should (equal (gt--id (ghostel-toggle--selected-session 'agent root))
                     (gt--id a3))))))

;;; --- E. prefix dispatch through the real commands --------------------------------

(ert-deftest gta-win-c2-s-t-spawns-codex ()
  "`C-2 s-t' from a code buffer creates a Codex session (the global agent
s-t parses prefixes)."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-agent-new-session-command 2)
      (let ((session (ghostel-toggle--current-session 'agent)))
        (should session)
        (should (eq (plist-get session :agent) 'codex))
        (should (equal (plist-get session :label) "Codex"))
        (should (equal (plist-get session :root)
                       (ghostel-toggle--normalize-root root)))))))

(ert-deftest gta-win-c2-s-l-returns-to-last-codex ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (c1 (gta--register 'claude root))
           (x1 (gta--register 'codex root)))
      (ghostel-toggle--select-session x1)           ; codex's last → x1
      (ghostel-toggle--select-session c1)           ; project selection → claude
      (gt--show-code root)
      (ghostel-agent-toggle-command 2)              ; C-2 s-l
      (should (eq (current-buffer) (plist-get x1 :buffer))))))

;;; --- F. text cleaning (pure) -----------------------------------------------------

(ert-deftest gta-clean-text-console-formatting ()
  (should (equal (ghostel-agent--clean-text
                  "⏺ Foo bar\n  baz qux\n\n  Next para wraps\n  onto two lines")
                 "Foo bar baz qux\n\nNext para wraps onto two lines"))
  (should (equal (ghostel-agent--clean-text "  | line 1\n  | line 2\n  | line 3")
                 "line 1\nline 2\nline 3"))
  (should (equal (ghostel-agent--clean-text
                  "  Two reasons:\n  1. first item wraps\n  here\n  2. second")
                 "Two reasons:\n1. first item wraps here\n2. second")))

(ert-deftest gta-clean-text-preserves-blank-lines-between-paragraphs ()
  (should (equal (ghostel-agent--clean-text "para one\nwraps\n\npara two")
                 "para one wraps\n\npara two")))

(ert-deftest gta-clean-text-no-join-keeps-line-breaks ()
  (should (equal (ghostel-agent--clean-text "⏺ code line 1\n  code line 2" t)
                 "code line 1\ncode line 2"))
  (should (equal (ghostel-agent--clean-text "⏺ a\n  b" t) "a\nb")))

(ert-deftest gta-clean-text-strips-rendered-bullet ()
  "The `●' that the render advice substitutes for `⏺' is stripped too."
  (should (equal (ghostel-agent--clean-text "● Foo bar\n  baz qux")
                 "Foo bar baz qux"))
  (should (equal (ghostel-agent--clean-text "● Foo\n  bar") "Foo bar")))

(ert-deftest gta-clean-text-folds-block-bar-blockquote ()
  "Claude's `▎' blockquote bars are stripped and wrapped lines rejoined."
  (should (equal (ghostel-agent--clean-text
                  "  ▎ a quoted line the terminal wrapped\n  ▎ onto a second line")
                 "a quoted line the terminal wrapped onto a second line")))

(ert-deftest gta-clean-text-block-bar-blank-line-splits-paragraphs ()
  "A bare `▎' bar (empty quote line) stays a paragraph break, not a fold."
  (should (equal (ghostel-agent--clean-text "▎ para one\n▎\n▎ para two")
                 "para one\n\npara two")))

;;; --- quote-region (s-') -----------------------------------------------------------

(ert-deftest gta-quote-region-cleans-and-quotes ()
  "Plain `s-'': clean (strip marker, fold wraps) then blockquote + blank line."
  (gt-with-env
    (with-temp-buffer
      (insert "● Foo bar\n  baz qux")
      (ghostel-agent-quote-region (point-min) (point-max)))
    (should (equal gt--last-paste "> Foo bar baz qux\n\n"))))

(ert-deftest gta-quote-region-raw-keeps-verbatim ()
  "`C-u s-'': quote verbatim — marker kept, lines not folded."
  (gt-with-env
    (with-temp-buffer
      (insert "● Foo bar\n  baz qux")
      (ghostel-agent-quote-region (point-min) (point-max) t))
    (should (equal gt--last-paste "> ● Foo bar\n>   baz qux\n\n"))))

(ert-deftest gta-quote-region-blank-lines-become-bare-gt ()
  "Blank lines inside the quote become `>' so it stays one blockquote."
  (gt-with-env
    (with-temp-buffer
      (insert "para one\n\npara two")
      (ghostel-agent-quote-region (point-min) (point-max)))
    (should (equal gt--last-paste "> para one\n>\n> para two\n\n"))))

(ert-deftest gta-session-mode-map-bindings ()
  "The agent session keymap wires s-c and s-'; s-t stays global (prefix-aware)."
  (should (eq (lookup-key ghostel-agent-session-mode-map (kbd "s-c"))
              'ghostel-agent-copy-clean))
  (should (eq (lookup-key ghostel-agent-session-mode-map (kbd "s-'"))
              'ghostel-agent-quote-region))
  (should-not (lookup-key ghostel-agent-session-mode-map (kbd "s-t"))))

;;; ============================================================================
;;; E. gtt- — terminals instantiation
;;; ============================================================================

(ert-deftest gtt-label-gap-reuse ()
  "Killing \"Terminal\" frees its number for the next session."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (t2 (gtt--register root)))
      (should (equal (plist-get t1 :label) "Terminal"))
      (should (equal (plist-get t2 :label) "Terminal 2"))
      (kill-buffer (plist-get t1 :buffer))
      (should (equal (ghostel-terminal--next-label root) "Terminal")))))

(ert-deftest gtt-win-toggle-creates-in-bottom-drawer ()
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-terminal-toggle)
      (let ((win (ghostel-toggle--panel-window 'terminal)))
        (should win)
        (should (eq (window-parameter win 'window-side) 'bottom))
        (should (eq (selected-window) win))
        (should (= (length (ghostel-toggle--sessions-for-root 'terminal root))
                   1))))))

(ert-deftest gtt-win-toggle-visible-hides-even-unfocused ()
  "Unlike the agent 5-state toggle, s-i hides the drawer whenever it shows
this project — no focus dance."
  (gt-with-env
    (let ((root "/tmp/projA/")
          (code-win (selected-window)))
      (gtt--register root)
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; show drawer (focused)
      (select-window code-win)                      ; focus the code window
      (ghostel-terminal-toggle)                     ; visible → hide, even unfocused
      (should-not (ghostel-toggle--panel-window 'terminal))
      (should (= (gt--win-count) 1)))))

(ert-deftest gtt-win-toggle-prefix-creates-new ()
  "C-u s-i creates a new terminal tab."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gtt--register root)
      (gt--show-code root)
      (let ((current-prefix-arg '(4)))
        (ghostel-terminal-toggle))
      (should (= (length (ghostel-toggle--sessions-for-root 'terminal root))
                 2)))))

(ert-deftest gtt-win-toggle-shows-selected-session ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (_t2 (gtt--register root)))
      (gt--show-code root)
      (ghostel-toggle--select-session t1)
      (ghostel-terminal-toggle)
      (should (eq (current-buffer) (plist-get t1 :buffer))))))

(ert-deftest gtt-win-cycle-within-kind ()
  "s-<left>/s-<right> cycle among this project's terminals only, agent
sessions in the same root notwithstanding."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (t2 (gtt--register root))
           (t-ids (list (gt--id t1) (gt--id t2))))
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-toggle--select-session t1)
      (dotimes (_ 3)
        (ghostel-terminal-next-session)
        (should (member (gt--id (ghostel-toggle--selected-session 'terminal root))
                        t-ids))))))

(ert-deftest gtt-s-t-in-drawer-spawns-terminal-ignoring-prefix ()
  "In drawer buffers the minor-mode map shadows the global agent s-t; the
terminal command ignores prefix args entirely — C-2/C-u still spawn plain
terminals, never agents."
  (should (eq (lookup-key ghostel-terminal-session-mode-map (kbd "s-t"))
              'ghostel-terminal-new-session))
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer focused
      (let ((current-prefix-arg '(4)))              ; would mean claude-resume to s-t
        (ghostel-terminal-new-session))
      (should (= (length (ghostel-toggle--sessions-for-root 'terminal root)) 2))
      (should (= (length (ghostel-toggle--sessions-for-root 'agent root)) 0)))))

(ert-deftest gtt-win-fullscreen-via-s-return ()
  "s-<return> in the drawer promotes the terminal to fullscreen; again
demotes it back to the drawer."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer focused
      (let ((buf (current-buffer)))
        (ghostel-toggle-fullscreen-command)         ; promote
        (should (gt--fullscreen-now-p buf))
        (should (ghostel-toggle--view-for-root 'terminal root))
        (ghostel-toggle-fullscreen-command)         ; demote
        (should-not (ghostel-toggle--view-for-root 'terminal root))
        (should (gt--panel-shows-session-p 'terminal))))))

(ert-deftest gtt-win-fullscreen-flip-via-s-i ()
  "While the project terminal is in sticky fullscreen, plain s-i flips its
visibility: hide to the code, re-expand from the code."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer focused
      (let ((buf (current-buffer)))
        (ghostel-toggle-fullscreen-command)         ; promote
        (ghostel-terminal-toggle)                   ; s-i → hide
        (let ((view (ghostel-toggle--view-for-root 'terminal root)))
          (should view)
          (should (plist-get view :hidden))
          (should-not (get-buffer-window buf)))
        (ghostel-terminal-toggle)                   ; s-i → re-expand
        (should (gt--fullscreen-now-p buf))
        (should-not (plist-get (ghostel-toggle--view-for-root 'terminal root)
                               :hidden))))))

(ert-deftest gtt-win-home-toggle-c-s-i ()
  "C-s-i promotes a home-rooted terminal fullscreen; s-i dismisses it and
C-s-i re-fires it; C-s-i demotes back to the previous layout."
  (gt-with-env
    (let* ((home (ghostel-toggle--normalize-root "~/"))
           (hs (gtt--register home))
           (homebuf (plist-get hs :buffer)))
      (gt--show-code "/tmp/projA/")
      (ghostel-terminal-home-toggle)                ; C-s-i → home fullscreen
      (should (gt--fullscreen-now-p homebuf))
      (should (plist-get (ghostel-toggle--view-for-root 'terminal home)
                         :dismiss))
      (ghostel-terminal-toggle)                     ; s-i → dismiss
      (should-not (get-buffer-window homebuf))
      (should-not (ghostel-toggle--view-for-root 'terminal home))
      (ghostel-terminal-home-toggle)                ; C-s-i re-fires it
      (should (gt--fullscreen-now-p homebuf))
      (ghostel-terminal-home-toggle)                ; C-s-i demotes
      (should-not (ghostel-toggle--view-for-root 'terminal home))
      (should-not (get-buffer-window homebuf)))))

(ert-deftest gtt-win-home-toggle-spawns-when-missing ()
  "C-s-i with no home terminal creates one (hidden creation, then promote)."
  (gt-with-env
    (let ((home (ghostel-toggle--normalize-root "~/")))
      (gt--show-code "/tmp/projA/")
      (ghostel-terminal-home-toggle)
      (let ((session (car (ghostel-toggle--sessions-for-root 'terminal home))))
        (should session)
        (should (gt--fullscreen-now-p (plist-get session :buffer)))))))

(ert-deftest gtt-win-after-exit-shows-previous-terminal ()
  "A dying terminal's drawer window moves to the previous tab and stays a
dedicated drawer."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (t2 (gtt--register root)))
      (gt--show-code root)
      (ghostel-toggle--select-session t2)
      (ghostel-terminal-toggle)                     ; show t2 in drawer
      (ghostel-toggle--after-exit (plist-get t2 :buffer) nil)
      (let ((w (ghostel-toggle--panel-window 'terminal)))
        (should w)
        (should (eq (window-buffer w) (plist-get t1 :buffer)))
        (should (window-dedicated-p w))))))

(ert-deftest gtt-session-mode-map-bindings ()
  "The terminal keymap wires cycling and s-t; the agent-only helpers are
absent."
  (should (eq (lookup-key ghostel-terminal-session-mode-map (kbd "s-<left>"))
              'ghostel-terminal-previous-session))
  (should (eq (lookup-key ghostel-terminal-session-mode-map (kbd "s-<right>"))
              'ghostel-terminal-next-session))
  (should (eq (lookup-key ghostel-terminal-session-mode-map (kbd "s-t"))
              'ghostel-terminal-new-session))
  (should-not (lookup-key ghostel-terminal-session-mode-map (kbd "s-c")))
  (should-not (lookup-key ghostel-terminal-session-mode-map (kbd "s-'"))))

;;; ============================================================================
;;; F. gtx- — cross-kind isolation
;;; ============================================================================

(ert-deftest gtx-panels-coexist-and-toggle-independently ()
  "s-i and s-l drive disjoint panels: hiding one never touches the other."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gtt--register root)
      (gta--register 'claude root)
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer (focused)
      (ghostel-agent-toggle-command nil)            ; sidebar from the drawer
      (should (= (gt--win-count) 3))
      (should (gt--panel-shows-session-p 'terminal))
      (should (gt--panel-shows-session-p 'agent))
      (ghostel-agent-toggle-command nil)            ; focused agent → hide agent
      (should-not (ghostel-toggle--panel-window 'agent))
      (should (gt--panel-shows-session-p 'terminal))
      (ghostel-terminal-toggle)                     ; drawer visible → hide drawer
      (should-not (ghostel-toggle--panel-window 'terminal))
      (should (= (gt--win-count) 1)))))

(ert-deftest gtx-previous-session-never-crosses-kind ()
  "With one terminal and one agent in a root, neither has a previous
session — the other kind is invisible to cycling and after-exit."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (ts (gtt--register root))
           (as (gta--register 'claude root)))
      (should-not (ghostel-toggle--previous-session ts))
      (should-not (ghostel-toggle--previous-session as)))))

(ert-deftest gtx-tab-line-lists-only-own-kind ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root)))
      (gtt--register root)
      (gta--register 'claude root)
      (gta--register 'claude root)
      (with-current-buffer (plist-get t1 :buffer)
        (let ((line (ghostel-toggle--tab-line)))
          (should line)
          (should (string-match-p "Terminal" line))
          (should-not (string-match-p "Claude" line)))))))

(ert-deftest gtx-terminal-death-under-agent-fullscreen-keeps-view ()
  "An invisible terminal dying while the agent is fullscreen pops the drawer
with its successor (faithful port of today's behavior) but never touches the
agent's view."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (t1 (gtt--register root))
           (t2 (gtt--register root))
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; agent sidebar
      (ghostel-toggle-fullscreen-command)           ; agent fullscreen
      (ghostel-toggle--after-exit (plist-get t2 :buffer) nil)
      (let ((view (ghostel-toggle--view-for-root 'agent root)))
        (should view)
        (should (eq (plist-get view :buffer) abuf))
        (should-not (plist-get view :hidden)))
      (let ((w (ghostel-toggle--panel-window 'terminal)))
        (should w)
        (should (eq (window-buffer w) (plist-get t1 :buffer)))))))

(ert-deftest gtx-agent-death-repoints-only-agent-view ()
  "With both kinds holding views in one root, an agent exit repoints only
the agent view; the terminal view is untouched."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (ts (gtt--register root))
           (tbuf (plist-get ts :buffer))
           (a1 (gta--register 'claude root))
           (a2 (gta--register 'claude root)))
      (gt--show-code root)
      ;; terminal fullscreen, then hidden
      (ghostel-toggle--enter-fullscreen tbuf)
      (ghostel-toggle--hide-fullscreen
       (ghostel-toggle--view-for-root 'terminal root))
      ;; agent fullscreen on a2
      (ghostel-toggle--select-session a2)
      (ghostel-agent-toggle-command nil)
      (ghostel-toggle-fullscreen-command)
      (ghostel-toggle--after-exit (plist-get a2 :buffer) nil)
      (should (eq (plist-get (ghostel-toggle--view-for-root 'agent root) :buffer)
                  (plist-get a1 :buffer)))
      (should (eq (plist-get (ghostel-toggle--view-for-root 'terminal root) :buffer)
                  tbuf)))))

(ert-deftest gtx-fullscreen-nesting-lifo ()
  "agent fs → terminal fs on top → hide terminal → agent fs is back →
exit agent → the original layout."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; sidebar
      (ghostel-toggle-fullscreen-command)           ; agent fullscreen
      (ghostel-terminal-toggle)                     ; drawer over the fullscreen
      (ghostel-toggle-fullscreen-command)           ; terminal fullscreen on top
      (let ((va (ghostel-toggle--view-for-root 'agent root))
            (vt (ghostel-toggle--view-for-root 'terminal root)))
        (should (and va vt))
        (ghostel-terminal-toggle)                   ; s-i → hide terminal fs
        (should (plist-get vt :hidden))
        (should-not (plist-get va :hidden))
        (should (get-buffer-window abuf))           ; agent fullscreen is back
        (should-not (get-buffer-window (plist-get vt :buffer)))
        ;; demote the agent from its full-frame window
        (select-window (get-buffer-window abuf))
        (ghostel-toggle-fullscreen-command)
        (should-not (ghostel-toggle--view-for-root 'agent root))
        (should (ghostel-toggle--view-for-root 'terminal root))  ; still hidden
        (should (gt--panel-shows-session-p 'agent))))))

(ert-deftest gtx-hide-agent-then-hide-terminal-no-resurrection ()
  "Out-of-order hiding: a restored snapshot may contain another view's
full-frame buffer; the hide sweep must keep hidden views hidden."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)            ; sidebar
      (ghostel-toggle-fullscreen-command)           ; agent fs
      (ghostel-terminal-toggle)                     ; drawer over it
      (ghostel-toggle-fullscreen-command)           ; terminal fs (snapshot: agent fs + drawer)
      (let ((va (ghostel-toggle--view-for-root 'agent root))
            (vt (ghostel-toggle--view-for-root 'terminal root))
            (tbuf (current-buffer)))
        (ghostel-agent-toggle-command nil)          ; s-l → re-expand agent over terminal
        (should (gt--fullscreen-now-p abuf))
        (ghostel-agent-toggle-command nil)          ; s-l → hide agent (restores terminal fs)
        (should (plist-get va :hidden))
        (should (gt--fullscreen-now-p tbuf))
        (ghostel-terminal-toggle)                   ; s-i → hide terminal
        (should (plist-get vt :hidden))
        ;; neither hidden view's buffer may be visible anywhere
        (should-not (get-buffer-window abuf))
        (should-not (get-buffer-window tbuf))))))

(ert-deftest gtx-exit-terminal-fs-opens-drawer-not-agent-frame ()
  "Demoting a terminal fullscreen whose snapshot contains the agent
fullscreen shows the terminal in the drawer punched over it — demote means
show-in-panel, and the agent's frame window is never hijacked."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (ghostel-agent-toggle-command nil)
      (ghostel-toggle-fullscreen-command)           ; agent fs
      (ghostel-terminal-toggle)                     ; drawer over it
      (ghostel-toggle-fullscreen-command)           ; terminal fs on top
      (let ((tbuf (current-buffer)))
        (ghostel-toggle-fullscreen-command)         ; s-<return> → demote terminal
        (should-not (ghostel-toggle--view-for-root 'terminal root))
        (let ((w (ghostel-toggle--panel-window 'terminal)))
          (should w)
          (should (eq (window-buffer w) tbuf)))
        (should (get-buffer-window abuf))
        (should (ghostel-toggle--view-for-root 'agent root))))))

(ert-deftest gtx-s-return-in-drawer-enters-terminal-not-agent ()
  "With a hidden agent view in the root, s-<return> in the drawer promotes
the terminal and leaves the agent view untouched."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (with-current-buffer abuf
        (ghostel-toggle--enter-fullscreen abuf))
      (ghostel-toggle--hide-fullscreen
       (ghostel-toggle--view-for-root 'agent root)) ; agent view hidden
      (ghostel-terminal-toggle)                     ; drawer (creates terminal)
      (ghostel-toggle-fullscreen-command)           ; s-<return> in the drawer
      (let ((va (ghostel-toggle--view-for-root 'agent root))
            (vt (ghostel-toggle--view-for-root 'terminal root)))
        (should vt)
        (should (gt--fullscreen-now-p (plist-get vt :buffer)))
        (should va)
        (should (plist-get va :hidden))))))

(ert-deftest gtx-hidden-view-demotable-via-flip-then-s-return ()
  "A hidden view is never a dead end: the plain key re-expands it, then
s-<return> demotes it to the panel."
  (gt-with-env
    (let ((root "/tmp/projA/"))
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer (creates terminal)
      (ghostel-toggle-fullscreen-command)           ; promote
      (ghostel-terminal-toggle)                     ; hide (sticky)
      (should (plist-get (ghostel-toggle--view-for-root 'terminal root) :hidden))
      (ghostel-terminal-toggle)                     ; re-expand
      (ghostel-toggle-fullscreen-command)           ; demote
      (should-not (ghostel-toggle--view-for-root 'terminal root))
      (should (gt--panel-shows-session-p 'terminal)))))

(ert-deftest gtx-tab-line-shown-hidden-view-exits-directly ()
  "Showing a session in the panel while its view is hidden (tab-line click
path) and pressing s-<return> there exits the hidden view directly."
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root))
           (abuf (plist-get as :buffer)))
      (gt--show-code root)
      (with-current-buffer abuf
        (ghostel-toggle--enter-fullscreen abuf))
      (ghostel-toggle--hide-fullscreen
       (ghostel-toggle--view-for-root 'agent root))
      (ghostel-toggle--show-session-by-id (gt--id as))  ; tab-line click
      (should (ghostel-toggle--panel-window-p (selected-window) 'agent))
      (ghostel-toggle-fullscreen-command)
      (should-not (ghostel-toggle--view-for-root 'agent root)))))

(ert-deftest gtx-home-agent-and-home-terminal-coexist ()
  "C-s-l and C-s-i hold independent fullscreen views at ~ and unwind
independently."
  (gt-with-env
    (let* ((home (ghostel-toggle--normalize-root "~/"))
           (ha (gta--register 'claude home))
           (ht (gtt--register home))
           (habuf (plist-get ha :buffer))
           (htbuf (plist-get ht :buffer)))
      (gt--show-code "/tmp/projA/")
      (ghostel-agent-home-toggle nil)               ; home agent fullscreen
      (should (gt--fullscreen-now-p habuf))
      (ghostel-terminal-home-toggle)                ; home terminal on top
      (should (gt--fullscreen-now-p htbuf))
      (should (ghostel-toggle--view-for-root 'agent home))
      (should (ghostel-toggle--view-for-root 'terminal home))
      (ghostel-terminal-home-toggle)                ; demote home terminal
      (should-not (ghostel-toggle--view-for-root 'terminal home))
      (should (gt--fullscreen-now-p habuf))         ; home agent is back
      (ghostel-agent-home-toggle nil)               ; demote home agent
      (should-not (ghostel-toggle--view-for-root 'agent home))
      (should-not (get-buffer-window habuf))
      (should-not (get-buffer-window htbuf)))))

(ert-deftest gtx-s-l-in-terminal-buffer-targets-agent-of-same-root ()
  (gt-with-env
    (let* ((root "/tmp/projA/")
           (as (gta--register 'claude root)))
      (gtt--register root)
      (gt--show-code root)
      (ghostel-terminal-toggle)                     ; drawer focused
      (ghostel-agent-toggle-command nil)            ; s-l from the drawer
      (should (ghostel-toggle--panel-window-p (selected-window) 'agent))
      (should (eq (current-buffer) (plist-get as :buffer))))))

;;; ghostel-toggle-tests.el ends here

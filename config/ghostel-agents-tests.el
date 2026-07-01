;;; ghostel-agents-tests.el --- ERT tests for ghostel-agents -*- lexical-binding: t -*-

;; Not loaded at startup (absent from the `dotspacemacs/user-config' dolist).
;;
;; Run headless:
;;   emacs -batch -l ert \
;;     -l config/ghostel-agents.el \
;;     -l config/ghostel-agents-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Run in this Emacs: open the file, `M-x eval-buffer', then `M-x ert RET t RET'.
;;
;; These tests use fake, process-less buffers in `ghostel-mode' instead of
;; real agent shells, so they cover the registry/selection/fullscreen *logic*
;; without spawning claude/codex.  Literal window geometry (which window the
;; sidebar lands in, focus cycling) is left to interactive use; what is locked
;; in here is the state machine that decides what to show.

(require 'ert)
(require 'cl-lib)

(defvar gat--test-buffers nil
  "Buffers created by the running test, killed on cleanup.")

(defmacro gat-with-clean-registry (&rest body)
  "Run BODY with fresh, isolated ghostel-agent global state."
  (declare (indent 0) (debug t))
  `(let ((ghostel-agent--sessions nil)
         (ghostel-agent--selected-session-alist nil)
         (ghostel-agent--last-session-alist nil)
         (ghostel-agent--session-counter 0)
         (ghostel-agent--fullscreen-views nil)
         (ghostel-agent--last-window nil)
         (gat--test-buffers nil))
     (unwind-protect
         (progn ,@body)
       (dolist (buf gat--test-buffers)
         (when (buffer-live-p buf) (kill-buffer buf))))))

(defun gat--fake-agent-buffer ()
  "Return a fresh buffer that masquerades as a ghostel agent buffer.
Setting `major-mode' is enough for `derived-mode-p' — no process needed."
  (let ((buf (generate-new-buffer " *gat-agent*")))
    (with-current-buffer buf (setq major-mode 'ghostel-mode))
    (push buf gat--test-buffers)
    buf))

(defun gat--register (agent root)
  "Register a fake AGENT session in ROOT through the real code path."
  (ghostel-agent--register-session agent root (gat--fake-agent-buffer)))

(defun gat--id (session) (and session (plist-get session :id)))

;;; --- integration harness (real commands, fake/stubbed agents) ---------------
;;
;; The tests below drive the *real* interactive commands (s-l, s-t, s-<return>,
;; C-s-l, cycling) against a real (batch) window tree.  Nothing spawns claude or
;; codex: `ghostel' is stubbed to hand back a fake `ghostel-mode' buffer, and the
;; send/paste primitives are neutered.  Batch Emacs runs split-window /
;; display-buffer-in-side-window / set-window-configuration faithfully, so window
;; geometry is asserted just like in a GUI.

(defvar gat--last-paste nil
  "Last string handed to the stubbed `ghostel-paste-string'.")

(defmacro gat-with-env (&rest body)
  "Like `gat-with-clean-registry', plus a real window tree and ghostel stubs.
Runs BODY from a single clean window, restores the window configuration and
kills created buffers afterwards.  `projectile-project-root' is forced to nil
so project roots resolve deterministically via `default-directory'."
  (declare (indent 0) (debug t))
  `(let ((ghostel-agent--sessions nil)
         (ghostel-agent--selected-session-alist nil)
         (ghostel-agent--last-session-alist nil)
         (ghostel-agent--session-counter 0)
         (ghostel-agent--fullscreen-views nil)
         (ghostel-agent--last-window nil)
         (gat--test-buffers nil)
         (gat--last-paste nil)
         (window-min-width 2)
         (window-min-height 2)
         (orig (current-window-configuration)))
     (cl-letf (((symbol-function 'ghostel)
                (lambda (&rest _) (gat--fake-agent-buffer)))
               ((symbol-function 'ghostel-agent--send-command) #'ignore)
               ((symbol-function 'ghostel-send-string) #'ignore)
               ((symbol-function 'ghostel-send-key) #'ignore)
               ((symbol-function 'ghostel-paste-string)
                (lambda (s) (setq gat--last-paste s)))
               ((symbol-function 'projectile-project-root) (lambda (&rest _) nil)))
       (unwind-protect
           (progn (delete-other-windows) ,@body)
         (set-window-configuration orig)
         (dolist (buf gat--test-buffers)
           (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun gat--code-buffer (root &optional file)
  "Return a fresh non-agent buffer whose `default-directory' is ROOT.
With FILE, also set `buffer-file-name' to ROOT/FILE."
  (let ((buf (generate-new-buffer " *gat-code*"))
        (dir (ghostel-agent--normalize-root root)))
    (with-current-buffer buf
      (setq default-directory dir)
      (when file (setq buffer-file-name (expand-file-name file dir))))
    (push buf gat--test-buffers)
    buf))

(defun gat--show-code (root &optional file)
  "Show a code buffer for ROOT in the selected window; make it current."
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer (gat--code-buffer root file))
  (current-buffer))

(defun gat--win-count ()
  "Number of live non-minibuffer windows in the selected frame."
  (length (window-list nil 'no-minibuf)))

(defun gat--agent-buffer-p (buf)
  "Return non-nil when BUF is a registered agent session buffer."
  (and (buffer-live-p buf) (ghostel-agent--session-for-buffer buf) t))

(defun gat--sidebar-shows-agent-p ()
  "Return non-nil when the sidebar side window shows an agent buffer."
  (let ((w (ghostel-agent--sidebar-window)))
    (and w (gat--agent-buffer-p (window-buffer w)))))

(defun gat--fullscreen-now-p (buf)
  "Return non-nil when BUF fills the frame as the sole window."
  (and (one-window-p t) (eq (window-buffer (selected-window)) buf)))

(defun gat--select-region (buf beg end)
  "Activate a region BEG..END in BUF (transient-mark bound by the caller)."
  (with-current-buffer buf
    (set-mark beg)
    (goto-char end)
    (setq mark-active t)))

;;; --- pure helpers -----------------------------------------------------------

(ert-deftest gat-parse-prefix ()
  (should (equal (ghostel-agent--parse-prefix nil)   '(nil nil)))
  (should (equal (ghostel-agent--parse-prefix '(4))  '(claude t)))
  (should (equal (ghostel-agent--parse-prefix 2)     '(codex nil)))
  (should (equal (ghostel-agent--parse-prefix 3)     '(codex t)))
  (should-error (ghostel-agent--parse-prefix 99)))

(ert-deftest gat-command-line ()
  (let ((claude (ghostel-agent--profile 'claude))
        (codex  (ghostel-agent--profile 'codex)))
    (should (equal (ghostel-agent--command-line claude nil) "claude"))
    (should (equal (ghostel-agent--command-line claude t)   "claude --resume"))
    (should (equal (ghostel-agent--command-line codex  t)   "codex resume"))))

(ert-deftest gat-normalize-root ()
  (should (string-suffix-p "/" (ghostel-agent--normalize-root "/tmp/foo")))
  (should (equal (ghostel-agent--normalize-root "/tmp/foo")
                 (ghostel-agent--normalize-root "/tmp/foo/"))))

;;; --- session registry & labels ---------------------------------------------

(ert-deftest gat-labels-increment-per-agent ()
  (gat-with-clean-registry
    (let* ((root "/tmp/projA/")
           (s1 (gat--register 'claude root))
           (s2 (gat--register 'claude root))
           (c1 (gat--register 'codex  root)))
      (should (equal (plist-get s1 :label) "Claude"))
      (should (equal (plist-get s2 :label) "Claude 2"))
      (should (equal (plist-get c1 :label) "Codex")))))

(ert-deftest gat-selected-and-default-session ()
  (gat-with-clean-registry
    (let* ((root "/tmp/projA/")
           (s1 (gat--register 'claude root))
           (s2 (gat--register 'codex  root)))
      ;; Nothing selected, not on an agent buffer → default is the last one.
      (should (equal (gat--id (ghostel-agent--default-session root)) (gat--id s2)))
      (ghostel-agent--select-session s1)
      (should (equal (gat--id (ghostel-agent--selected-session root)) (gat--id s1)))
      (should (equal (gat--id (ghostel-agent--default-session root)) (gat--id s1))))))

(ert-deftest gat-last-session-is-per-agent ()
  "`C-2 s-l' returns to the codex you last used, independent of the
project-wide selection set by a later claude/codex pick."
  (gat-with-clean-registry
    (let* ((root "/tmp/projA/")
           (c1 (gat--register 'claude root))
           (_c2 (gat--register 'claude root))
           (x1 (gat--register 'codex  root)))
      (ghostel-agent--select-session c1)   ; claude's last → c1
      (ghostel-agent--select-session x1)   ; codex's last → x1 (and project selection)
      (should (equal (gat--id (ghostel-agent--last-session-for-agent 'claude root))
                     (gat--id c1)))
      (should (equal (gat--id (ghostel-agent--last-session-for-agent 'codex root))
                     (gat--id x1))))))

(ert-deftest gat-cleanup-on-kill-prunes-everything ()
  (gat-with-clean-registry
    (let* ((root "/tmp/projA/")
           (_s1 (gat--register 'claude root))
           (s2  (gat--register 'codex  root)))
      (ghostel-agent--select-session s2)
      (kill-buffer (plist-get s2 :buffer))
      (should (= (length (ghostel-agent--sessions-for-root root)) 1))
      (should (null (ghostel-agent--selected-session root)))
      (should (null (ghostel-agent--last-session-for-agent 'codex root))))))

(ert-deftest gat-previous-session-orders-and-wraps ()
  "Drives the `s-<left>'/after-exit fallback: previous in order, wrapping
the first session to the last."
  (gat-with-clean-registry
    (let* ((root "/tmp/projA/")
           (s1 (gat--register 'claude root))
           (s2 (gat--register 'claude root))
           (s3 (gat--register 'claude root)))
      (should (equal (gat--id (ghostel-agent--previous-session s2)) (gat--id s1)))
      (should (equal (gat--id (ghostel-agent--previous-session s1)) (gat--id s3))))))

;;; --- fullscreen state machine -----------------------------------------------

(ert-deftest gat-fullscreen-sticky-lifecycle ()
  "enter → find → hide (sticky, view survives) → re-show → exit (removed)."
  (gat-with-clean-registry
    (save-window-excursion
      (let* ((root "/tmp/projA/")
             (s1 (gat--register 'claude root))
             (buf (plist-get s1 :buffer)))
        (with-current-buffer buf (ghostel-agent--enter-fullscreen buf))
        (let ((view (ghostel-agent--fullscreen-view-for-root root)))
          (should view)
          (should (eq (plist-get view :agent) buf))
          (should (null (plist-get view :hidden)))
          ;; hide → still registered (sticky), just marked hidden
          (ghostel-agent--hide-fullscreen view)
          (should (ghostel-agent--fullscreen-view-for-root root))
          (should (plist-get view :hidden))
          ;; re-show clears hidden
          (ghostel-agent--show-fullscreen view)
          (should (null (plist-get view :hidden)))
          ;; exit removes the view
          (ghostel-agent--exit-fullscreen view)
          (should (null (ghostel-agent--fullscreen-view-for-root root))))))))

(ert-deftest gat-current-fullscreen-view-requires-shown-agent ()
  "Display routing must not mistake a hidden view or a code buffer for the
fullscreen window."
  (gat-with-clean-registry
    (save-window-excursion
      (let* ((root "/tmp/projA/")
             (s1 (gat--register 'claude root))
             (buf (plist-get s1 :buffer)))
        (with-current-buffer buf
          (ghostel-agent--enter-fullscreen buf)
          (should (ghostel-agent--current-fullscreen-view))   ; on agent, shown
          (plist-put (ghostel-agent--fullscreen-view-for-root root) :hidden t)
          (should (null (ghostel-agent--current-fullscreen-view))))  ; hidden → nil
        ;; point on a non-agent buffer → nil
        (should (null (ghostel-agent--current-fullscreen-view)))))))

(ert-deftest gat-fullscreen-pruned-when-agent-killed ()
  (gat-with-clean-registry
    (save-window-excursion
      (let* ((root "/tmp/projA/")
             (s1 (gat--register 'claude root))
             (buf (plist-get s1 :buffer)))
        (with-current-buffer buf (ghostel-agent--enter-fullscreen buf))
        (should (ghostel-agent--fullscreen-view-for-root root))
        (kill-buffer buf)
        (should (null (ghostel-agent--fullscreen-view-for-root root)))))))

(ert-deftest gat-home-and-project-fullscreen-coexist ()
  "Regression: `C-s-l' home fullscreen and project `s-<return>' fullscreen
are keyed independently and must not collide."
  (gat-with-clean-registry
    (save-window-excursion
      (let* ((rootA "/tmp/projA/")
             (home  (ghostel-agent--normalize-root "~/"))
             (sa (gat--register 'claude rootA))
             (_sh (gat--register 'claude home))   ; pre-exists → home-toggle won't spawn
             (bufA (plist-get sa :buffer)))
        ;; project A → fullscreen via the real command
        (with-current-buffer bufA (ghostel-agent-toggle-fullscreen))
        (should (ghostel-agent--fullscreen-view-for-root rootA))
        ;; home → fullscreen via the real command (reuses existing buffer)
        (ghostel-agent-home-toggle nil)
        (should (ghostel-agent--fullscreen-view-for-root home))
        (should (ghostel-agent--fullscreen-view-for-root rootA))  ; A untouched
        ;; demote home; project A stays fullscreen
        (ghostel-agent-home-toggle nil)
        (should (null (ghostel-agent--fullscreen-view-for-root home)))
        (should (ghostel-agent--fullscreen-view-for-root rootA))))))

;;; ============================================================================
;;; INTEGRATION TESTS (gat-win-*) — real commands against a real window tree
;;; ============================================================================

;;; --- A. Fullscreen / split geometry -----------------------------------------

(ert-deftest gat-win-split-roundtrip-preserves-split ()
  "User scenario #1: vsplit → s-l → s-<return> → s-l keeps the 2-pane split,
hides the agent, and stays in (sticky) fullscreen mode."
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (split-window-right)                          ; 2 code panes
      (ghostel-agent-toggle-command nil)            ; s-l → sidebar (3 windows)
      (should (= (gat--win-count) 3))
      (ghostel-agent-toggle-fullscreen)             ; s-<return> → fullscreen
      (should (= (gat--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; s-l → hide (collapse to code)
      (should (= (gat--win-count) 2))
      (should (null (gat--sidebar-shows-agent-p)))
      (let ((view (ghostel-agent--fullscreen-view-for-root root)))
        (should view)                               ; sticky: still in fullscreen mode
        (should (plist-get view :hidden))))))

(ert-deftest gat-win-reexpand-from-split-is-fullscreen ()
  "User scenario #3: s-l → s-<return> → s-l → vsplit → s-l re-expands the
agent to fill the frame despite the split."
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (s (gat--register 'claude root))
           (buf (plist-get s :buffer)))
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (ghostel-agent-toggle-fullscreen)             ; fullscreen
      (ghostel-agent-toggle-command nil)            ; hide → just code
      (split-window-right)                          ; 2 code panes
      (should (> (gat--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; s-l → re-expand
      (should (gat--fullscreen-now-p buf))
      (should (null (plist-get (ghostel-agent--fullscreen-view-for-root root)
                               :hidden))))))

(ert-deftest gat-win-home-then-back-to-local-fullscreen ()
  "User scenario #2: s-l → s-<return> → C-s-l → C-s-l ends back at the local
project agent, fullscreen, with the project view intact and the home view gone."
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (home (ghostel-agent--normalize-root "~/"))
           (sa (gat--register 'claude root))
           (bufA (plist-get sa :buffer)))
      (gat--register 'claude home)                  ; pre-exists → no spawn
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; A sidebar
      (ghostel-agent-toggle-fullscreen)             ; A fullscreen
      (ghostel-agent-home-toggle nil)               ; C-s-l → home fullscreen
      (should (ghostel-agent--fullscreen-view-for-root home))
      (should (ghostel-agent--fullscreen-view-for-root root))
      (ghostel-agent-home-toggle nil)               ; C-s-l → demote home
      (should (null (ghostel-agent--fullscreen-view-for-root home)))
      (should (ghostel-agent--fullscreen-view-for-root root))
      (should (gat--fullscreen-now-p bufA)))))

(ert-deftest gat-win-exit-fullscreen-restores-splits ()
  "s-<return> twice (enter then exit) restores the pre-fullscreen splits and
returns the agent to the sidebar (exit, unlike hide, keeps the agent shown)."
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (split-window-right)
      (ghostel-agent-toggle-command nil)            ; sidebar (3 windows)
      (ghostel-agent-toggle-fullscreen)             ; fullscreen
      (ghostel-agent-toggle-fullscreen)             ; exit
      (should (= (gat--win-count) 3))
      (should (gat--sidebar-shows-agent-p)))))

(ert-deftest gat-win-new-session-in-fullscreen-takes-frame ()
  "s-t while fullscreen shows the new session full-frame (not a stray sidebar)
and repoints the view at it; fullscreen stays sticky."
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gat--register 'claude root)))
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; show s1 sidebar
      (ghostel-agent-toggle-fullscreen)             ; s1 fullscreen
      (ghostel-agent-new-session-command nil)       ; s-t → new session s2
      (let ((new (current-buffer))
            (view (ghostel-agent--fullscreen-view-for-root root)))
        (should (gat--fullscreen-now-p new))
        (should (not (eq new (plist-get s1 :buffer))))
        (should (eq (plist-get view :agent) new))
        (should (= (length (ghostel-agent--sessions-for-root root)) 2))))))

(ert-deftest gat-win-cycle-in-fullscreen-stays-fullscreen ()
  "s-<right> while fullscreen switches the shown session but stays full-frame."
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (s1 (gat--register 'claude root))
           (s2 (gat--register 'claude root)))
      (gat--show-code root)
      (ghostel-agent--select-session s2)
      (ghostel-agent-toggle-command nil)            ; show s2 sidebar
      (ghostel-agent-toggle-fullscreen)             ; s2 fullscreen
      (ghostel-agent-next-session)                  ; s-<right> → s1
      (let ((view (ghostel-agent--fullscreen-view-for-root root)))
        (should (gat--fullscreen-now-p (plist-get s1 :buffer)))
        (should (eq (plist-get view :agent) (plist-get s1 :buffer)))))))

(ert-deftest gat-win-enter-fullscreen-strips-side-params ()
  "Promoting the sidebar (a side window) to fullscreen strips its window-side
parameters so the full-frame window behaves like an ordinary window."
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; focus is the side window
      (ghostel-agent-toggle-fullscreen)
      (let ((win (selected-window)))
        (should (null (window-parameter win 'window-side)))
        (should (null (window-parameter win 'window-slot)))
        (should (null (window-parameter win 'no-delete-other-windows)))))))

(ert-deftest gat-win-show-fullscreen-resnapshots-after-split ()
  "Hiding then splitting then re-showing re-snapshots the layout, so the next
hide returns to the split that existed at re-show time, not the original."
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; sidebar
      (ghostel-agent-toggle-fullscreen)             ; fullscreen
      (ghostel-agent-toggle-command nil)            ; hide → 1 code window
      (split-window-right)                          ; 2 code windows
      (ghostel-agent-toggle-command nil)            ; re-show (re-snapshots split)
      (should (= (gat--win-count) 1))
      (ghostel-agent-toggle-command nil)            ; hide again
      (should (= (gat--win-count) 2)))))            ; back to the 2-pane split

;;; --- B. Sidebar 5-state toggle + region send --------------------------------

(ert-deftest gat-win-toggle-not-visible-shows-and-focuses ()
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)
      (should (gat--sidebar-shows-agent-p))
      (should (ghostel-agent--sidebar-window-p (selected-window))))))

(ert-deftest gat-win-toggle-focused-hides ()
  (gat-with-env
    (let ((root "/tmp/projA/"))
      (gat--register 'claude root)
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; show, focus sidebar
      (ghostel-agent-toggle-command nil)            ; focused → hide
      (should (null (ghostel-agent--sidebar-window)))
      (should (= (gat--win-count) 1)))))

(ert-deftest gat-win-toggle-unfocused-focuses-not-hides ()
  (gat-with-env
    (let ((root "/tmp/projA/")
          (code-win (selected-window)))
      (gat--register 'claude root)
      (gat--show-code root)
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (select-window code-win)                      ; focus the code window
      (let ((n (gat--win-count)))
        (ghostel-agent-toggle-command nil)          ; unfocused → focus sidebar
        (should (ghostel-agent--sidebar-window-p (selected-window)))
        (should (= (gat--win-count) n))))))

(ert-deftest gat-win-toggle-hidden-with-region-sends-and-shows ()
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (code (gat--show-code root "foo.js")))
      (gat--register 'claude root)
      (with-current-buffer code (insert "alpha\nbeta\n"))
      (gat--select-region code (point-min) (point-max))
      (let ((transient-mark-mode t))
        (ghostel-agent-toggle-command nil))         ; hidden + region → send & show
      (should gat--last-paste)
      (should (string-match-p "alpha" gat--last-paste))
      (should (gat--sidebar-shows-agent-p)))))

(ert-deftest gat-win-toggle-visible-with-region-sends-and-focuses ()
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (code (gat--show-code root "foo.js"))
           (code-win (selected-window)))
      (gat--register 'claude root)
      (with-current-buffer code (insert "gamma\ndelta\n"))
      (ghostel-agent-toggle-command nil)            ; show sidebar
      (select-window code-win)
      (gat--select-region code (point-min) (point-max))
      (let ((n (gat--win-count))
            (transient-mark-mode t))
        (ghostel-agent-toggle-command nil)          ; visible + region → send & focus
        (should (string-match-p "gamma" gat--last-paste))
        (should (ghostel-agent--sidebar-window-p (selected-window)))
        (should (= (gat--win-count) n))))))

(ert-deftest gat-win-send-region-formats-with-file-context ()
  "`--send-region' prefixes the project-relative path and line range, then
fences the text."
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (s (gat--register 'claude root))
           (code (gat--show-code root "foo.js")))
      (with-current-buffer code
        (insert "line1\nline2\nline3\n")
        (set-mark (point-min))
        (goto-char (point-min))
        (forward-line 2)                            ; region = lines 1..3 start
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda (&rest _) "/tmp/projA/")))
          (ghostel-agent--send-region (plist-get s :buffer))))
      (should (string-prefix-p "foo.js:1-3\n```\n" gat--last-paste))
      (should (string-match-p "line1" gat--last-paste)))))

;;; --- C. Multi-project independence ------------------------------------------

(ert-deftest gat-win-two-projects-independent-fullscreen ()
  (gat-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (sa (gat--register 'claude rootA))
           (sb (gat--register 'claude rootB)))
      (gat--show-code rootA)
      (ghostel-agent-toggle-command nil)            ; A sidebar
      (ghostel-agent-toggle-fullscreen)             ; A fullscreen
      (switch-to-buffer (gat--code-buffer rootB))   ; move to project B code
      (ghostel-agent-toggle-fullscreen)             ; B fullscreen
      (should (ghostel-agent--fullscreen-view-for-root rootA))
      (should (ghostel-agent--fullscreen-view-for-root rootB))
      (should (= (length ghostel-agent--fullscreen-views) 2))
      ;; exiting B (current buffer is B's agent) leaves A's view intact
      (should (eq (current-buffer) (plist-get sb :buffer)))
      (ghostel-agent-toggle-fullscreen)
      (should (null (ghostel-agent--fullscreen-view-for-root rootB)))
      (should (ghostel-agent--fullscreen-view-for-root rootA))
      (ignore sa))))

(ert-deftest gat-win-selection-persists-per-root ()
  (gat-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (a1 (gat--register 'claude rootA))
           (a2 (gat--register 'claude rootA))
           (b1 (gat--register 'claude rootB))
           (b2 (gat--register 'claude rootB)))
      (ghostel-agent--select-session a2)
      (ghostel-agent--select-session b1)
      (should (equal (gat--id (ghostel-agent--selected-session rootA)) (gat--id a2)))
      (should (equal (gat--id (ghostel-agent--selected-session rootB)) (gat--id b1)))
      (ignore a1 b2))))

(ert-deftest gat-win-cycling-respects-project-boundary ()
  (gat-with-env
    (let* ((rootA "/tmp/projA/")
           (rootB "/tmp/projB/")
           (a1 (gat--register 'claude rootA))
           (a2 (gat--register 'claude rootA))
           (a-ids (list (gat--id a1) (gat--id a2))))
      (gat--register 'claude rootB)
      (gat--register 'claude rootB)
      (gat--register 'claude rootB)
      (gat--show-code rootA)
      (ghostel-agent--select-session a1)
      (ghostel-agent-next-session)
      (should (member (gat--id (ghostel-agent--selected-session rootA)) a-ids))
      (ghostel-agent-next-session)
      (should (member (gat--id (ghostel-agent--selected-session rootA)) a-ids)))))

;;; --- D. Cycling / after-exit ------------------------------------------------

(ert-deftest gat-win-after-exit-shows-previous-in-same-window ()
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (a1 (gat--register 'claude root))
           (a2 (gat--register 'claude root)))
      (gat--show-code root)
      (ghostel-agent--select-session a2)
      (ghostel-agent-toggle-command nil)            ; show a2 in sidebar
      (ghostel-agent--after-exit (plist-get a2 :buffer) nil)
      (let ((w (ghostel-agent--sidebar-window)))
        (should w)
        (should (eq (window-buffer w) (plist-get a1 :buffer)))))))

(ert-deftest gat-win-cycle-skips-dead-buffer ()
  (gat-with-env
    (let* ((root "/tmp/projA/")
           (a1 (gat--register 'claude root))
           (a2 (gat--register 'claude root))
           (a3 (gat--register 'claude root)))
      (gat--show-code root)
      (ghostel-agent--select-session a1)
      (kill-buffer (plist-get a2 :buffer))          ; a2 dies
      (ghostel-agent-next-session)                  ; should skip a2 → a3
      (should (equal (gat--id (ghostel-agent--selected-session root))
                     (gat--id a3))))))

;;; --- E. Text cleaning (pure) extras -----------------------------------------

(ert-deftest gat-clean-text-preserves-blank-lines-between-paragraphs ()
  (should (equal (ghostel-agent--clean-text "para one\nwraps\n\npara two")
                 "para one wraps\n\npara two")))

(ert-deftest gat-clean-text-no-join-keeps-line-breaks ()
  (should (equal (ghostel-agent--clean-text "⏺ code line 1\n  code line 2" t)
                 "code line 1\ncode line 2")))

(ert-deftest gat-clean-text-strips-rendered-bullet ()
  "The `●' that the render advice substitutes for `⏺' is stripped too."
  (should (equal (ghostel-agent--clean-text "● Foo bar\n  baz qux") "Foo bar baz qux")))

;;; --- quote-region (s-') ------------------------------------------------------

(ert-deftest gat-quote-region-cleans-and-quotes ()
  "Plain `s-'': clean (strip marker, fold wraps) then blockquote + blank line."
  (gat-with-env
    (with-temp-buffer
      (insert "● Foo bar\n  baz qux")
      (ghostel-agent-quote-region (point-min) (point-max)))
    (should (equal gat--last-paste "> Foo bar baz qux\n\n"))))

(ert-deftest gat-quote-region-raw-keeps-verbatim ()
  "`C-u s-'': quote verbatim — marker kept, lines not folded."
  (gat-with-env
    (with-temp-buffer
      (insert "● Foo bar\n  baz qux")
      (ghostel-agent-quote-region (point-min) (point-max) t))
    (should (equal gat--last-paste "> ● Foo bar\n>   baz qux\n\n"))))

(ert-deftest gat-quote-region-blank-lines-become-bare-gt ()
  "Blank lines inside the quote become `>' so it stays one blockquote."
  (gat-with-env
    (with-temp-buffer
      (insert "para one\n\npara two")
      (ghostel-agent-quote-region (point-min) (point-max)))
    (should (equal gat--last-paste "> para one\n>\n> para two\n\n"))))

(ert-deftest gat-session-mode-map-bindings ()
  "The session keymap wires s-c (copy-clean) and s-' (quote-region)."
  (should (eq (lookup-key ghostel-agent-session-mode-map (kbd "s-c"))
              'ghostel-agent-copy-clean))
  (should (eq (lookup-key ghostel-agent-session-mode-map (kbd "s-'"))
              'ghostel-agent-quote-region)))

;;; ghostel-agents-tests.el ends here

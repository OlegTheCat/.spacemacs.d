;;; ghostel-agents.el --- Ghostel coding agent sidebar on ghostel-toggle -*- lexical-binding: t -*-

;; The `agent' instantiation of the ghostel-toggle library: Claude/Codex
;; CLIs in a right sidebar (`s-l'), with tabs (`s-t'), prefix-selected
;; agents (`C-u'/`C-2'/`C-3'), per-project fullscreen (`s-<return>' while
;; focused), a home-directory fullscreen agent (`C-s-l'), region sending,
;; and console-output cleanup helpers (`s-c', `s-'').

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ghostel-toggle)

(defvar ghostel-agent-profiles
  '((claude
     :program "claude"
     :args nil
     :resume-args ("--resume"))
    (codex
     :program "codex"
     :args nil
     :resume-args ("resume")))
  "Agent profiles available through ghostel agent commands.")

(defvar ghostel-agent--last-session-alist nil
  "Alist mapping (AGENT . PROJECT-ROOT) keys to selected session ids.")

(defvar ghostel-agent-command-delay 0.3
  "Seconds to wait before sending the agent command to a new shell.")

(defvar ghostel-agent-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-<left>") #'ghostel-agent-previous-session)
    (define-key map (kbd "s-<right>") #'ghostel-agent-next-session)
    (define-key map (kbd "s-c") #'ghostel-agent-copy-clean)
    (define-key map (kbd "s-'") #'ghostel-agent-quote-region)
    map)
  "Keymap active in managed ghostel agent session buffers.")

(define-minor-mode ghostel-agent-session-mode
  "Minor mode for ghostel buffers managed by the agent sidebar."
  :init-value nil
  :lighter nil
  :keymap ghostel-agent-session-mode-map)

(defun ghostel-agent--on-select (session)
  "Record SESSION as its agent's last session for the project.
Also prunes entries whose sessions died."
  (when-let* ((agent (plist-get session :agent)))
    (setf (alist-get (cons agent (plist-get session :root))
                     ghostel-agent--last-session-alist nil nil #'equal)
          (plist-get session :id)))
  (setq ghostel-agent--last-session-alist
        (cl-remove-if-not
         (lambda (entry) (ghostel-toggle--live-session-by-id (cdr entry)))
         ghostel-agent--last-session-alist)))

(ghostel-toggle-define-kind 'agent
                            :side 'right
                            :size 0.55
                            :minor-mode 'ghostel-agent-session-mode
                            :on-select #'ghostel-agent--on-select)

(defun ghostel-agent--profile (agent)
  "Return the profile plist for AGENT."
  (or (cdr (assq agent ghostel-agent-profiles))
      (user-error "Unknown ghostel agent: %S" agent)))

(defun ghostel-agent--sessions-for-agent (agent root)
  "Return live AGENT sessions for ROOT."
  (seq-filter (lambda (session)
                (eq agent (plist-get session :agent)))
              (ghostel-toggle--sessions-for-root 'agent root)))

(defun ghostel-agent--last-session-for-agent (agent root)
  "Return AGENT's latest selected live session in ROOT, or nil."
  (let ((root (ghostel-toggle--normalize-root root)))
    (or (when-let* ((id (alist-get (cons agent root)
                                   ghostel-agent--last-session-alist
                                   nil nil #'equal)))
          (ghostel-toggle--live-session-by-id id))
        (ghostel-toggle--last (ghostel-agent--sessions-for-agent agent root)))))

(defun ghostel-agent--next-label (agent root)
  "Return the display label for a new AGENT session in ROOT."
  (let* ((base (capitalize (symbol-name agent)))
         (count (1+ (length (ghostel-agent--sessions-for-agent agent root)))))
    (if (= count 1)
        base
      (format "%s %d" base count))))

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

(defun ghostel-agent--create (agent root &optional resume hidden)
  "Create a new ghostel terminal running AGENT in ROOT and return its window.
When RESUME is non-nil, use the agent profile's resume arguments.  When
HIDDEN is non-nil, create without touching the current window layout."
  (let* ((profile (ghostel-agent--profile agent))
         (command (ghostel-agent--command-line profile resume))
         (args (list :name (symbol-name agent)
                     :label (ghostel-agent--next-label agent root)
                     :extra (list :agent agent :resume resume)
                     :setup (lambda (buf)
                              (run-at-time ghostel-agent-command-delay nil
                                           #'ghostel-agent--send-command
                                           buf command)))))
    (if hidden
        (apply #'ghostel-toggle--create-session-hidden 'agent root args)
      (apply #'ghostel-toggle-create-session 'agent root args))))

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
1. Not visible + region → show, send region & focus.
2. Not visible → show.
3. Visible + focused → hide.
4. Visible + not focused + region → send region & focus.
5. Visible + not focused → focus."
  (let* ((buf (ghostel-toggle--session-buffer session))
         (win (and buf (get-buffer-window buf t)))
         (in-sidebar (and buf (eq (current-buffer) buf))))
    (ghostel-toggle--select-session session)
    (cond
     ;; Fullscreen → show this (prefix-targeted) session in the frame.
     ;; Plain `s-l' is intercepted earlier to flip; this only runs for
     ;; prefixed `s-l' (e.g. `C-2 s-l').
     ((ghostel-toggle--current-fullscreen-view 'agent)
      (ghostel-toggle--show-session session))
     ;; Visible + focused → hide
     ((and win in-sidebar)
      (when-let* ((last (ghostel-toggle--last-window 'agent)))
        (select-window last))
      (delete-window win))
     ;; Visible + not focused + region → send region & focus
     ((and win (use-region-p))
      (ghostel-agent--send-region buf)
      (ghostel-toggle--remember-last-window 'agent)
      (select-window win))
     ;; Visible + not focused → focus
     (win
      (ghostel-toggle--remember-last-window 'agent)
      (select-window win))
     ;; Buffer exists but not visible → show (sending any region first)
     (buf
      (when (use-region-p)
        (ghostel-agent--send-region buf))
      (ghostel-toggle--remember-last-window 'agent)
      (select-window (ghostel-toggle--show-panel 'agent buf)))
     (t
      (user-error "Ghostel agent session is no longer live")))))

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

(defun ghostel-agent--fullscreen-sl (view)
  "Plain `s-l' action for fullscreen VIEW (sticky fullscreen mode).
On the agent shown full-frame, hide it (collapse to the code).  Otherwise
\(hidden, split, or on a code buffer) re-expand the agent to fullscreen,
sending any active region from a code buffer first."
  (let ((buf (plist-get view :buffer)))
    (ghostel-toggle--fullscreen-flip
     view
     (lambda ()
       (when (and (not (eq (current-buffer) buf))
                  (use-region-p))
         (ghostel-agent--send-region buf))))))

(defun ghostel-agent-toggle-command (arg)
  "Toggle a ghostel agent sidebar based on prefix ARG.
Plain `s-l' toggles the selected session for this project.  While this
project is in agent fullscreen mode it instead toggles the agent's
visibility: hide it (to the code) when shown, or re-expand it to
fullscreen when hidden or split (sending any active region first).
Fullscreen mode is sticky until `s-<return>' demotes it (see
`ghostel-toggle-fullscreen-command').
`C-u s-l' toggles the latest Claude session, creating a resume
session if none exists.  `C-2 s-l' toggles the latest Codex session,
and `C-3 s-l' creates a Codex resume session only when no Codex
session exists yet."
  (interactive "P")
  (let* ((root (ghostel-toggle-command-root))
         (view (and (null arg)
                    (ghostel-toggle--view-for-root 'agent root))))
    (if view
        (ghostel-agent--fullscreen-sl view)
      (let* ((parsed (ghostel-agent--parse-prefix arg))
             (agent (car parsed))
             (resume (cadr parsed))
             (session (if agent
                          (ghostel-agent--last-session-for-agent agent root)
                        (ghostel-toggle--default-session 'agent root))))
        (if session
            (ghostel-agent-toggle-session session)
          (ghostel-toggle--remember-last-window 'agent)
          (select-window (ghostel-agent--create (or agent 'claude)
                                                root resume)))))))

(defun ghostel-agent-new-session-command (arg)
  "Create and show a new ghostel agent session based on prefix ARG.
Plain `s-t' creates Claude, `C-u s-t' creates Claude resume,
`C-2 s-t' creates Codex, and `C-3 s-t' creates Codex resume."
  (interactive "P")
  (let* ((parsed (ghostel-agent--parse-prefix arg))
         (agent (or (car parsed) 'claude))
         (resume (cadr parsed))
         (root (ghostel-toggle-command-root)))
    (ghostel-toggle--remember-last-window 'agent)
    (select-window (ghostel-agent--create agent root resume))))

(defun ghostel-agent-next-session ()
  "Switch to the next ghostel agent session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'agent 1))

(defun ghostel-agent-previous-session ()
  "Switch to the previous ghostel agent session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'agent -1))

(defun ghostel-agent--ensure-buffer (agent root resume)
  "Return AGENT's live buffer in ROOT, creating the session if needed."
  (or (when-let* ((session (ghostel-agent--last-session-for-agent agent root)))
        (ghostel-toggle--session-buffer session))
      (progn
        (ghostel-agent--create agent root resume 'hidden)
        (when-let* ((session (ghostel-agent--last-session-for-agent agent root)))
          (ghostel-toggle--session-buffer session)))))

(defun ghostel-agent-home-toggle (arg)
  "Toggle a fullscreen agent session rooted in the home directory.
Prefix ARG follows `ghostel-agent-toggle-command' conventions: plain =
Claude, `C-u' = Claude resume, `C-2' = Codex, `C-3' = Codex resume.
Promotes the home (~/) agent to fullscreen from any project; press again
to restore the previous layout.  The home fullscreen also dismisses with
plain `s-l' (it is registered `:dismiss', so `s-l' dismisses it and
C-s-l re-fires it)."
  (interactive "P")
  (let* ((parsed (ghostel-agent--parse-prefix arg))
         (agent (or (car parsed) 'claude))
         (resume (cadr parsed)))
    (ghostel-toggle-home-toggle
     'agent
     (lambda (root) (ghostel-agent--ensure-buffer agent root resume)))))

;;; Console output cleanup (s-c / s-')

(defconst ghostel-agent--fresh-line-re
  "\\`[ \t]*\\(?:[-*•]\\|[0-9]+[.)]\\||\\)\\(?:[ \t]\\|\\'\\)"
  "Line that starts a fresh logical line and must not be folded onto the
previous one: a list item (`- ', `* ', `1. ', `2) ') or a `| ' quote line.")

(defun ghostel-agent--dedent (lines)
  "Strip the leading whitespace shared by every non-blank line in LINES."
  (let* ((indents (delq nil
                        (mapcar (lambda (l)
                                  (and (string-match "[^ \t]" l)
                                       (match-beginning 0)))
                                lines)))
         (common (if indents (apply #'min indents) 0)))
    (mapcar (lambda (l) (if (>= (length l) common) (substring l common) l))
            lines)))

(defun ghostel-agent--strip-quote (line)
  "Remove a leading quote marker from LINE, keeping any indent.
Handles the ASCII `| ' marker and the block-element bars (`▏▎▍▌') Claude
renders on each wrapped line of a blockquote."
  (if (string-match "\\`\\([ \t]*\\)[|▏▎▍▌][ \t]?" line)
      (concat (match-string 1 line) (substring line (match-end 0)))
    line))

(defun ghostel-agent--fold-lines (lines)
  "Fold wrapped continuation LINES into one line per paragraph.
Blank lines, list items and `| ' quote lines stay on their own line;
block-element quote bars (`▎', `▌', …) are stripped and their wrapped lines
rejoined.  A bare quote bar counts as a blank paragraph break."
  (let (out current)
    (dolist (line lines)
      (cond
       ((string-blank-p (ghostel-agent--strip-quote line))
        (when current (push current out) (setq current nil))
        (push "" out))
       ((or (null current)
            (string-match-p ghostel-agent--fresh-line-re line))
        (when current (push current out))
        (setq current (string-trim-right (ghostel-agent--strip-quote line))))
       (t
        (setq current (concat current " "
                              (string-trim (ghostel-agent--strip-quote line)))))))
    (when current (push current out))
    (nreverse out)))

(defun ghostel-agent--clean-text (text &optional no-join)
  "Return TEXT cleaned of Claude console formatting.
Strips the `⏺'/`●' response marker, the common indentation, and quote-bar
prefixes (ASCII `| ' and block bars like `▎').  Unless NO-JOIN, folds
wrapped lines within each paragraph (blank lines and list items stay
separate)."
  (let* ((text (replace-regexp-in-string "[⏺●]" " " text))
         (lines (ghostel-agent--dedent (split-string text "\n")))
         (lines (if no-join
                    (mapcar #'ghostel-agent--strip-quote lines)
                  (ghostel-agent--fold-lines lines))))
    (string-trim (string-join lines "\n"))))

(defun ghostel-agent-copy-clean (beg end &optional no-join)
  "Copy region BEG..END to the kill ring, cleaned of Claude console formatting.
Strips the `⏺' marker, the common indentation and `| ' quote prefixes,
and folds wrapped lines within each paragraph.  With prefix arg NO-JOIN
\(`C-u'), keep the original line breaks — use this for code blocks."
  (interactive "r\nP")
  (let ((clean (ghostel-agent--clean-text
                (buffer-substring-no-properties beg end) no-join)))
    (kill-new clean)
    (deactivate-mark)
    (when (fboundp 'ghostel-readonly-exit)
      (ignore-errors (ghostel-readonly-exit)))
    (message "Copied cleaned text (%d chars)%s"
             (length clean) (if no-join " [no join]" ""))))

(defun ghostel-agent-quote-region (beg end &optional raw)
  "Quote region BEG..END back into this agent's prompt as a reply.
Prefixes each line with `> ', exits copy mode, and pastes the quote plus a
blank line into the live prompt, leaving point below it ready for your
message.  Normally the selected output is cleaned first (see
`ghostel-agent--clean-text': strips the response marker and `| ' quote
prefixes, un-wraps hard-wrapped lines).  With prefix arg RAW (`C-u'), quote
the text verbatim and skip that cleanup — use it for code."
  (interactive "r\nP")
  (let* ((text (buffer-substring-no-properties beg end))
         (source (if raw (string-trim text) (ghostel-agent--clean-text text)))
         (quoted (mapconcat (lambda (l) (if (string-empty-p l) ">" (concat "> " l)))
                            (split-string source "\n")
                            "\n")))
    (deactivate-mark)
    (when (fboundp 'ghostel-readonly-exit)
      (ignore-errors (ghostel-readonly-exit)))
    (ghostel-paste-string (concat quoted "\n\n"))))

(global-set-key (kbd "s-l") #'ghostel-agent-toggle-command)
(global-set-key (kbd "s-t") #'ghostel-agent-new-session-command)
(global-set-key (kbd "C-s-l") #'ghostel-agent-home-toggle)

;;; ghostel-agents.el ends here

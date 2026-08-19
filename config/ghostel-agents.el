;;; ghostel-agents.el --- Ghostel coding agent sidebar on ghostel-toggle -*- lexical-binding: t -*-

;; The `agent' instantiation of the ghostel-toggle library: configurable
;; coding-agent CLIs in a right sidebar (`s-l'), with tabs (`s-t'),
;; prefix-selected creation commands (`C-u'/`C-2'/`C-3'), per-project
;; fullscreen (`s-<return>' while
;; focused), a home-directory fullscreen agent (`C-s-l'), region sending,
;; and console-output cleanup helpers (`s-c', `s-'').

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ghostel-toggle)

(defvar ghostel-agent-default-command "claude"
  "Complete command used to create an agent without a prefix.")

(defvar ghostel-agent-c-u-command "claude --resume"
  "Complete command used to create an agent with a `C-u' prefix.")

(defvar ghostel-agent-c-2-command "codex"
  "Complete command used to create an agent with a `C-2' prefix.")

(defvar ghostel-agent-c-3-command "codex resume"
  "Complete command used to create an agent with a `C-3' prefix.")

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

;;; Optional paired backticks
;;;
;;; Comment out the `define-key' at the end of this section to restore
;;; Ghostel's normal one-backtick-per-keypress behavior.

(defvar-local ghostel-agent--pending-backticks nil
  "Number of generated closing backticks still ahead of the cursor.")

(defvar-local ghostel-agent--backticks-expandable nil
  "Non-nil while consecutive backtick presses may grow the current pair.")

(defun ghostel-agent--send-key-n (key count)
  "Send Ghostel KEY COUNT times."
  (dotimes (_ count)
    (ghostel-send-key key)))

(defun ghostel-agent--reset-backticks-after-command ()
  "Forget a generated closer after commands that may move the terminal cursor."
  (unless (memq this-command
                '(ghostel-agent-backtick-dwim
                  ghostel--self-insert
                  ghostel-yank
                  ghostel-yank-pop
                  universal-argument
                  universal-argument-more
                  digit-argument
                  negative-argument))
    (setq ghostel-agent--pending-backticks nil
          ghostel-agent--backticks-expandable nil)))

(defun ghostel-agent-backtick-dwim (&optional literal)
  "Insert paired backticks in a managed Ghostel agent prompt.

One press inserts an inline pair and leaves the cursor between it.
Three consecutive presses turn that pair into a fenced block with a blank
line for input.  After typing inside either form, another press skips its
generated closer.  With prefix argument LITERAL, insert one backtick."
  (interactive "P")
  (add-hook 'post-command-hook
            #'ghostel-agent--reset-backticks-after-command nil t)
  (cond
   (literal
    (setq ghostel-agent--backticks-expandable nil)
    (ghostel-send-string "`"))
   ((and ghostel-agent--backticks-expandable
         (eq last-command 'ghostel-agent-backtick-dwim)
         (eq ghostel-agent--pending-backticks 1))
    ;; Grow `|' into ``|``; a third press handles the fenced-block case.
    (ghostel-send-string "`")
    (ghostel-send-key "right")
    (ghostel-send-string "`")
    (ghostel-agent--send-key-n "left" 2)
    (setq ghostel-agent--pending-backticks 2))
   ((and ghostel-agent--backticks-expandable
         (eq last-command 'ghostel-agent-backtick-dwim)
         (eq ghostel-agent--pending-backticks 2))
    ;; Starting from ``|``, add the outer ticks and blank fenced-block line.
    ;; Bracketed paste keeps the embedded newlines from submitting the prompt.
    (ghostel-paste-string "`\n\n`")
    (ghostel-agent--send-key-n "left" 2)
    (setq ghostel-agent--pending-backticks 3
          ghostel-agent--backticks-expandable nil))
   (ghostel-agent--pending-backticks
    ;; A fenced closer also has its leading newline ahead of the cursor.
    (ghostel-agent--send-key-n
     "right"
     (if (= ghostel-agent--pending-backticks 3)
         4
       ghostel-agent--pending-backticks))
    (setq ghostel-agent--pending-backticks nil
          ghostel-agent--backticks-expandable nil))
   (t
    (ghostel-send-string "``")
    (ghostel-send-key "left")
    (setq ghostel-agent--pending-backticks 1
          ghostel-agent--backticks-expandable t))))

;; This is the only line to comment out if the behavior does not stick.
(define-key ghostel-agent-session-mode-map (kbd "`")
            #'ghostel-agent-backtick-dwim)

(ghostel-toggle-define-kind 'agent
                            :side 'right
                            :size 0.55
                            :minor-mode 'ghostel-agent-session-mode)

(defun ghostel-agent--sessions-with-label (label root)
  "Return live agent sessions with base LABEL in ROOT."
  (seq-filter (lambda (session)
                (equal label (plist-get session :agent-label)))
              (ghostel-toggle--sessions-for-root 'agent root)))

(defun ghostel-agent--next-label (base root)
  "Return the next display label for BASE in ROOT."
  (let ((count (1+ (length (ghostel-agent--sessions-with-label base root)))))
    (if (= count 1)
        base
      (format "%s %d" base count))))

(defun ghostel-agent--command-for-prefix (arg)
  "Return the configured creation command for raw prefix ARG."
  (cond
   ((null arg) ghostel-agent-default-command)
   ((equal arg '(4)) ghostel-agent-c-u-command)
   ((equal arg 2) ghostel-agent-c-2-command)
   ((equal arg 3) ghostel-agent-c-3-command)
   (t (user-error "Unknown ghostel agent prefix: %S" arg))))

(defun ghostel-agent--command-program (command)
  "Return the executable token from complete shell COMMAND."
  (unless (and (stringp command) (not (string-empty-p command)))
    (user-error "Ghostel agent command must be a non-empty string"))
  (or (car (split-string-shell-command command))
      (user-error "Ghostel agent command must name an executable")))

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

(defun ghostel-agent--create (command root &optional hidden)
  "Create a new ghostel terminal running COMMAND in ROOT and return its window.
When HIDDEN is non-nil, create without touching the current window layout."
  (let* ((program (file-name-nondirectory
                   (ghostel-agent--command-program command)))
         (base-label (capitalize program))
         (args (list :name program
                     :label (ghostel-agent--next-label base-label root)
                     :extra (list :agent-label base-label :command command)
                     :setup (lambda (buf)
                              (run-at-time ghostel-agent-command-delay nil
                                           #'ghostel-agent--send-command
                                           buf command)))))
    (if hidden
        (apply #'ghostel-toggle--create-session-hidden 'agent root args)
      (apply #'ghostel-toggle-create-session 'agent root args))))

(defun ghostel-agent--send-region (buf)
  "Send the active region with file context to the ghostel agent BUF.
Leave a blank line after the pasted block for continuing the prompt."
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (file (let ((f (or (buffer-file-name) (buffer-name)))
                     (root (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))))
                 (if root (file-relative-name f root) f)))
         (line-beg (line-number-at-pos beg))
         (line-end (line-number-at-pos end))
         (formatted (format "%s:%d-%d\n```\n%s\n```\n\n"
                            file line-beg line-end text)))
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
     ((ghostel-toggle--current-fullscreen-view 'agent (plist-get session :root))
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
Every prefix toggles the selected session for this project.  Prefixes only
choose the command used when no session exists yet.  While this project is
in agent fullscreen mode, every prefix instead toggles the agent's visibility:
hide it (to the code) when shown, or re-expand it to fullscreen when hidden
or split (sending any active region first).
Fullscreen mode is sticky until `s-<return>' demotes it (see
`ghostel-toggle-fullscreen-command').
When creation is needed, ARG selects one of `ghostel-agent-default-command',
`ghostel-agent-c-u-command', `ghostel-agent-c-2-command', and
`ghostel-agent-c-3-command'."
  (interactive "P")
  (let* ((root (ghostel-toggle-command-root))
         (view (ghostel-toggle--view-for-root 'agent root))
         (command (ghostel-agent--command-for-prefix arg)))
    (if view
        (ghostel-agent--fullscreen-sl view)
      (let ((session (ghostel-toggle--default-session 'agent root)))
        (if session
            (ghostel-agent-toggle-session session)
          (ghostel-toggle--remember-last-window 'agent)
          (select-window (ghostel-agent--create command root)))))))

(defun ghostel-agent-new-session-command (arg)
  "Create and show a new ghostel agent session based on prefix ARG.
ARG selects the same four configurable commands as
`ghostel-agent-toggle-command'."
  (interactive "P")
  (let* ((command (ghostel-agent--command-for-prefix arg))
         (root (ghostel-toggle-command-root)))
    (ghostel-toggle--remember-last-window 'agent)
    (select-window (ghostel-agent--create command root))))

(defun ghostel-agent-next-session ()
  "Switch to the next ghostel agent session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'agent 1))

(defun ghostel-agent-previous-session ()
  "Switch to the previous ghostel agent session for this project."
  (interactive)
  (ghostel-toggle-cycle-session 'agent -1))

(defun ghostel-agent--ensure-buffer (command root)
  "Return the selected live agent buffer in ROOT, creating one if needed.
COMMAND is used only when ROOT has no session."
  (or (when-let* ((session (ghostel-toggle--default-session 'agent root)))
        (ghostel-toggle--session-buffer session))
      (progn
        (ghostel-agent--create command root 'hidden)
        (when-let* ((session (ghostel-toggle--default-session 'agent root)))
          (ghostel-toggle--session-buffer session)))))

(defun ghostel-agent-home-toggle (arg)
  "Toggle a fullscreen agent session rooted in the home directory.
Reuse the selected home agent regardless of prefix.  When no home session
exists, prefix ARG selects the same configurable creation command as
`ghostel-agent-toggle-command'.
Promotes the home (~/) agent to fullscreen from any project; press again
to restore the previous layout.  The home fullscreen also dismisses with
plain `s-l' (it is registered `:dismiss', so `s-l' dismisses it and
  C-s-l re-fires it)."
  (interactive "P")
  (let ((command (ghostel-agent--command-for-prefix arg)))
    (ghostel-toggle-home-toggle
     'agent
     (lambda (root)
       (ghostel-agent--ensure-buffer command root)))))

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
        (let ((line (ghostel-agent--strip-quote line)))
          ;; Codex hangs later prose paragraphs under its response bullet.
          ;; At a paragraph boundary that indent is presentation, whereas
          ;; indentation on a list item can still carry nesting information.
          (setq current
                (if (string-match-p ghostel-agent--fresh-line-re line)
                    (string-trim-right line)
                  (string-trim line)))))
       (t
        (setq current (concat current " "
                              (string-trim (ghostel-agent--strip-quote line)))))))
    (when current (push current out))
    (nreverse out)))

(defun ghostel-agent--clean-text (text &optional no-join)
  "Return TEXT cleaned of Claude/Codex console formatting.
Strips a leading `⏺'/`●'/`•' response marker, the common indentation,
and quote-bar prefixes (ASCII `| ' and block bars like `▎').  Unless
NO-JOIN, folds wrapped lines within each paragraph (blank lines and list items
stay separate)."
  ;; Replace the marker field with Codex/Claude's two-column hanging indent so
  ;; `ghostel-agent--dedent' can remove that same offset from every line.
  (let* ((text (replace-regexp-in-string
                "\\`[ \t\n]*[⏺●•][ \t]+" "  " text))
         ;; Keep the older cleanup for rendered Claude markers inside a
         ;; selection, but do not erase real Codex `•' list items later on.
         (text (replace-regexp-in-string "[⏺●]" " " text))
         (lines (ghostel-agent--dedent (split-string text "\n")))
         (lines (if no-join
                    (mapcar #'ghostel-agent--strip-quote lines)
                  (ghostel-agent--fold-lines lines))))
    (string-trim (string-join lines "\n"))))

(defun ghostel-agent-copy-clean (beg end &optional no-join)
  "Copy region BEG..END to the kill ring, cleaned of agent console formatting.
Strips the leading response marker, presentation indentation and `| ' quote
prefixes, and folds wrapped lines within each paragraph.  With prefix arg NO-JOIN
\(`C-u'), keep the original line breaks — use this for code blocks."
  (interactive "r\nP")
  (let ((clean (ghostel-agent--clean-text
                (buffer-substring-no-properties beg end) no-join)))
    (kill-new clean)
    (deactivate-mark)
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

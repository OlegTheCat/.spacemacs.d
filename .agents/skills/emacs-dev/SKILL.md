---
name: emacs-dev
description: This skill should be used when the user asks to "edit emacs config", "develop elisp", "debug emacs", "modify spacemacs", "emacsclient eval", "reload emacs config", or mentions Emacs Lisp development, Spacemacs configuration, or interacting with a running Emacs instance.
activation:
  keywords:
    - emacs
    - elisp
    - emacsclient
    - spacemacs
    - emacs lisp
    - emacs config
    - emacs server
  tools:
    - tool: Bash
      match: "emacsclient.*"
      action: suggest
  priority: 80
---

# Emacs Dev

Develop and debug Emacs Lisp interactively using `emacsclient --eval` against a running Emacs GUI instance.

## Prerequisites

Verify the Emacs server is running before any operation:

```bash
emacsclient -e 't'
```

If this fails, ask the user to start the Emacs server (`M-x server-start` in Emacs).

**Never use `emacsclient -a "" --eval ...`** — that starts a headless daemon disconnected from the GUI Emacs. Always use `emacsclient --eval` without the `-a` flag.

## Eval Syntax Rules

`emacsclient --eval` evaluates a **single sexp**. Wrap multiple forms in `progn`:

```bash
emacsclient --eval '(progn (do-thing-1) (do-thing-2))'
```

**Quoting**: Use single quotes around the elisp expression. For literal quotes inside elisp, escape with `'\''`:

```bash
emacsclient --eval '(mapcar #'\''buffer-name (buffer-list))'
```

## Inspecting Emacs State

Any elisp introspection function works via `emacsclient --eval`. Common examples:

### Windows and layout

```bash
emacsclient --eval '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w))) (window-list))'
```

### List buffers

```bash
emacsclient --eval '(mapcar #'\''buffer-name (buffer-list))'
```

### Frame properties

```bash
emacsclient --eval '(mapcar (lambda (f) (list (frame-parameter f '\''name) (frame-width f) (frame-height f))) (frame-list))'
```

### Modeline

```bash
emacsclient --eval '(substring-no-properties (format-mode-line mode-line-format))'
```

Raw `(format-mode-line mode-line-format)` returns text with properties (faces, XPM images for Powerline). Wrap with `substring-no-properties` for readable output.

### Cursor position and selected text

Use `with-selected-window` (not `with-current-buffer`) to access cursor and selection state — `with-current-buffer` does not preserve the window's selection.

```bash
# Cursor position (point, line, column)
emacsclient --eval '(with-selected-window (selected-window) (list :point (point) :line (line-number-at-pos) :column (current-column)))'

# Selected text (nil if no selection)
emacsclient --eval '(with-selected-window (selected-window) (when mark-active (buffer-substring-no-properties (region-beginning) (region-end))))'
```

### Minibuffer active check

```bash
emacsclient --eval '(minibuffer-window-active-p (minibuffer-window))'
```

Always check minibuffer state before sending interactive commands — an active minibuffer blocks eval.

## Loading & Reloading Code

**Editing a `.el` file does NOT load it into Emacs.** Always load explicitly after editing:

```bash
# Load a config file
emacsclient --eval '(load (expand-file-name "config/my-config.el" "~/.spacemacs.d/"))'

# Redefine a single function inline
emacsclient --eval '(defun my/function () (message "hello"))'
```

Workflow for iterating on elisp:
1. Edit the `.el` file using standard file tools
2. Load it into Emacs with `(load ...)`
3. Verify by calling the function or inspecting state

## Window-Mutating Tests in the Live Frame

`emacsclient --eval` runs in the user's **live GUI frame**. Read-only checks
(`lookup-key`, `buffer-list`, byte-compile, `(load ...)`) are safe and silent.
But tests that mutate window layout — driving fullscreen/sidebar commands,
`set-window-configuration`, `delete-other-windows`, `split-window` — disrupt
whatever the user is doing, and can error depending on where their point is.

**Ask the user to take over the frame for a couple of minutes before running
disruptive window tests.** They'll step away and let it drive cleanly. Don't
poke their live layout while they're working.

When you do run such a test, make it self-restoring and focus-immune:

```elisp
(let ((orig (current-window-configuration))
      (ignore-window-parameters t))   ; immune to current side-window focus
  (unwind-protect
      (progn ;; ...window-mutating test...
        )
    (set-window-configuration orig)))
```

- Save `current-window-configuration` up front and restore it in
  `unwind-protect`, so an error mid-test still restores the layout.
- Bind `ignore-window-parameters` to `t` around the test. Without it,
  `delete-other-windows` signals **"Cannot make side window the only window"**
  when the user is focused in a side window (e.g. a sidebar).
- Save/restore any global state the code touches (e.g. registries, alists) the
  same way.
- Write results to a scratch file (`with-temp-file`) and read it back —
  `--eval` prints the return value, not `message` output.

## Killing Buffers Safely

Two prompts can block `emacsclient` when killing buffers:

### Modified buffer prompt

Emacs prompts to save before killing. Bypass:

```bash
emacsclient --eval '(with-current-buffer "buffer-name" (set-buffer-modified-p nil) (kill-buffer))'
```

### Process/hook prompt

`kill-buffer-query-functions` may prompt for buffers with running processes (vterm, shell). Bypass:

```bash
emacsclient --eval '(let ((kill-buffer-query-functions nil)) (kill-buffer "buffer-name"))'
```

## Development Workflow Summary

1. **Check server**: `emacsclient -e 't'`
2. **Inspect current state**: List buffers, windows, frames as needed
3. **Edit config files**: Use standard file editing tools
4. **Load into Emacs**: `(load ...)` after every file change
5. **Test interactively**: Eval expressions to verify behavior
6. **Iterate**: Edit, load, test until correct

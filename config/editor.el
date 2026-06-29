;;; editor.el --- General editor settings -*- lexical-binding: t -*-

;; JS indent = 2
(setq js-indent-level 2)

;; Windmove
(windmove-default-keybindings)

;; Ensure GUI Emacs can find user-installed executables (macOS often doesn't
;; inherit your shell PATH).
(let ((opencode-bin (expand-file-name "~/.opencode/bin")))
  (when (file-directory-p opencode-bin)
    (add-to-list 'exec-path opencode-bin)
    (setenv "PATH" (concat opencode-bin path-separator (or (getenv "PATH") "")))))

;; Golden ratio
(spacemacs/toggle-golden-ratio-on)
(advice-add 'magit-status :after (lambda (&rest args) (golden-ratio)))
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
(add-to-list 'golden-ratio-exclude-modes "IELM")
(add-to-list 'golden-ratio-exclude-modes "eshell-mode")
(add-to-list 'golden-ratio-exclude-modes "dired-mode")
(add-to-list 'golden-ratio-exclude-modes "sr-mode")
(add-to-list 'golden-ratio-exclude-modes "sr-buttons-mode")

;; Git Link
(setq git-link-use-commit t)
(setq git-link-open-in-browser nil)
(setq github-browse-file-show-line-at-point t)

;;; Fix git-link URLs: remove "ssh." prefix from generated links
;;; e.g., https://ssh.gitlab.company.com/... → https://gitlab.company.com/...
(defun my/git-link-remove-ssh-prefix (url)
  "Remove 'ssh.' prefix from hostname in URL."
  (replace-regexp-in-string
   "\\(https?://\\)ssh\\." "\\1" url))

(advice-add 'git-link--new :filter-args
            (lambda (args)
              "Filter git-link URL to remove ssh. prefix."
              (cons (my/git-link-remove-ssh-prefix (car args))
                    (cdr args))))

;; Persistent scratch
(persistent-scratch-setup-default)

;; Smartparens
(with-eval-after-load 'smartparens
  (sp-use-paredit-bindings)

  (define-key smartparens-mode-map (kbd "M-(") (lambda () (interactive) (sp-wrap-with-pair "(")))
  (define-key smartparens-mode-map (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
  (define-key smartparens-mode-map (kbd "M-{") (lambda () (interactive) (sp-wrap-with-pair "{")))
  (define-key smartparens-mode-map (kbd "M-\"") (lambda () (interactive) (sp-wrap-with-pair "\""))))

;; Prog mode tweaks
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)

;; Custom keybindings
(global-unset-key (kbd "s-k"))
(global-set-key (kbd "C-s") 'helm-occur)

;; Use Menlo for vterm/Claude Code buffers.
(setq vterm-max-scrollback 100000)

(add-hook 'vterm-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :family "Menlo")))
(add-hook 'ghostel-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :family "Menlo")))

;; Reverse IM: use Latin keybindings with non-Latin keyboard layouts
(require 'reverse-im)
(reverse-im-activate "ukrainian-computer")

;; MC keybindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Flash jump
(require 'flash)
(global-set-key (kbd "s-j") 'flash-jump)

;; GPTel: use Claude as default backend
(with-eval-after-load 'gptel
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (lambda () (getenv "ANTHROPIC_API_KEY")))
        gptel-model 'claude-opus-4-6))

;;; editor.el ends here

;;; pr-review.el --- GitHub PR review -*- lexical-binding: t -*-

(defun my/pr-review-current-branch ()
  "Open pr-review for the PR/MR associated with the current git branch.
Resolves the URL via gh (GitHub), falling back to glab (GitLab)."
  (interactive)
  (let ((url (or (with-temp-buffer
                   (when (eq 0 (call-process "gh" nil '(t nil) nil
                                             "pr" "view" "--json" "url" "--jq" ".url"))
                     (string-trim (buffer-string))))
                 (with-temp-buffer
                   (when (eq 0 (call-process "glab" nil '(t nil) nil
                                             "mr" "view" "--output" "json"))
                     (alist-get 'web_url
                                (json-parse-string (buffer-string)
                                                   :object-type 'alist)))))))
    (if url
        (pr-review url)
      (user-error "No PR/MR found for current branch (tried gh and glab)"))))

;; Thread content on in-diff markers is shown via eldoc (point on the marker
;; line); allow it to span multiple echo-area lines in review buffers only.
(add-hook 'pr-review-mode-hook
          (lambda () (setq-local eldoc-echo-area-use-multiline-p t)))

;; Reuse forge's ^forge authinfo token instead of a separate ^emacs-pr-review entry.
(with-eval-after-load 'pr-review
  (setq pr-review-ghub-auth-name 'forge)
  ;; self-hosted GitLab; API host must end in /api/v4 (ghub convention),
  ;; authinfo machine must match it exactly
  (add-to-list 'pr-review-forges-alist
               '("gitlab.grammarly.io" . (gitlab "gitlab.grammarly.io/api/v4" "oleh.palianytsia"))))

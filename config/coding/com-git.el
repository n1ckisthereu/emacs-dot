(use-package magit
	:custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
	:ensure t)

;; Diff-hl
(use-package diff-hl
		:after magit
		:ensure t
		:config
		(diff-hl-flydiff-mode)
		(diff-hl-margin-mode)
		(add-hook 'diff-hl-mode-hook 'diff-hl-show-hunk-mouse-mode)
		:hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
					 (magit-post-refresh . diff-hl-magit-post-refresh)
					 (vc-checkin				 . diff-hl-update)
					 (diff-hl-mode-hook  . diff-hl-show-hunk-mouse-mode))
		:init (global-diff-hl-mode))

(provide 'com-git)

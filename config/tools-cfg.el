(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'auto)
  (projectile-project-search-path '("~/"))
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :ensure t)

(use-package diff-hl
    :after magit
    :config
    (diff-hl-flydiff-mode)
    (diff-hl-margin-mode)
    :ensure t
    :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
                 (magit-post-refresh . diff-hl-magit-post-refresh)
                 (vc-checkin         . diff-hl-update)
                )
    :init (global-diff-hl-mode)
    )

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(provide 'tools-cfg)

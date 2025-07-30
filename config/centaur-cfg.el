; Tabs

(use-package centaur-tabs
  :ensure t
  :demand
  :bind
  ("C-S-h" . centaur-tabs-backward)
  ("C-S-l" . centaur-tabs-forward)
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 15)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-show-navigation-buttons t)
  :hook (dashboard-mode . centaur-tabs-local-mode)
  )

(provide 'centaur-cfg)

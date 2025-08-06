;; LSP configuration
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-auto-configure t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-auto-activate t)
  (lsp-idle-delay 0.1)
  :init
  :ensure t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-delay 0.05)
  :ensure t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after treemacs
  :init
  (setq lsp-treemacs-theme "nerd-icons")
  :config
  (lsp-treemacs-sync-mode 1)
  :ensure t)

;; Dap debugger
(use-package dap-mode
  :ensure t)

;; Fly check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Company for completion
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :ensure t)

;; Dockerfile highlight
(use-package dockerfile-mode
  :defer t
  :ensure t)

;; Projectile
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

;; Magit
(use-package magit
  :ensure t)

;; Diff-hl
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
    :init (global-diff-hl-mode))

(provide 'code-cfg)

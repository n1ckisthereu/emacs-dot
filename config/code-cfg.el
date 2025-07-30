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
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
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

(use-package dap-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :ensure t)


(use-package dockerfile-mode
  :defer t
  :ensure t)

(provide 'code-cfg)

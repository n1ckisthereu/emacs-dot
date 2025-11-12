;; LSP configuration
(use-package lsp-mode
	:commands (lsp lsp-deferred)
	:hook (lsp-mode . lsp-enable-which-key-integration)
	:custom
	(lsp-auto-configure t)
	(lsp-log-io nil)
	:init
	:ensure t)

(use-package lsp-diagnostics
	:after flycheck
	:config (lsp-diagnostics-flycheck-enable))

(use-package lsp-ui
	:commands lsp-ui-mode
	:custom
	(lsp-ui-sideline-show-diagnostics t)
	(lsp-ui-sideline-show-code-actions t)
	(lsp-ui-doc-position 'at-point)
	(lsp-ui-doc-show-with-mouse t)
	(lsp-ui-doc-enable t)
	(lsp-headerline-breadcrumb-enable nil)
	(lsp-ui-doc-delay 0)
	:ensure t)


(use-package lsp-treemacs
	:commands lsp-treemacs-errors-list
	:after treemacs
  :custom
  (lsp-treemacs-theme "nerd-icons-ext")
	:config
	(lsp-treemacs-sync-mode 1)
	:ensure t)


;; Dap debugger
(use-package dap-mode
	:custom
	(dap-auto-configure-features '(sessions locals tooltip))
	:config
	(setq lsp-enable-dap-auto-configure nil)
	(with-eval-after-load 'dap-ui
		(unless (display-graphic-p)
			(set-face-background 'dap-ui-marker-face "color-166")
			(set-face-attribute 'dap-ui-marker-face nil :inherit nil)
			(set-face-background 'dap-ui-pending-breakpoint-face "blue")
			(set-face-attribute 'dap-ui-verified-breakpoint-face nil
                        :inherit 'dap-ui-pending-breakpoint-face)))
	:ensure t)

;; Fly check
(use-package flycheck
	:ensure t
	:init (global-flycheck-mode))

;; (use-package flyover
;; 	:ensure t
;; 	:custom
;; 	(flyover-levels '(error warning info)))

(use-package treesit-auto
		:ensure t
		:config
		(global-treesit-auto-mode)
		:custom
		(treesit-font-lock-level 4)
		(treesit-auto-install t))

(provide 'com-lsp)

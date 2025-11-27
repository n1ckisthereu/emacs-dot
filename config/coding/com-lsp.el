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

;; (use-package lsp-treemacs
;; 	:commands lsp-treemacs-errors-list
;; 	:after treemacs lsp-mode
;;   ;; :custom
;;   ;; (lsp-treemacs-theme "nerd-icons-ext")
;; 	:config
;; 	(lsp-treemacs-sync-mode 1)
;; 	:ensure t)

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
	:config (global-treesit-auto-mode)
	:custom
	(treesit-auto-install nil)
	(treesit-font-lock-level 4)
	:init
	(setq treesit-language-source-alist
				'((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git" "v0.23.3"))
					(c          . ("https://github.com/tree-sitter/tree-sitter-c.git" "v0.23.6"))
					(cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git" "v0.23.4"))
					(css        . ("https://github.com/tree-sitter/tree-sitter-css.git" "v0.23.2"))
					(html       . ("https://github.com/tree-sitter/tree-sitter-html.git" "v0.23.2"))
					(json       . ("https://github.com/tree-sitter/tree-sitter-json.git" "v0.24.8"))
					(lua        . ("https://github.com/tree-sitter/tree-sitter-lua.git"))
					(make       . ("https://github.com/alemuller/tree-sitter-make.git"))
					(markdown   . ("https://github.com/ikatyang/tree-sitter-markdown.git"))
					(python     . ("https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6"))
					(ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git" "v0.23.1"))
					(rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git" "v0.23.3"))
					))
	:ensure t
	)

(provide 'com-lsp)

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
	:init
	(setq lsp-treemacs-theme "nerd-icons")
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

(use-package corfu
  :ensure t :init (global-corfu-mode)
  ;; ativa popupinfo junto
  (corfu-popupinfo-mode 1)
	:custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-max-height 14)
  (corfu-popupinfo-resize t)
	(corfu-popupinfo-delay 0)
  (corfu-popupinfo-direction '(right vertical left)))

(use-package corfu-mouse
	:load-path "~/.config/emacs/emacs-local-plugins/corfu-terminal"
	:config
	(if (display-graphic-p)
			(corfu-mouse-mode 1)))

(use-package nerd-icons-corfu
	:after corfu
	:config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
	:ensure t)

(use-package cape
	:ensure t
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (setq-default completion-at-point-functions
                (append (default-value 'completion-at-point-functions)
                        (list #'cape-dabbrev #'cape-file #'cape-abbrev #'cape-tex #'cape-dict))))

(use-package orderless
	:ensure t
	:init
	(setq completion-styles '(orderless basic))
	(setq completion-category-defaults nil)
	(setq completion-category-overrides
				 '((file (styles partial-completion))))	)

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
	(projectile-project-search-path '("~/" "~/.config/emacs"))
	(projectile-enable-caching t)
	;; current directory as the project root like neovim plugins
	(projectile-require-project-root nil)
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
		(add-hook 'diff-hl-mode-hook 'diff-hl-show-hunk-mouse-mode)
		:ensure t
		:hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
					 (magit-post-refresh . diff-hl-magit-post-refresh)
					 (vc-checkin				 . diff-hl-update)
					 (diff-hl-mode-hook  . diff-hl-show-hunk-mouse-mode))
		:init (global-diff-hl-mode))

(use-package treesit-auto
		:ensure t
		:config
		(global-treesit-auto-mode)
		:custom
		(treesit-font-lock-level 4)
		(treesit-auto-install t))

(use-package breadcrumb
	:load-path "~/.config/emacs/emacs-local-plugins/breadcrumb"
	:hook
  (prog-mode . breadcrumb-local-mode)
  :custom
  ;; Add nerd-icons to breadcrumb
  (breadcrumb-imenu-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-project-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-imenu-max-length 0.5)
  (breadcrumb-project-max-length 0.5)
  :preface
  ;; Add icons to breadcrumb
  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-icon-for-file string)
                              " " string)
                    (concat (nerd-icons-faicon
                             "nf-fa-folder_open"
                             :face 'breadcrumb-project-crumbs-face)
                            " "
                            string)))))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-codicon
                               "nf-cod-symbol_field"
                               :face 'breadcrumb-imenu-leaf-face)
                              " " string)
                    (cond ((string= string "Packages")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Requires")
                           (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Method")
                           (concat (nerd-icons-codicon "nf-cod-symbol_method" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Class")
                           (concat (nerd-icons-codicon "nf-cod-symbol_class" :face 'breadcrumb-imenu-crumbs-face) " " string))
													((string= string "Field")
                           (concat (nerd-icons-codicon "nf-cod-symbol_field" :face 'breadcrumb-imenu-crumbs-face) " " string))
													((string= string "Module")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((or (string= string "Variable") (string= string "Variables"))
                           (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Function")
                           (concat (nerd-icons-codicon "nf-cod-symbol_field" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          (t string)))))))

(use-package copilot
  :hook (prog-mode . copilot-mode)
	:custom
	(copilot-idle-delay 1)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
	:ensure t)

(use-package corfu-terminal
	:load-path "~/.config/emacs/emacs-local-plugins/corfu-terminal"
	:config
	(unless (display-graphic-p)
		(corfu-terminal-mode +1)))

(provide 'code-cfg)

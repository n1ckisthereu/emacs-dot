(use-package corfu
  :ensure t
	:init (global-corfu-mode)
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

(use-package corfu-terminal
	:load-path "~/.config/emacs/emacs-local-plugins/corfu-terminal"
	:config
	(unless (display-graphic-p)
		(corfu-terminal-mode +1)))

;; Enable Vertico.
(use-package vertico
	:ensure t
	:config
	(vertico-mouse-mode)
	:custom
	(vertico-scroll-margin 0) ;; Different scroll margin
	(vertico-count 20) ;; Show more candidates
	(vertico-resize t) ;; Grow and shrink the Vertico minibuffer
	(vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
	:init
	(vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:ensure t
	:init
	(savehist-mode))

(use-package orderless
	:ensure t
	:custom
	(completion-styles '(orderless basic))
	(completion-category-defaults nil)
	(completion-category-overrides '((file (styles . (partial-completion))))))

;; Marginalia
(use-package marginalia
	:ensure t
	:init
	(marginalia-mode))

;; Nerd icons for minibuffer
(use-package nerd-icons-completion
	:ensure t
	:after marginalia
	:config
	(nerd-icons-completion-mode)
	(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package embark
	:ensure t
	:init
	(setq prefix-help-command #'embark-prefix-help-command)
	:config
	(add-to-list 'display-buffer-alist
							 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
								 nil
								 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
	:ensure t
	:hook
	(embark-collect-mode . consult-preview-at-point-mode))

(provide 'com-completion)

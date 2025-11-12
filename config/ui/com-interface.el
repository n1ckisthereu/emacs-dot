; Doom Modeline
(use-package doom-modeline
	:custom
	(doom-modeline-indent-info t)
	(doom-modeline-height 25)
	(doom-modeline-bar-width 4)
	:ensure t
	:hook (after-init . doom-modeline-mode))

;; Centaur tabs
(use-package centaur-tabs
	:ensure t
	;; :demand
	:bind
	("C-S-h" . centaur-tabs-backward)
	("C-S-l" . centaur-tabs-forward)
	:config
	(centaur-tabs-mode t)
	:custom
	(centaur-tabs-style "bar")
	(centaur-tabs-height 28)
	(centaur-tabs-set-icons t)
	(centaur-tabs-icon-type 'nerd-icons)
	(centaur-tabs-cycle-scope 'tabs)
	(centaur-tabs-show-navigation-buttons t)
	:hook (dashboard-mode . centaur-tabs-local-mode))

;; Treemacs
(use-package treemacs
	:ensure t
	:config
	(treemacs-indent-guide-mode t)
	:custom
	(treemacs-follow-after-init t)
	(treemacs-follow-mode 1)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode 'always)
	(with-eval-after-load 'treemacs
		(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
		(define-key treemacs-mode-map [mouse-1] #'treemacs-RET-action)))

(use-package treemacs-evil
	:after evil
	:ensure t)

(use-package treemacs-nerd-icons
  :after (treemacs)
	;; :functions treemacs-load-theme
  ;; :preface
  ;; (defun treemacs--propagate-new-icons (_theme))
  ;; :custom-face (cfrs-border-color ((t (:inherit posframe-border))))
	:config
	(treemacs-load-theme "nerd-icons")
	;; (treemacs-nerd-icons-config)
	:ensure t)

(use-package treemacs-projectile
	:ensure t
	:after (treemacs projectile)
	:config
		(treemacs-project-follow-mode 1)
		(add-hook 'projectile-after-switch-project-hook
							#'treemacs-display-current-project-exclusively))

(use-package nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline)
	:demand t
  :ensure t
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font Mono"))

;; (use-package svg-lib
;; 	:ensure t)

(use-package nerd-icons-completion
  :demand t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (after-init . nerd-icons-completion-mode))

(use-package whitespace
		:custom
		(whitespace-style '(face tabs spaces trailing space-mark tab-mark))
		(whitespace-display-mappings '((space-mark ?\	 [?·])
			(tab-mark ?\t [?→ ?\t])))
		:ensure nil)

(provide 'com-interface)

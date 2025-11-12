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
	;; (projectile-require-project-root nil)
	:bind-keymap
	("C-c p" . projectile-command-map))

(provide 'com-projects)

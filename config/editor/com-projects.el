;; Projectile
(use-package projectile
	:ensure t
	:diminish projectile-mode
	:config
	(projectile-mode +1)
	:custom
	(projectile-completion-system 'auto)
	(projectile-project-search-path '("/home/nick/.config/emacs" "/home/nick/Documents/Notes"))
	(projectile-enable-caching t)
	;; current directory as the project root like neovim plugins
	;; (projectile-require-project-root nil)
	)

(provide 'com-projects)

(use-package doom-themes
	:ensure t
	:custom
	;; Global settings (defaults)
	(doom-themes-treemacs-enable-variable-pitch nil)
	(doom-themes-enable-bold t)		; if nil, bold is universally disabled
	(doom-themes-enable-italic t) ; if nil, italics is universally disabled
	;; for treemacs users
	(doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
	:config
	(load-theme 'doom-gruvbox :noconfirm)
	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)
	;; Enable custom neotree theme (nerd-icons must be installed!)
	;; (doom-themes-neotree-config)
	;; or for treemacs users
	(doom-themes-treemacs-config)
	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))
	;; (custom-set-faces
  ;;  `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
  ;;  `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))

(provide 'com-theme)

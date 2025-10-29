;; Configurations for graphic mode
(when (display-graphic-p)
	(tool-bar-mode 0)
	(scroll-bar-mode 0)
	(set-face-attribute 'default nil
		:font "JetBrainsMono Nerd Font Mono"
		:height 110))

;; Enable mouse on terminal
(unless (display-graphic-p)
	(xterm-mouse-mode t))

;; Remove initial tutorial screen and others
(setq inhibit-startup-screen t
	inhibit-startup-message t
	initial-scratch-message nil
	frame-resize-pixelwise t)

;; Show relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(dolist (mode '(eshell-mode-hook dired-mode-hook treemacs-mode-hook lsp-ui-imenu-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Enable current line highlithing
(global-hl-line-mode t)

;; Show current line number
(column-number-mode t)

;; Simplify message yes or no p
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Enable which key
(which-key-mode)

;; Define tab width
(setq-default tab-width 2)

(provide 'com-editor-ui)

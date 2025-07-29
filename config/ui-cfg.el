; Define my colorscheme
(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t)
  (catppuccin-set-color 'base "#0e1119")
  (catppuccin-reload)
  :custom
  (catppuccin-flavor 'mocha)

  :ensure t
  )

; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

; Tabs
(use-package centaur-tabs
  :bind
  ("C-S-h" . centaur-tabs-backward)
  ("C-S-l" . centaur-tabs-forward)
  :config
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  (centaur-tabs-height 25)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-show-new-tab-button nil)
  :demand
  :ensure t
  :hook (dashboard-mode . centaur-tabs-local-mode))

;; Widgets functions
;; (you can use them as template for more widgets)
(which-key-mode)
;; (which-key-setup-side-window-right-bottom)

(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (set-face-attribute 'default nil
    :font "JetBrainsMono Nerd Font Mono"
    :height 110))

(unless (display-graphic-p)
  (xterm-mouse-mode t)
)

; Remove initial tutorial screen and others
(setq inhibit-startup-screen t
  inhibit-startup-message t
  initial-scratch-message nil
  frame-resize-pixelwise t
)

; Show relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

; Enable current line highlithing
(global-hl-line-mode t)

(column-number-mode t)

; Simplify message yes or no p
(fset 'yes-or-no-p 'y-or-n-p)

; Disable cursor blinking
(blink-cursor-mode 0)

(provide 'ui-cfg)

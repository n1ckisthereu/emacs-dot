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
(which-key-setup-side-window-right-bottom)

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

; Dashboard configuration
(use-package dashboard
  :ensure t
  :after general
  :hook (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "E M A C S")
  (dashboard-startup-banner (if (display-graphic-p) 'logo 4))
  (dashboard-page-separator "\n")
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-item-generators
   '((recents . dashboard-insert-recents-shortmenu)
     (bookmarks . dashboard-insert-bookmark-shortmenu)
     (projects . dashboard-insert-project-shortmenu)
     (agenda . dashboard-insert-org-agenda-shortmenu)))
  (dashboard-items '(projects
                     agenda
                     bookmarks
                     recents))
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-newline
     dashboard-insert-newline
     dashboard-insert-homepage-footer))
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (dashboard-setup-startup-hook))

;; Widgets functions
;; (you can use them as template for more widgets)
(defun dashboard-insert-homepage-footer ()
  (widget-create 'item
                 :tag (nerd-icons-faicon "nf-fa-github_alt" :face 'success)
                 :action (lambda (&rest _) (browse-url "https://github.com/n1ckisthereu"))
                 :mouse-face 'highlight
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]")
  (dashboard-center-text (- (point) 1) (point))
  (insert "\n"))

(defun dashboard-insert-project-shortmenu (&rest _)
  (let* ((fn #'project-switch-project)
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'projects dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    ;; (insert (format "%-3s" icon))
    (insert (format "%-s   " icon))
    (widget-create 'item
                   :tag (format "%-30s" "Open project")
                   :action (lambda (&rest _) (call-interactively #'project-switch-project))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (insert (propertize (substitute-command-keys fn-keymap)
                        'face
                        'font-lock-constant-face))))

(defun dashboard-insert-org-agenda-shortmenu (&rest _)
  (let* ((fn #'org-agenda)
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'agenda dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    ;; (insert (format "%-3s" icon))
    (insert (format "%-s   " icon))
    (widget-create 'item
                   :tag (format "%-30s" "Open org-agenda")
                   :action (lambda (&rest _) (call-interactively #'org-agenda))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (insert (propertize (substitute-command-keys fn-keymap)
                        'face
                        'font-lock-constant-face))))

(defun dashboard-insert-bookmark-shortmenu (&rest _)
  (let* ((fn #'bookmark-jump)
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'bookmarks dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    (insert (format "%-s   " icon))
    ;; (insert (format "%-3s" icon))
    (widget-create 'item
                   :tag (format "%-30s" "Jump to bookmark")
                   :action (lambda (&rest _) (call-interactively #'bookmark-jump))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (insert (propertize (substitute-command-keys fn-keymap)
                        'face
                        'font-lock-constant-face))))

(defun dashboard-insert-recents-shortmenu (&rest _)
  (let* ((fn #'recentf)
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'recents dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    (insert (format "%-s   " icon))
    ;; (insert (format "%-3s" icon))
    (widget-create 'item
                   :tag (format "%-30s" "Recently opened files")
                   :action (lambda (&rest _) (call-interactively #'recentf))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (insert (propertize (substitute-command-keys fn-keymap)
                        'face
                        'font-lock-constant-face))))
(provide 'ui-cfg)

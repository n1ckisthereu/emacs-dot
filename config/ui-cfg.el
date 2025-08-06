;; Colorscheme
(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t)
  (catppuccin-set-color 'base "#0e1119")
  (catppuccin-reload)
  :custom
  (catppuccin-flavor 'mocha)
  :ensure t)

; Doom Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Treemacs
(use-package treemacs
  :ensure t
  :custom
  (treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode t)
  (treemacs-indent-guide-mode t)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-RET-action)))

(use-package treemacs-evil
	:after evil
	:ensure t)

(use-package treemacs-nerd-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons")
  :ensure t)

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :config
    (treemacs-project-follow-mode 1)
    (add-hook 'projectile-after-switch-project-hook
              #'treemacs-display-current-project-exclusively))

;; Centaur tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :bind
  ("C-S-h" . centaur-tabs-backward)
  ("C-S-l" . centaur-tabs-forward)
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 15)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-show-navigation-buttons t)
  :hook (dashboard-mode . centaur-tabs-local-mode))

;; Dashboard configuration
(use-package dashboard
  :ensure t
  :after general
  :init
  (dashboard-setup-startup-hook)
  :bind
    (:map dashboard-mode-map
        ("<remap> <dashboard-previous-line>" . widget-backward)
        ("<remap> <dashboard-next-line>" . widget-forward)
        ("<remap> <previous-line>" . widget-backward)
        ("<remap> <next-line>"  . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>"  . widget-backward))
  :custom
  ;; (initial-buffer-choice 'dashboard-open)
  (dashboard-banner-logo-title "Welcome back!")
  ;; (dashboard-startup-banner (if (display-graphic-p) 'logo 2))
  (dashboard-startup-banner
   (if (display-graphic-p)(expand-file-name "dogs.png" user-emacs-directory) 2))
  (dashboard-page-separator "\n")
  (dashboard-icon-type 'nerd-icons)
  (dashboard-navigation-cycle t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-vertically-center-content t)
  (dashboard-center-content t)
  (dashboard-item-generators
   '((recents . dashboard-insert-recents-shortmenu)
     (bookmarks . dashboard-insert-bookmark-shortmenu)
     (projects . dashboard-insert-project-shortmenu)
     (agenda . dashboard-insert-org-agenda-shortmenu)))
  (dashboard-items '(recents
                     projects
                     agenda
                     bookmarks))
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
     dashboard-insert-homepage-footer)))

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

(provide 'ui-cfg)

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
	("<remap> <next-line>"	. widget-forward)
	("<remap> <right-char>" . widget-forward)
	("<remap> <left-char>"	. widget-backward))
	:custom
	;; (initial-buffer-choice 'dashboard-open)
	(dashboard-banner-logo-title "\nWelcome to the revolution!")
	(dashboard-image-banner-max-height 500)
	;; (dashboard-startup-banner (if (display-graphic-p) 'logo 2))
	(dashboard-startup-banner
	 (if (display-graphic-p)(locate-user-emacs-file "main.png") 2))
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
		(insert (format "%-s	 " icon))
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
		(insert (format "%-s	 " icon))
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
		(insert (format "%-s	 " icon))
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
		(insert (format "%-s	 " icon))
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

(provide 'com-dashboard)

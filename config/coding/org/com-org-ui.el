(use-package org-modern
	:ensure t
  :after org
	:custom
	(org-modern-star 'star)
	(org-modern)
	(org-modern-replace-stars "αβχδε")
  :hook (org-mode . (lambda () (when (display-graphic-p) (org-modern-mode))))
  :hook (org-agenda-finalize . org-modern-agenda))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . (lambda () (unless (display-graphic-p) (org-bullets-mode)))))

(add-hook 'org-mode-hook (lambda ()
                           (interactive)
                           (electric-indent-local-mode -1)))

(use-package org-modern-indent
  :load-path "~/.config/emacs/emacs-local-plugins/org-modern-indent"
  :config  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-rainbow-tags
  :ensure t
  :custom
  (org-rainbow-tags-hash-start-index 10)
  (org-rainbow-tags-extra-face-attributes
   ;; Default is '(:weight 'bold)
   '(:inverse-video t :box t :weight 'bold))
  :hook
  ((org-mode org-agenda-finalize) . org-rainbow-tags-mode))

(use-package olivetti
	:ensure t
	:config
	:hook (olivetti-mode-on-hook . (lambda () (olivetti-set-width 100))))

(use-package imenu-list
  :ensure t
  :bind (("C-'". imenu-list-smart-toggle))
  :custom
  (imenu-list-position 'right)
  (imenu-list-size 30))

(provide 'com-org-ui)

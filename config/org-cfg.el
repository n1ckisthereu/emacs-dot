(use-package org
	:ensure-system-package ("/home/nick/.config/emacs/pkgmanager/python/bin/python" . "/home/nick/.config/emacs/scripts/linux/python-libs.sh")
	:ensure t
	:hook (org-mode .  org-indent-mode)
	:hook (org-mode .  (lambda ()
											 (setq-local fill-column 80)
											 (display-fill-column-indicator-mode 1)))
	:config
	(setq python-shell-interpreter "/home/nick/.config/emacs/pkgmanager/python/bin/python")
	(setq org-ellipsis												 " ⤵"
				;; org-redisplay-inline-images					 t
				;; org-display-inline-images						 t
				;; org-startup-with-inline-images			 "inlineimages"
				;; org-image-actual-width							 300
				org-pretty-entities										 t
				org-fontify-whole-heading-line				 t
				org-fontify-done-headline							 t
				org-fontify-quote-and-verse-blocks		  t
				;; org-src-preserve-indentation					 t
				))

(use-package evil-org
	:ensure t
	:after org
	:hook (org-mode . evil-org-mode)
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

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

(use-package htmlize
	:ensure t
	:commands htmlize-buffer)

;; Modify behavior according to evil mode
(with-eval-after-load 'evil
  (defvar-local my/org-modern-was-enabled nil)

  (defun my/org-modern-save-and-disable ()
    (when (derived-mode-p 'org-mode)
      (setq my/org-modern-was-enabled (bound-and-true-p org-modern-mode))
      (when my/org-modern-was-enabled
        (org-modern-mode -1))))

  (defun my/org-modern-restore-after-insert ()
    (when (derived-mode-p 'org-mode)
      (when my/org-modern-was-enabled
        (org-modern-mode 1)
        (setq my/org-modern-was-enabled nil))))

  (add-hook 'evil-insert-state-entry-hook #'my/org-modern-save-and-disable)
  (add-hook 'evil-insert-state-exit-hook  #'my/org-modern-restore-after-insert))

(use-package olivetti
	:ensure t
	:config
	(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 100))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename default-dir-notes))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
	(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
	(org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package websocket
  :ensure t)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
	(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package imenu-list
  :ensure t
  :bind (("C-'". imenu-list-smart-toggle))
  :custom
  (imenu-list-position 'right)
  (imenu-list-size 30))

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   ))

(provide 'org-cfg)

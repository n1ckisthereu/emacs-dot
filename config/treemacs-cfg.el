(use-package treemacs
  :ensure t
  :config
  (treemacs-indent-guide-mode t)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-RET-action))
  :hook(treemacs-mode . (lambda () (display-line-numbers-mode 0)))  )

(use-package treemacs-nerd-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons")
  :ensure t
)

(provide 'treemacs-cfg)

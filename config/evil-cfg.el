(defun vim-dec-inc-delta (delta)
  (interactive "p")
  (skip-chars-backward "0-9-")
  (if (looking-at "-?[0-9]+")
      (let* ((old (match-string 0))
             (new (number-to-string (+ (string-to-number old) delta))))
        (replace-match new t)   ;; substitui e move para depois do número
        (backward-char 1))      ;; volta 1 caractere para ficar no último dígito
    (error "No number at point")))

(global-set-key (kbd "C-a") (lambda () (interactive) (vim-dec-inc-delta 1)))
(global-set-key (kbd "C-S-x") (lambda () (interactive) (vim-dec-inc-delta -1)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :ensure t
  :config
  (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))


(provide 'evil-cfg)

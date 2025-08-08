;; Remove white space from end of file
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Remove two spaces from end of sentence
(setq-default sentence-end-double-space nil)

;; Improve navigation between words
(global-subword-mode 1)

;; Default emacs lisp
;; (setq-default initial-major-mode 'emacs-lisp-mode)

;; Remove indent tabs
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; Move to trash
(setq delete-by-moving-to-trash t)

;; Remove initial message
(setq-default initial-scratch-message nil)

;; Increase undo limit
(setq undo-limit 100000000
      auto-save-default t)

;; Take new window space from all other windows
(setq window-combination-resize t)

;; Visual feedback
;; (setq visible-bell t)

;; Fill all real char
(setq x-stretch-cursor t)

;; ellipsis on truncated text
(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "…"))

;; Improve title - actually doesn't work
;; (setq frame-title-format
;;       '(""
;;         "%b"
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p) " ◉ %s" "  ●  %s - Emacs") project-name))))))

; Scroll step
(setq scroll-step            1
      scroll-conservatively  10000)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Not create bkp/lockfiles
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Remove subtitle legend on press one key
(setq echo-keystrokes 0)

;;; Evil configuration
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

;; Evil configuration
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :ensure t
  :config
  (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t))

(use-package evil-collection
  :ensure t
  :after evil treemacs
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))

(use-package evil-terminal-cursor-changer
  :ensure t
  :unless (display-graphic-p)
  :after evil
  :config
  (evil-terminal-cursor-changer-activate))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Enable Vertico.
(use-package vertico
  :ensure t
  :config
  (vertico-mouse-mode)
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  ;; :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

;; Marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Nerd icons for minibuffer
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :ensure t)

(use-package consult-lsp
  :defer t
  :after lsp
  :ensure t)

(use-package embark
  :ensure t
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

; Make clipboard works on wayland terminal
(when (executable-find "wl-copy")
  (defun wl-copy (text)
    (let ((p (make-process :name "wl-copy" :command '("wl-copy") :connection-type 'pipe)))
      (process-send-string p text)
      (process-send-eof p)))
  (setq interprogram-cut-function 'wl-copy))

(when (executable-find "wl-paste")
  (defun wl-paste ()
    (shell-command-to-string "wl-paste"))
  (setq interprogram-paste-function 'wl-paste))

(provide 'behavior-cfg)

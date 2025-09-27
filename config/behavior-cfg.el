;; Remove white space from end of file
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Remove two spaces from end of sentence
(setq-default sentence-end-double-space nil)

(global-subword-mode 1)

;; Move to trash
(setq delete-by-moving-to-trash t)

;; Improve emacs perfomance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max 1048576)

(setq-default initial-scratch-message nil)

;; Increase undo limit
(setq undo-limit 100000000
			auto-save-default t)

;; Take new window space from all other windows
(setq window-combination-resize t)

;; Fill all real char
(setq x-stretch-cursor t)

;; ellipsis on truncated text
(with-eval-after-load 'mule-util
	(setq truncate-string-ellipsis " ⤵"))

; Scroll step
(setq scroll-step						 1
			scroll-conservatively	 10000)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Not create bkp/lockfiles
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Remove subtitle legend on press one key
(setq echo-keystrokes 0)

;; Set utf enconding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;;; Evil configuration
(defun vim-dec-inc-delta (delta)
	(interactive "p")
	(skip-chars-backward "0-9-")
	(if (looking-at "-?[0-9]+")
			(let* ((old (match-string 0))
						 (new (number-to-string (+ (string-to-number old) delta))))
				(replace-match new t)		;; substitui e move para depois do número
				(backward-char 1))			;; volta 1 caractere para ficar no último dígito
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
	(evil-want-C-u-scroll t)
	(evil-kill-on-visual-paste nil))

(use-package evil-collection
	:ensure t
	:after evil treemacs
	:config
	(evil-collection-init))

(use-package evil-goggles
	:ensure t
	:custom
	(evil-goggles-use-diff-faces)
	:config
	(evil-goggles-mode))

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
	(vertico-count 20) ;; Show more candidates
	(vertico-resize t) ;; Grow and shrink the Vertico minibuffer
	(vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
	:init
	(vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:ensure t
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
	(setq prefix-help-command #'embark-prefix-help-command)
	:config
	(add-to-list 'display-buffer-alist
							 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
								 nil
								 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
	:ensure t
	:hook
	(embark-collect-mode . consult-preview-at-point-mode))

;; Disable Tooltips
(setq x-gtk-use-system-tooltips nil)
(setq use-system-tooltips nil)

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

;;; behavior-cfg.el ends here

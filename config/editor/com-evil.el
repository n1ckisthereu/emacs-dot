;; Evil (+) (-)
(defun vim-dec-inc-delta (delta)
	(interactive "p")
	(skip-chars-backward "0-9-")
	(if (looking-at "-?[0-9]+")
			(let* ((old (match-string 0))
						 (new (number-to-string (+ (string-to-number old) delta))))
				(replace-match new t)
				(backward-char 1))
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

;; Evil lib collection
(use-package evil-collection
	:ensure t
	:after evil treemacs
	:config
	(evil-collection-init))

;; Evil goggles for visual feedback on edits
(use-package evil-goggles
	:ensure t
	:custom
	(evil-goggles-use-diff-faces)
	:config
	(evil-goggles-mode))

;; For comments
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

(provide 'com-evil)

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

;; Hide and show code and comments
(add-hook 'prog-mode-hook #'hs-minor-mode)

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

(provide 'com-behavior)

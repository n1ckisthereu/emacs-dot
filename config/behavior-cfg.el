; Remove white space from end of file
(add-hook 'before-save-hook #'whitespace-cleanup)

; Remove two spaces from end of sentence
(setq-default sentence-end-double-space nil)

; Improve navigation between words
(global-subword-mode 1)

; Default emacs lisp
;; (setq-default initial-major-mode 'emacs-lisp-mode)

; Remove indent tabs
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

; Move to trash
(setq delete-by-moving-to-trash t)

; Remove initial message
(setq-default initial-scratch-message nil)

; Increase undo limit
(setq undo-limit 100000000
      auto-save-default t)

; Take new window space from all other windows
(setq window-combination-resize t)

; Visual feedback
;; (setq visible-bell t)

; Fill all real char
(setq x-stretch-cursor t)

; ellipsis on truncated text
(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "…"))

; Improve title - actually doesn't work
;; (setq frame-title-format
;;       '(""
;;         "%b"
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p) " ◉ %s" "  ●  %s - Emacs") project-name))))))

(setq scroll-step            1
      scroll-conservatively  10000)

(setq create-lockfiles nil)
(setq make-backup-files nil)

; Remove subtitle legend on press one key
(setq echo-keystrokes 0)

(provide 'behavior-cfg)

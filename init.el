(add-to-list 'load-path (locate-user-emacs-file "config/"))

(setq custom-file (locate-user-emacs-file "custom.el"))

(setq backup-directory-alist '((user-emacs-directory . "backups")))

;(cua-mode t)

(require 'melpa-cfg)
(require 'ui-cfg)
(require 'behavior-cfg)
(require 'evil-cfg)
(require 'treemacs-cfg)
(require 'tools-cfg)
(require 'keybinds-cfg)

(provide 'init)

; Load my configuration directory
(add-to-list 'load-path (locate-user-emacs-file "config/"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq backup-directory-alist '((user-emacs-directory . "backups")))

(require 'melpa-cfg)
(require 'ui-cfg)
(require 'behavior-cfg)
(require 'keybinds-cfg)
(require 'code-cfg)

(provide 'init)

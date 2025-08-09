;;; Load my configuration from the directory config
(add-to-list 'load-path (locate-user-emacs-file "config/"))

; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  (custom-file (locate-user-emacs-file "custom.el")))

(require 'melpa-cfg)
(require 'ui-cfg)
(require 'behavior-cfg)
(require 'keybinds-cfg)
(require 'code-cfg)

(provide 'init)

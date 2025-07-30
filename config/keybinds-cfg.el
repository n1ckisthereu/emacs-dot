(use-package general
  :ensure t)

(general-create-definer my/leader-key-def
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(my/leader-key-def
  ;; Arquivos
  "f"   '(:ignore t :which-key "files")
  "ff"  '(find-file :which-key "find file")
  "fs"  '(save-buffer :which-key "save file")

  ;; Buffers
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(switch-to-buffer :which-key "switch buffer")
  "bd"  '(kill-current-buffer :which-key "kill buffer")
  "ba"  '(centaur-tabs-kill-other-buffers-in-current-group :which-key "kill other buffers")
  "bc"  '(centaur-tabs-counsel-switch-group :which-key "change tab group")

  ;; Projetos
  "p"   '(:ignore t :which-key "projects")
  "pp"  '(projectile-switch-project :which-key "switch project")
  "pf"  '(projectile-find-file :which-key "find file in project")

  ;; Quit
  "q"   '(:ignore t :which-key "quit")
  "qq"  '(save-buffers-kill-emacs :which-key "quit Emacs")

  ;; Configuração
  ;; "c"   '(:ignore t :which-key "config")
  ;; "cf"  '(find-file-user-init-file :which-key "open init.el")

  ;; Agenda
  "a"   '(:ignore t :which-key "agenda" )
  "aa"  '(org-agenda :which-key "org-agenda" ov)

  ;; Outros
  "h"   '(:ignore t :which-key "help")
  "hf"  '(describe-function :which-key "describe function")
  "hv"  '(describe-variable :which-key "describe variable")
  "ha"  '(apropos :which-key "apropos")

  ;; Treemacs
  "t"   '(:ignore t :which-key "treemacs")
  "tt"  '(treemacs :which-key "toggle treemacs")

  ;; flycheck
  "e"   '(:ignore t :which-key "errors")
  "el"  '(flycheck-list-errors :which-key "list errors")
  "en"  '(flycheck-next-error :which-key "next error")
  "ep"  '(flycheck-previous-error :which-key "previous error")
  "ev"  '(flycheck-verify-setup :which-key "verify setup")
  "eb"  '(flycheck-buffer :which-key "check buffer")

  ;; lsp
  "l"   '(:ignore t :which-key "lsp")
  "la"  '(lsp-execute-code-action :which-key "code action")
  "lA"  '(lsp-ui-sideline-apply-code-actions :which-key "apply sideline action")

  ; Windows management
  "w"   '(:ignore t :which-key "windows")
  "wh"  '(split-window-below :which-key "split horizontal")
  "wv"  '(split-window-right :which-key "split vertical")
  "wd"  '(delete-window :which-key "delete window")
  "wo"  '(delete-other-windows :which-key "delete other windows")

  )

  ;; TODO: Implement keys to treemacs

(general-define-key
 :keymaps 'override
 "C-h" 'windmove-left
 "C-l" 'windmove-right
 "C-j" 'windmove-down
 "C-k" 'windmove-up)


(provide 'keybinds-cfg)

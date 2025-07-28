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
  "bd"  '(kill-this-buffer :which-key "kill buffer")

  ;; Projetos
  "p"   '(:ignore t :which-key "projects")
  "pp"  '(projectile-switch-project :which-key "switch project")
  "pf"  '(projectile-find-file :which-key "find file in project")

  ;; Quit
  "q"   '(:ignore t :which-key "quit")
  "qq"  '(save-buffers-kill-emacs :which-key "quit Emacs")

  ;; Configuração
  "c"   '(:ignore t :which-key "config")
  "cf"  '(find-file-user-init-file :which-key "open init.el")

  ;; Agenda
  "a"   '(:ignore t :which-key "agenda" )
  "aa"  '(org-agenda :which-key "org-agenda" ov)

  ;; Outros
  "h"   '(:ignore t :which-key "help")
  "hf"  '(describe-function :which-key "describe function")
  "hv"  '(describe-variable :which-key "describe variable"))

(provide 'keybinds-cfg)

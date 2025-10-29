(use-package breadcrumb
	:load-path "~/.config/emacs/emacs-local-plugins/breadcrumb"
	:hook
  (prog-mode . breadcrumb-local-mode)
  :custom
  ;; Add nerd-icons to breadcrumb
  (breadcrumb-imenu-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-project-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-imenu-max-length 0.5)
  (breadcrumb-project-max-length 0.5)
  :preface
  ;; Add icons to breadcrumb
  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-icon-for-file string)
                              " " string)
                    (concat (nerd-icons-faicon
                             "nf-fa-folder_open"
                             :face 'breadcrumb-project-crumbs-face)
                            " "
                            string)))))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-codicon
                               "nf-cod-symbol_field"
                               :face 'breadcrumb-imenu-leaf-face)
                              " " string)
                    (cond ((string= string "Packages")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Requires")
                           (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Method")
                           (concat (nerd-icons-codicon "nf-cod-symbol_method" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Class")
                           (concat (nerd-icons-codicon "nf-cod-symbol_class" :face 'breadcrumb-imenu-crumbs-face) " " string))
													((string= string "Field")
                           (concat (nerd-icons-codicon "nf-cod-symbol_field" :face 'breadcrumb-imenu-crumbs-face) " " string))
													((string= string "Module")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((or (string= string "Variable") (string= string "Variables"))
                           (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Function")
                           (concat (nerd-icons-codicon "nf-cod-symbol_field" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          (t string)))))))

(provide 'com-breadcrumb)

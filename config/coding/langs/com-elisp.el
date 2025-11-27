(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook (lambda () (indent-region (point-min) (point-max))) nil t)
            ))


(provide 'com-elisp)

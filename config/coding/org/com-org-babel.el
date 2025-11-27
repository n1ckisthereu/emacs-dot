;; (cl-defmacro lsp-org-babel-enable (lang)
;;   "Support LANG in org source code block."
;;   (setq centaur-lsp 'lsp-mode)
;;   (cl-check-type lang string)
;;   (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
;;          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
;;     `(progn
;;        (defun ,intern-pre (info)
;;          (let ((file-name (->> info caddr (alist-get :file))))
;;            (unless file-name
;;              (setq file-name (make-temp-file "babel-lsp-")))
;;            (setq buffer-file-name file-name)
;;            (lsp-deferred)))
;;        (put ',intern-pre 'function-documentation
;;             (format "Enable lsp-mode in the buffer of org source block (%s)."
;;                     (upcase ,lang)))
;;        (if (fboundp ',edit-pre)
;;            (advice-add ',edit-pre :after ',intern-pre)
;;          (progn
;;            (defun ,edit-pre (info)
;;              (,intern-pre info))
;;            (put ',edit-pre 'function-documentation
;;                 (format "Prepare local buffer environment for org source block (%s)."
;;                         (upcase ,lang))))))))

;; (defvar org-babel-lang-list
;;   '("python" "bash" "sh"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(lsp-org-babel-enable ,lang)))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (shell . t)
;;    ))

(provide 'com-org-babel)

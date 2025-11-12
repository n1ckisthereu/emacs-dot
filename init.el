;; (defun load-modules (path)
;;   (dolist (file (directory-files-recursively path "^com-.*\\.el$"))
;;     (let* ((dir  (file-name-directory file))
;;            (base (file-name-base file))
;;            (feat (intern base)))
;;       (add-to-list 'load-path dir)
;;       (condition-case err
;;           (progn
;;             (require feat)
;;             (message "loading module: %s" feat))
;;         (error
;;          (message "❌ error on load %s: %s" feat err))))))

;; (load-modules (locate-user-emacs-file "config/"))

(defun load-modules (path)
  (let* ((files (directory-files-recursively path "^com-.*\\.el$"))
         (total (length files))
         (count 0))
    (add-to-list 'load-path (locate-user-emacs-file "config/"))
    (require 'com-melpa)
    (dolist (file files)
      (let* ((dir  (file-name-directory file))
             (base (file-name-base file))
             (feat (intern base)))
        (add-to-list 'load-path dir)
        (setq count (1+ count))
        (let ((progress (format "[%d/%d] loading %s..."
                                count total feat)))
          (message "%s" progress))
        (condition-case err
            (require feat)
          (error
           (message "❌ error on %s: %s" feat err))))
      ))
  (message "✅ All modules loaded!"))

(load-modules (locate-user-emacs-file "config/"))

(provide 'init)


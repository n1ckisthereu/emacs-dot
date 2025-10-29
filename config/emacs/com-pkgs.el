(require 'url)
;; =============================
;; ===== pkgs utils =====
;; =============================

(defun com-get-os ()
	"Return the current so."
	(cond
	 ((eq system-type 'gnu/linux) "linux")
	 ((or (eq system-type 'darwin)
				(eq system-type 'macos)) "macos")
	 ((memq system-type '(windows-nt ms-dos cygwin)) "windows")
	 (t "unknown")))

(defun com-check-python()
	"Return the first python interpreter that has venv support.
Signal an error if none is found."
	(let ((interpreters '("python3" "python"))
				python-exists)
		(dolist (py interpreters python-exists)
			(when (string= "True"
										 (string-trim
											(shell-command-to-string
											 (format "%s -c \"import importlib.util; print(importlib.util.find_spec('venv') is not None)\""
															 py))))
				(setq python-exists py)))
		(unless python-exists
			(error "❌ No Python interpreter with venv module found."))
		python-exists))

(defun download-file (&optional url download-dir download-name)
  "Download a file from URL and save it to DOWNLOAD-DIR with DOWNLOAD-NAME.
Handles binary data safely."
  (interactive)
  (let* ((url (or url (read-string "Enter download URL: ")))
         (download-dir (or download-dir "~/downloads/"))
         (download-name (or download-name (car (last (split-string url "/" t)))))
         (output-path (expand-file-name download-name download-dir)))
    (message "Downloading %s..." url)
    (let ((buffer (url-retrieve-synchronously url t t)))
      (when buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (let ((data (buffer-substring-no-properties (point) (point-max))))
            (with-temp-file output-path
              (set-buffer-multibyte nil)
              (insert data)))))
      (kill-buffer buffer)
      (message "Download complete: %s" output-path))))

;; =======================
;; ===== python pkgs =====
;; =======================

(defun com-pkg-python ()
	"Install or update lsp python server in the local environment"
	(interactive)
	(let* ((env-dir (expand-file-name "com-pkgs/python" user-emacs-directory))
				 (pip-bin (expand-file-name "bin/pip" env-dir))
				 (python-exists (com-check-python)))

		(if (file-directory-p env-dir)
				(progn
					(message "📦 Update the packages at %s" env-dir)
					(shell-command (format "%s install --upgrade 'python-lsp-server[all]' mypy" pip-bin)))
			(progn
				(message "🌱 Creating new virtual environment at %s" env-dir)
				(shell-command (format "%s -m venv %s" python-exists env-dir))
				(shell-command (format "%s install --upgrade pip" pip-bin))
				(shell-command (format "%s install 'python-lsp-server[all]' mypy" pip-bin))
				(message "✅ Python enviroment is finished: %s" env-dir)))))

(defun com-pkg-org-py ()
	"Install a list of libs defined in the file \"com-variables.el\" to use on org"
	(interactive)
	(let* ((env-dir (expand-file-name "com-pkgs/python-libs" user-emacs-directory))
				 (pip-bin (expand-file-name "bin/pip" env-dir))
				 (python-exists (com-check-python)))
	(if (file-directory-p env-dir)
			(progn
				(message "📦 Update the packages at %s" env-dir)
				(shell-command (format "%s install --upgrade %s" pip-bin (mapconcat #'identity com-pkg-org-libs " "))))
		(progn
			(message "🌱 Creating new virtual environment at %s" env-dir)
			(shell-command (format "%s -m venv %s" python-exists env-dir))
			(shell-command (format "%s install --upgrade pip" pip-bin))
			(shell-command (format "%s install	%s" pip-bin (mapconcat #'identity com-pkg-org-libs " ")))
			(shell-command (format "%s install -r %s" pip-bin (mapconcat #'identity " " com-pkg-org-libs)))
			(message "✅ Python enviroment is finished: %s" env-dir)))))

;; ======================
;; ===== pandoc pkg =====
;; ======================

(defun com-pkg-pandoc ()
  "Install or update pandoc and its filters."
  (interactive)
  (let* ((pandoc-dir (expand-file-name "com-pkgs/pandoc/" user-emacs-directory))
         (archive-path (expand-file-name "pandoc.tar.gz" pandoc-dir))
         (pandoc-url "https://github.com/jgm/pandoc/releases/download/3.8.2/pandoc-3.8.2-linux-amd64.tar.gz")
         (temp-dir (expand-file-name "tmp-extract" pandoc-dir)))
    (unless (file-directory-p pandoc-dir)
      (make-directory pandoc-dir t))
    (download-file pandoc-url pandoc-dir "pandoc.tar.gz")
    (when (file-directory-p temp-dir)
      (delete-directory temp-dir t))
    (make-directory temp-dir t)
    (extract-archive archive-path temp-dir)
    (let* ((inner-dir (car (directory-files temp-dir t "^[^.].*"))))
      (when (file-directory-p inner-dir)
        (dolist (f (directory-files inner-dir t "^[^.].*"))
          (rename-file f pandoc-dir t))))
    (delete-file archive-path)
    (delete-directory temp-dir t)))

(defun com-pkg-pdflatex ()
  "Install or update pandoc and its filters."
  (interactive)
  (let* ((texlive-dir (expand-file-name "com-pkgs/pdflatex/" user-emacs-directory))
         (archive-path (expand-file-name "textlive.tar.gz" texlive-dir))
         (texlive-url "https://github.com/TeX-Live/texlive-source/releases/download/svn74917/texlive-bin-x86_64-linux.tar.gz")
         (temp-dir (expand-file-name "tmp-extract" texlive-dir)))
    (unless (file-directory-p texlive-dir)
      (make-directory texlive-dir t))
    (download-file texlive-url texlive-dir "textlive.tar.gz")
    (when (file-directory-p temp-dir)
      (delete-directory temp-dir t))
    (make-directory temp-dir t)
    (extract-archive archive-path temp-dir)
    (let* ((inner-dir (car (directory-files temp-dir t "^[^.].*"))))
      (when (file-directory-p inner-dir)
        (dolist (f (directory-files inner-dir t "^[^.].*"))
          (rename-file f texlive-dir t))))
    (delete-file archive-path)
    (delete-directory temp-dir t)))


(defun com-pkg-7z ()
  "Download and extract 7-Zip portable for the current OS into com-pgs/7z inside `user-emacs-directory`."
  (interactive)
  (let* ((os (com-get-os))
         (base-dir (expand-file-name "com-pkgs/7z" user-emacs-directory))
         (tmp-dir (expand-file-name temporary-file-directory))
         (url nil)
         (archive-path nil)
         (extract-cmd nil))
    (unless (file-directory-p base-dir)
      (make-directory base-dir t))
    (unless (file-directory-p tmp-dir)
      (make-directory tmp-dir t))
    (setq url
          (cond
           ((string= os "linux")
            "https://www.7-zip.org/a/7z2501-linux-x64.tar.xz")
           ((string= os "windows")
            "https://www.7-zip.org/a/7z2501-extra.7z")
					 ((string= os "macos")
						"https://github.com/ip7z/7zip/releases/download/25.01/7z2501-mac.tar.xz")
           (t (error "Unsupported OS: %s" os))))
    (setq archive-path
          (expand-file-name (file-name-nondirectory url) tmp-dir))
    (message "Downloading 7-Zip for %s..." os)
    (download-file url tmp-dir (file-name-nondirectory url))
    (message "Download complete: %s" archive-path)
    (setq extract-cmd
          (cond
           ((string= os "linux")
            (format "tar -xf %s -C %s"
                    (shell-quote-argument archive-path)
                    (shell-quote-argument base-dir)))
           ((string= os "macos")
            (format "tar -xf %s -C %s"
                    (shell-quote-argument archive-path)
                    (shell-quote-argument base-dir)))
           ((string= os "windows")
            (format "powershell -Command \"& {Expand-Archive -Path '%s' -DestinationPath '%s'}\""
                    archive-path base-dir))
           (t (error "No extraction command for %s" os))))
    (message "Extracting 7-Zip...")
    (shell-command extract-cmd)
    (when (member os '("linux" "macos"))
      (shell-command (format "chmod +x %s/7zz" (shell-quote-argument base-dir))))
    (message "7-Zip extracted to: %s" base-dir)
    base-dir))

(defun extract-archive (archive-path destination)
  (interactive "fArchive file: \nDExtract to directory: ")
  (let* ((archive-path (expand-file-name archive-path))
         (destination (expand-file-name destination))
         (sevenzip (or (executable-find "7zz")
                       (expand-file-name "7zz"
                                         (expand-file-name "com-pkgs/7z" user-emacs-directory)))))
    (unless (file-exists-p sevenzip)
      (message "7-Zip not found. Installing portable version...")
      (setq sevenzip (expand-file-name "7zz" (com-pkg-7z))))
    (unless (file-exists-p archive-path)
      (error "Archive not found: %s" archive-path))
    (unless (file-directory-p destination)
      (make-directory destination t))
    (let ((cmd (format "%s x %s -o%s -y"
                       (shell-quote-argument sevenzip)
                       (shell-quote-argument archive-path)
                       (shell-quote-argument destination))))
      (shell-command cmd))
    (when (string-match-p
           (rx "." (or "tar.gz" "tgz" "tar.xz" "tar.bz2" "tar.zst" "tar.lz"))
           archive-path)
      (let ((tar-file (car (directory-files destination t "\\.tar$"))))
        (when (and tar-file (file-exists-p tar-file))
          (let ((cmd (format "%s x %s -o%s -y"
                             (shell-quote-argument sevenzip)
                             (shell-quote-argument tar-file)
                             (shell-quote-argument destination))))
            (shell-command cmd))
          (delete-file tar-file))))))

;; (defun extract-archive (archive-path destination)
;;   "Extract ARCHIVE-PATH to DESTINATION using 7z.
;; If 7z is not installed, call `com-pkg-7z` to install it first."
;;   (interactive "fArchive file: \nDExtract to directory: ")
;;   (let* ((archive-path (expand-file-name archive-path))
;;          (destination (expand-file-name destination))
;;          (sevenzip (or (executable-find "7zz")
;;                        (expand-file-name "7zz" (expand-file-name "com-pkgs/7z" user-emacs-directory)))))
;;     (unless (file-exists-p sevenzip)
;;       (message "7-Zip not found. Installing portable version...")
;;       (setq sevenzip (expand-file-name "7zz" (com-pkg-7z))))
;;     (unless (file-exists-p archive-path)
;;       (error "Archive file not found: %s" archive-path))
;;     (unless (file-directory-p destination)
;;       (make-directory destination t))
;;     (message "Extracting %s to %s..." archive-path destination)
;;     (let ((cmd (format "%s x %s -o%s -y"
;;                        (shell-quote-argument sevenzip)
;;                        (shell-quote-argument archive-path)
;;                        (shell-quote-argument destination))))
;;       (message "Running: %s" cmd)
;;       (shell-command cmd))
;;     (message "Archive extracted successfully to: %s" destination)))

;; (defun com-node-pkgs (action package)
;;   (interactive
;;    (let ((action (completing-read "Action: " '("install" "update" "remove" "list"))))
;;      (list action
;;            (read-string
;;             (if (string= action "update")
;;                 "Package (or 'all' for update all): "
;;               "Package: ")))))
;;   (if (not (executable-find "npm"))
;;       (message "npm not found in PATH")
;;     (let* ((base-dir (expand-file-name "com-pkgs/com-node-pkgs/" user-emacs-directory))
;;            (pkg-dir (expand-file-name package base-dir)))
;;       (unless (file-directory-p base-dir)
;;         (make-directory base-dir t))
;;       (cond
;;        ((string= action "install")
;;         (unless (file-directory-p pkg-dir)
;;           (make-directory pkg-dir t))
;;         (let ((default-directory pkg-dir))
;;           (shell-command (format "npm init -y && npm install %s" package))))
;;        ((string= action "update")
;;         (if (string= package "all")
;;             (let ((dirs (directory-files base-dir t "^[^.]" t)))
;;               (if dirs
;;                   (dolist (dir dirs)
;;                     (when (file-directory-p dir)
;; 											(let* ((default-directory dir)
;;                              (pkg-name (file-name-nondirectory dir)))
;;                         (shell-command (format "npm install %s@latest" pkg-name)))))
;;                 (message "No packages installed")))
;;           (if (file-directory-p pkg-dir)
;;               (let ((default-directory pkg-dir))
;;                 (shell-command (format "npm install %s@latest" package)))
;;             (message "Package not installed"))))
;;        ((string= action "remove")
;;         (if (file-directory-p pkg-dir)
;;             (progn
;;               (delete-directory pkg-dir t)
;;               (message "Package removed"))
;;           (message "Package not found")))
;;        ((string= action "list")
;;         (if (file-directory-p pkg-dir)
;;             (message "Package '%s' installed at: %s" package (file-relative-name pkg-dir user-emacs-directory))
;;           (message "Package '%s' not installed" package)))
;;        (t (message "Invalid action"))))))
;; 	)

;; (defun com-node-pkgs (action package)
;;   "Manage Node.js packages locally via npm.

;; ACTION is one of \"install\", \"update\", \"remove\", \"list\".
;; PACKAGE is the name of the package, or 'all' for updating all packages."
;;   (interactive
;;    (let ((action (completing-read "Action: " '("install" "update" "remove" "list"))))
;;      (list action
;;            (read-string
;;             (if (string= action "update")
;;                 "Package (or 'all' for update all): "
;;               "Package: ")))))
;;   (let ((interactive-call (called-interactively-p 'any)))
;;     (if (not (executable-find "npm"))
;;         (progn
;;           (when interactive-call (message "npm not found in PATH"))
;;           "npm not found in PATH")
;;       (let* ((base-dir (expand-file-name "com-pkgs/com-node-pkgs/" user-emacs-directory))
;;              (pkg-dir (expand-file-name package base-dir)))
;;         (unless (file-directory-p base-dir)
;;           (make-directory base-dir t))
;;         (cond
;;          ((string= action "install")
;;           (unless (file-directory-p pkg-dir)
;;             (make-directory pkg-dir t))
;;           (let ((default-directory pkg-dir))
;;             (shell-command (format "npm init -y && npm install %s" package)))
;;           (let ((result (format "Package '%s' installed at %s" package pkg-dir)))
;;             (when interactive-call (message "%s" result))
;;             result))

;;          ((string= action "update")
;;           (if (string= package "all")
;;               (let ((dirs (directory-files base-dir t "^[^.]" t))
;;                     results)
;;                 (if dirs
;;                     (progn
;;                       (dolist (dir dirs)
;;                         (when (file-directory-p dir)
;;                           (let ((default-directory dir)
;;                                 (pkg-name (file-name-nondirectory dir))
;;                                 res)
;;                             (shell-command (format "npm install %s@latest" pkg-name))
;;                             (setq res (format "Package '%s' updated" pkg-name))
;;                             (push res results)
;;                             (when interactive-call (message "%s" res)))))
;;                       (nreverse results))
;;                   (progn
;;                     (when interactive-call (message "No packages installed"))
;;                     "No packages installed"))
;;                 )
;;             (if (file-directory-p pkg-dir)
;;                 (let ((default-directory pkg-dir)
;;                       result)
;;                   (shell-command (format "npm install %s@latest" package))
;;                   (setq result (format "Package '%s' updated" package))
;;                   (when interactive-call (message "%s" result))
;;                   result)
;;               (progn
;;                 (when interactive-call (message "Package '%s' not installed" package))
;;                 (format "Package '%s' not installed" package)))))

;;          ((string= action "remove")
;;           (if (file-directory-p pkg-dir)
;;               (progn
;;                 (delete-directory pkg-dir t)
;;                 (let ((result (format "Package '%s' removed" package)))
;;                   (when interactive-call (message "%s" result))
;;                   result))
;;             (progn
;;               (when interactive-call (message "Package '%s' not found" package))
;;               (format "Package '%s' not found" package))))

;;          ((string= action "list")
;;           (let ((result (if (file-directory-p pkg-dir)
;;                             (format "Package '%s' installed at: %s"
;;                                     package
;;                                     (file-relative-name pkg-dir user-emacs-directory))
;;                           (format "Package '%s' not installed" package))))
;;             (when interactive-call (message "%s" result))
;;             result))

;;          (t
;;           (when interactive-call (message "Invalid action"))
;;           "Invalid action"))))))

(provide 'com-pkgs)

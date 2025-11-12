(use-package org
	:ensure t
	:hook (org-mode . (lambda () (setq-local org-babel-python-command "/home/nick/.config/emacs/com-pkgs/python-libs/bin/python")))
	:hook (org-mode . (lambda () (setq-local python-shell-exec-path "/home/nick/.config/emacs/com-pkgs/python-libs/bin/python")))
	:hook (org-mode . (lambda () (setq-local python-shell-interpreter "/home/nick/.config/emacs/com-pkgs/python-libs/bin/python")))
	:hook (org-mode .  org-indent-mode)
	:hook (org-mode .  (lambda ()
											 (setq-local fill-column 80)
											 (display-fill-column-indicator-mode 1)))
	:config
	(setq org-ellipsis												 "	 ⤵"
				;;																			 org-redisplay-inline-images					 t
				;;																			 org-display-inline-images						 t
				;;																			 org-startup-with-inline-images			 "inlineimages"
				;;																			 org-image-actual-width							 300
				org-pretty-entities											 t
				org-fontify-whole-heading-line					 t
				org-fontify-done-headline								 t
				org-fontify-quote-and-verse-blocks			 t
				;;																			 org-src-preserve-indentation					 t
				))

(use-package evil-org
	:ensure t
	:after org
	:hook (org-mode . evil-org-mode)
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

;; Modify behavior according to evil mode
;; Enable / disable org rendering
(with-eval-after-load 'evil
  (defvar-local my/org-modern-was-enabled nil)

  (defun my/org-modern-save-and-disable ()
    (when (derived-mode-p 'org-mode)
      (setq my/org-modern-was-enabled (bound-and-true-p org-modern-mode))
      (when my/org-modern-was-enabled
        (org-modern-mode -1))))

  (defun my/org-modern-restore-after-insert ()
    (when (derived-mode-p 'org-mode)
      (when my/org-modern-was-enabled
        (org-modern-mode 1)
        (setq my/org-modern-was-enabled nil))))

  (add-hook 'evil-insert-state-entry-hook #'my/org-modern-save-and-disable)
  (add-hook 'evil-insert-state-exit-hook  #'my/org-modern-restore-after-insert))


(after! org
  (setq +latex-viewers '(skim zathura evince sumatrapdf okular pdf-tools))
  ;; (require 'ox-beamer)
  ;; (setq org-latex-listings 'minted)
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-link-context-for-files t)
	(setq org-latex-classes
				'(("article" "\\documentclass[11pt]{article}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					("report" "\\documentclass[11pt]{report}"
					 ("\\part{%s}" . "\\part*{%s}")
					 ("\\chapter{%s}" . "\\chapter*{%s}")
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
					("book" "\\documentclass[11pt]{book}"
					 ("\\part{%s}" . "\\part*{%s}")
					 ("\\chapter{%s}" . "\\chapter*{%s}")
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
					("beamer" "\\documentclass{beamer}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					("neocampus" "\\documentclass{neocampus}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
					)
))


(provide 'com-org)

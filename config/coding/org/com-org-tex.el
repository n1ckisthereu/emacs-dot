;; (use-package htmlize
;; 	:ensure t
;; 	:commands htmlize-buffer)

;; (setq org-latex-compiler "")
;; (setq org-latex-compiler "/home/nick/.config/emacs/com-pkgs/pdflatex/lualatex")

(setq org-latex-listings 't)

(with-eval-after-load 'ox-latex
	(add-to-list 'org-latex-classes
							 '("org-plain-latex"
								 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
								 ("\\section{%s}" . "\\section*{%s}")
								 ("\\subsection{%s}" . "\\subsection*{%s}")
								 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
								 ("\\paragraph{%s}" . "\\paragraph*{%s}")
								 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'com-org-export)

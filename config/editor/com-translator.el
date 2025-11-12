(use-package gt
	:config
	(setq gt-default-translator (gt-translator :engines (gt-google-engine)))

	:ensure t)


(provide 'com-translator)


;; Load theme, taking current system APPEARANCE into consideration
(add-hook 'ns-system-appearance-change-functions
		  (lambda
			(appearance)
			(mapc #'disable-theme custom-enabled-themes)
			(pcase appearance
			  ('light
			   (load-theme 'flexoki-themes-light t))
			  ('dark
			   (load-theme 'flexoki-themes-dark t)))))
;; Maximize with no frame
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setenv "LSP_USE_PLISTS" "true")

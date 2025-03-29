;; Load theme, taking current system APPEARANCE into consideration
(defun my/apply-theme (appearance) (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance ('light (load-theme 'flexoki-themes-light t)) ('dark (load-theme 'flexoki-themes-dark t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
;; Maximize with no frame
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

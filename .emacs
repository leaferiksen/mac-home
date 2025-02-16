(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(use-package magit :ensure t)
(use-package xclip :ensure t)
(use-package auto-dark :ensure t)
(use-package flexoki-themes :ensure t)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dark-allow-osascript t)
 '(auto-dark-themes '((flexoki-themes-dark) (flexoki-themes-light)))
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(flexoki-themes-use-bold-builtins t)
 '(flexoki-themes-use-bold-keywords t)
 '(global-display-line-numbers-mode t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages '(xclip magit flexoki-themes auto-dark))
 '(xclip-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(use-package flexoki-themes :ensure t)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

(setq last-dark-mode-state 'unknown)

(defun check-and-set-dark-mode ()
  "Automatically set the theme to match if macOS is in dark mode."
  (let ((dark-mode-enabled (system-dark-mode-enabled-p)))
    (if (not (eq dark-mode-enabled last-dark-mode-state))
	(progn
	  (setq last-dark-mode-state dark-mode-enabled)
	  (if dark-mode-enabled
	    (load-theme 'flexoki-themes-dark  t)
	    (load-theme 'flexoki-themes-light t))))))

(defun system-dark-mode-enabled-p ()
  "Check if dark mode is currently enabled on macOS."
  (if (string= system-type "darwin")
      (string=
       (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"")
       "true")
      nil))

(run-with-timer 0 2 'check-and-set-dark-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(dired-listing-switches "-aoh")
 '(flexoki-themes-use-bold-builtins t)
 '(flexoki-themes-use-bold-keywords t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages '(xclip magit flexoki-themes))
 '(xclip-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

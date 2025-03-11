;; Dotconfig
(defalias 'yes-or-no-p 'y-or-n-p)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'flexoki-themes-light t))
    ('dark (load-theme 'flexoki-themes-dark t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "C-c t")
                (lambda ()
                  "Open the folder containing the current file or the current `dired` directory in Ghostty."
                  (interactive)
                  (let ((path (if (derived-mode-p 'dired-mode)
                                  (dired-get-file-for-visit)
                                (buffer-file-name))))
                    (when path
                      (shell-command (concat "open -a Ghostty --args --working-directory="
                                             (shell-quote-argument (file-name-directory path))))))))
(add-hook 'after-save-hook (lambda ()
                              (when (eq major-mode 'web-mode)
                                (shell-command "rustywind --write . > /dev/null 2>&1")
                                (revert-buffer :ignore-auto :noconfirm))))
;; Enabled optional core features
(require 'project)
(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind ( :map completion-preview-active-mode-map ("M-n" . completion-preview-next-candidate) ("M-p" . completion-preview-prev-candidate)))
;; DLC
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(use-package flexoki-themes :ensure t)
(use-package magit :ensure t)
(use-package auto-package-update :defer nil :ensure t :config (auto-package-update-maybe))
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode))
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<S-tab>") 'copilot-accept-completion-by-word))
;; Settings... ⌘ + ,
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(cursor-type 'bar)
 '(default-frame-alist '((undecorated . t) (fullscreen . maximized)))
 '(delete-by-moving-to-trash t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-agho --color=always --group-directories-first")
 '(electric-pair-mode t)
 '(flexoki-themes-use-bold-builtins t)
 '(flexoki-themes-use-bold-keywords t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
	      "main")))
 '(pixel-scroll-precision-mode t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(trash-directory "~/.Trash")
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

;; Archived config ideas 
;; opens too many tabs (use-package html-autoview-mode :ensure nil :hook (mhtml-mode . html-autoview-mode))

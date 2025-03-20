;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)
;; Auto theme
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'flexoki-themes-light t))
    ('dark (load-theme 'flexoki-themes-dark t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
;; Fix  the trackpad
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cmd Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal arrow movement
(global-set-key (kbd "s-<backspace>") 'kill-whole-line) ;; Kill one word with Alt+Backspace.
(global-set-key (kbd "M-S-<backspace>") 'kill-word)     ;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<right>") (kbd "C-e"))          ;; End of line
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))      ;; Select to end of line
(global-set-key (kbd "s-<left>") (kbd "M-m"))           ;; Beginning of line (first non-whitespace character)
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))       ;; Select to beginning of line
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)    ;; First line
(global-set-key (kbd "s-<down>") 'end-of-buffer)        ;; Last line
;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; Cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; Cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; Cmd-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0"))  ;; Cmd-0...
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; ...and Cmd-w to close current window
;; Other
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "s-d") 'dired)
(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "M-s-b") 'bookmark-set)
(global-set-key (kbd "s-g") (kbd "C-x g"))
(global-set-key (kbd "s-t")
                (lambda ()
                  "Open the folder containing the current file or the current `dired` directory in Ghostty."
                  (interactive)
                  (let ((path (if (derived-mode-p 'dired-mode)
                                  (dired-get-file-for-visit)
                                (buffer-file-name))))
                    (when path
                      (shell-command (concat "open -a Ghostty --args --working-directory="
                                             (shell-quote-argument (file-name-directory path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-save-hook (lambda ()
                              (when (eq major-mode 'web-mode)
                                (shell-command "rustywind --write . > /dev/null 2>&1")
                                (revert-buffer :ignore-auto :noconfirm))))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;; Dired Mode
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2]
    (lambda (event)
      "In Dired, visit the file or directory name you click on."
      (interactive "e")
      (mouse-set-point event)
      (dired-find-file))))
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'project)
(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind ( :map completion-preview-active-mode-map ("M-n" . completion-preview-next-candidate) ("M-p" . completion-preview-prev-candidate)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(use-package flexoki-themes)
(use-package undo-fu
  :config
  (global-set-key (kbd "s-z") 'undo-fu-only-undo)
  (global-set-key (kbd "s-Z") 'undo-fu-only-redo))
(use-package magit)
(use-package auto-package-update :defer nil :config
  (auto-package-update-maybe))
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :config
  (with-eval-after-load 'web-mode
    (define-key web-mode-map (kbd "C-c b") 'browse-url-of-file)))
(use-package copilot :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main") :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<S-tab>") 'copilot-accept-completion-by-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Settings ⌘,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(backward-delete-char-untabify-method nil)
 '(cursor-type 'bar)
 '(default-frame-alist '((undecorated . t) (fullscreen . maximized)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-agho --color=always --group-directories-first")
 '(dired-omit-files "\\`[.]?#\\|\\.DS_Store\\|\\`\\._")
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(initial-buffer-choice "~/")
 '(make-backup-files nil)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
	      "main")))
 '(pixel-scroll-precision-mode t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(trash-directory "~/.Trash")
 '(use-package-always-ensure t)
 '(which-key-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archived config ideas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opens too many tabs (use-package html-autoview-mode :ensure nil :hook (mhtml-mode . html-autoview-mode))
;; (global-set-key (kbd "s-Z") 'undo-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Behavior
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
;; Fix the trackpad
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ⌘ Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; split vertically
(global-set-key (kbd "s-w") (kbd "s-k"))  ;; close current window
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "s-d") 'dirvish)
(global-set-key (kbd "s-e") 'elfeed)
;; Open the folder containing the current file or directory in Ghostty
(global-set-key (kbd "s-t")
                (lambda ()
                  (interactive)
                  (let ((path (if (derived-mode-p 'dired-mode)
                                  (dired-get-file-for-visit)
                                (buffer-file-name))))
                    (when path
                      (shell-command (concat "open -a Ghostty --args --working-directory="
                                             (shell-quote-argument (file-name-directory path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-startup-hook (lambda () (run-at-time 5 600 'elfeed-update)))
(add-hook 'elfeed-show-mode-hook (lambda ()
								   (setq-local line-spacing 15)
								   (setq-local fill-column 90)
								   (setq-local shr-width 85)
								   (variable-pitch-mode)
								   (visual-fill-column-mode)))
(use-package dirvish
  :config
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dired-find-file)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-find-file))
;; Web Mode
(use-package web-mode :mode ("\\.html\\'" . web-mode) :config
  (define-key web-mode-map (kbd "C-c b") 'browse-url-of-file))
(add-hook 'after-save-hook (lambda ()
                              (when (eq major-mode 'web-mode)
                                (shell-command "rustywind --write . > /dev/null 2>&1")
                                (revert-buffer :ignore-auto :noconfirm))))
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (web-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-skip-config-check t)
  (setq lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wrap-urls-with-parentheses (start end)
  ;; Wrap quoted URLs with parentheses within the selected region
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\"\\(https?://[^\"]+\\)\"" end t)
      (replace-match "(\"\\1\")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archived config ideas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package eglot
;;   :after web-mode
;;   :config (add-to-list 'eglot-server-programs '((web-mode :language-id "html") . ("tailwindcss-language-server"))))

;; Normal arrow movement
;; (global-set-key (kbd "s-<backspace>") 'kill-whole-line)
;; (global-set-key (kbd "M-S-<backspace>") 'kill-word)
;; (global-set-key (kbd "s-<right>") 'end-of-line)
;; (global-set-key (kbd "S-s-<right>") (kbd "C-S-e")) ;; Select to end of line
;; (global-set-key (kbd "s-<left>") 'back-to-indentation)
;; (global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))  ;; Select to beginning of line
;; (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
;; (global-set-key (kbd "s-<down>") 'end-of-buffer)

;; (use-package copilot :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main") :config
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "<S-tab>") 'copilot-accept-completion-by-word))

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
 '(dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first -go")
 '(dired-mode-hook '(dired-omit-mode))
 '(dired-omit-files
   "\\`[.]?#\\|\\.DS_Store\\|\\`\\._\\|\\.CFUserTextEncoding\\|\\.Trash")
 '(dirvish-override-dired-mode t)
 '(electric-pair-mode t)
 '(elfeed-db-directory "~/.emacs.d/elfeed")
 '(elfeed-feeds
   '(("https://xkcd.com/rss.xml" comics) ("https://www.smbc-comics.com/comic/rss" comics) ("https://www.questionablecontent.net/QCRSS.xml" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://aftermath.site/feed" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://www.gameinformer.com/rss.xml" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://redstrate.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://cdn.jwz.org/blog/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://www.girlonthenet.com/feed/" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://lwlies.com/feed/" journals) ("https://howtodothingswithmemes.substack.com/feed" journals) ("https://basicappleguy.com/basicappleblog?format=rss" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
 '(elfeed-search-filter "@1-month-ago +unread")
 '(fill-column 9999)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-prettify-symbols-mode t)
 '(initial-buffer-choice 'dirvish)
 '(make-backup-files nil)
 '(package-selected-packages
   '(dirvish elfeed flexoki-themes lsp-mode lsp-tailwindcss magit undo-fu visual-fill-column web-mode))
 '(package-vc-selected-packages
   '((lsp-tailwindcss :url "https://github.com/merrickluo/lsp-tailwindcss" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(flymake-mode display-line-numbers-mode completion-preview-mode visual-line-mode))
 '(project-mode-line t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(tab-width 4)
 '(trash-directory "~/.Trash")
 '(use-package-always-ensure t)
 '(visual-fill-column-center-text t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :foundry "nil" :slant normal :weight regular :height 200 :width normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install selected packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install-selected-packages)
(package-autoremove)

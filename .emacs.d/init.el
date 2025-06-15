;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable text rescaling
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "<C-M-wheel-up>") 'ignore)
(global-set-key (kbd "<C-M-wheel-down>") 'ignore)
;; My custom keybinds
(global-set-key (kbd "s-w")   'kill-current-buffer)
(global-unset-key (kbd "s-z"))
(global-set-key (kbd "s-z")   'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
(global-set-key (kbd "s-j") 'bookmark-jump)
(global-set-key (kbd "C-;") 'comment-box)
(global-set-key (kbd "C-y") 'yt-dlp)
;; Escape hatch
(define-key esc-map	[escape] 'keyboard-quit)
(define-key ctl-x-map [escape] 'keyboard-quit)
(define-key help-map [escape] 'keyboard-quit)
(define-key goto-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme, taking current system APPEARANCE into consideration
;; https://codeberg.org/crmsnbleyd/flexoki-emacs-theme
(use-package flexoki-themes
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments t))
;; https://github.com/d12frosted/homebrew-emacs-plus
(add-hook 'ns-system-appearance-change-functions
		  (lambda
			(appearance)
			(mapc #'disable-theme custom-enabled-themes)
			(pcase appearance
			  ('light
			   (load-theme 'flexoki-themes-light t)
			   (set-face-attribute 'markdown-italic-face nil :slant 'italic :foreground "#100f0f"))
			  ('dark
			   (load-theme 'flexoki-themes-dark t)
			   (set-face-attribute 'markdown-italic-face nil :slant 'italic :foreground "#fffcf0")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(use-package dired
  :bind
  ("C-c d" . 'dired-finder-path)
  (:map dired-mode-map
		("<mouse-2>" . dired-mouse-find-file)
		("SPC" . 'quicklook)
		("o" . 'macopen)))
(defun quicklook ()
  "QuickLook the currently selected file in Dired."
  (interactive)
  (let ((filename (dired-get-file-for-visit))) (shell-command (format "qlmanage -p '%s'" filename))))
(defun macopen ()
  "QuickLook the currently selected file in Dired."
  (interactive)
  (let ((filename (dired-get-file-for-visit))) (shell-command (format "open '%s'" filename))))
(defun dired-finder-path ()
  "Open Dired in the frontmost Finder window path, if available."
  (interactive)
  (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
	(if path (dired (string-trim path)) (message "No Finder window found."))))
(defun yt-dlp (url)
  "Download the audio, video, or video with subs from a given URL."
  (interactive "sEnter URL to download: ")
  (let ((choice	(completing-read "Choose download option: " '("video" "video with subtitles" "audio"))))
	(cond
	 ((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
	 ((string-equal choice "video with subtitles") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url)))
	 ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x --embed-thumbnail \"%s\"" url))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; (add-hook 'after-init-hook #'global-prettier-mode)
(use-package prettier
  :hook after-init)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :hook (html-ts-mode css-ts-mode js-ts-mode typescript-ts-mode swift-mode) . 'eglot-ensure)
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note-taking
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown
  :hook
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . variable-pitch-mode)
  (markdown-mode .
				 (lambda
				   ()
				   (setq-local fill-column 90)
				   (setq-local line-spacing 12))))
;; https://github.com/licht1stein/obsidian.el
(use-package obsidian
  :hook markdown-mode
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind
  (:map obsidian-mode-map
		("s-i" . markdown-insert-italic)
		("s-b" . markdown-insert-bold)
		("s-<return>" . obsidian-follow-link-at-point)
		("s-S-<return>" . obsidian-backlink-jump)))
;; https://github.com/minad/jinx
(use-package jinx
  :hook markdown-mode
  :bind
  ("C-c c" . jinx-correct))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://depp.brause.cc/nov.el/
(use-package nov
  :init
  (add-to-list 'auto-mode-alist
			   '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode .
			(lambda
			  ()
			  (setq-local line-spacing 15)
			  (setq-local fill-column 90)
			  (face-remap-add-relative 'variable-pitch :family "kermit" :height 240)))
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  :custom
  (nov-text-width t)
  (visual-fill-column-center-text t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :hook
  (elfeed-show-mode . variable-pitch-mode)
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . visual-fill-column-mode)
  (elfeed-show-mode .
					(lambda ()
					  (setq-local line-spacing 12)
					  (setq-local fill-column 90)
					  (setq-local shr-width 85)
					  (setq-local shr-max-image-proportion 0.5)
					  (setq-local shr-inhibit-images t)))
  :bind
  (:map elfeed-show-mode-map
		("<mouse-1>" . elfeed-show-next)
		("<mouse-3>" . elfeed-show-prev)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper
(use-package minesweeper
  :hook
  (minesweeper-mode .
					(lambda ()
					  (face-remap-add-relative 'default :height 200))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Midnight automations
(defun backup-obsidian ()
  "Run the zsh script to backup Obsidian and send a message."
  (call-process-shell-command "cd \"$HOME/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes\" && zip -r \"$HOME/Git/Obsidian Backups/$(date +%Y-%m-%d_%H%M).zip\" ."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
(defun insert-date ()
  "Insert today's date in iso format."
  (interactive)
  (format-time-string "%Y-%m-%d"))
(defun vc-amend ()
  "Amend the previous commit title."
  (interactive)
  (vc-checkin nil 'git)
  (vc-git-log-edit-toggle-amend))
(defun send-to-self	(message)
  "Send a MESSAGE to myself."
  (interactive "sMessage to send: ")
  (let ((message
		 (or message "")))	; Ensure message isn't nil
	(shell-command (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'" (shell-quote-argument message)))))
(defun wrap-urls-with-parentheses (start end)
  "Wrap quoted URLs with parentheses from START to END."
  (interactive "r")
  (save-excursion (goto-char start)
				  (while (re-search-forward "\"\\(https?://[^\"]+\\)\"" end t) (replace-match "(\"\\1\")"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-global-mode t)
 '(auto-package-update-delete-old-versions t)
 '(backward-delete-char-untabify-method nil)
 '(completion-styles '(basic partial-completion emacs22 flex))
 '(cursor-type 'bar)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first")
 '(dired-mode-hook
   '(dired-hide-details-mode nerd-icons-dired-mode dired-omit-mode))
 '(dired-mouse-drag-files 'move)
 '(dired-omit-files
   "\\`[.]?#\\|\\.DS_Store\\|\\`\\._\\|\\.CFUserTextEncoding\\|\\.Trash")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '(("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://aftermath.site/feed" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://redstrate.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://www.girlonthenet.com/feed/" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://basicappleguy.com/basicappleblog?format=rss" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
 '(elfeed-search-filter "@1-month-ago +unread")
 '(fill-column 9999)
 '(frame-resize-pixelwise t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-flycheck-mode t)
 '(global-prettify-symbols-mode t)
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(lsp-dired-mode t)
 '(make-backup-files nil)
 '(markdown-enable-wiki-links t)
 '(markdown-header-scaling t)
 '(markdown-hide-markup nil)
 '(markdown-wiki-link-alias-first nil)
 '(midnight-delay 7200)
 '(midnight-hook '(update-homebrew backup-obsidian))
 '(midnight-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(ns-pop-up-frames nil)
 '(ns-right-alternate-modifier 'control)
 '(obsidian-directory
   "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes" nil nil "Customized with use-package obsidian")
 '(package-selected-packages
   '(apheleia eglot elfeed elfeed-protocol esxml exec-path-from-shell flexoki-themes flymake-eslint jinx minesweeper minions nerd-icons-dired nov obsidian prettier swift-mode terminal-here treesit-auto undo-fu visual-fill-column))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(flymake-mode display-line-numbers-mode completion-preview-mode))
 '(project-mode-line t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(tab-line-new-button-show nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(trash-directory "~/.Trash")
 '(use-dialog-box nil)
 '(use-package-always-ensure t)
 '(visual-fill-column-center-text t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(markdown-code-face ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :foundry "nil" :slant normal :weight regular :height 200 :width normal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install selected packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install-selected-packages)
(package-autoremove)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

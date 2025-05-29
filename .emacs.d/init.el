;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max
	  (* 1024 1024))
;; 1mb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System consistency improvements to trackpad, ⌘, ⌥, and esc.
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key
 (kbd "<pinch>")
 'ignore)
(global-set-key
 (kbd "<C-wheel-up>")
 'ignore)
(global-set-key
 (kbd "<C-wheel-down>")
 'ignore)
(global-set-key
 (kbd "<C-M-wheel-up>")
 'ignore)
(global-set-key
 (kbd "<C-M-wheel-down>")
 'ignore)
(global-set-key
 (kbd "s-z")
 'undo-fu-only-undo)
(global-set-key
 (kbd "s-Z")
 'undo-fu-only-redo)
(global-set-key
 (kbd "s-o")
 'bookmark-jump)
(global-set-key
 (kbd "s-;")
 'comment-box)
(global-set-key
 [escape]
 'keyboard-quit)
(define-key esc-map
			[escape]
			'keyboard-quit)
(define-key ctl-x-map
			[escape]
			'keyboard-quit)
(define-key help-map
			[escape]
			'keyboard-quit)
(define-key goto-map
			[escape]
			'keyboard-quit)
(define-key minibuffer-local-map
			[escape]
			'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map
			[escape]
			'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map
			[escape]
			'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map
			[escape]
			'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map
			[escape]
			'minibuffer-keyboard-quit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation and Selection mode
;; https://github.com/mrkkrp/modalka
(use-package modalka
  :init
  (setq-default
   modalka-cursor-type 'box)
  (modalka-global-mode 1)
  (add-to-list 'modalka-excluded-modes 'dired-mode)
  (add-to-list 'modalka-excluded-modes 'vc-git-log-edit-mode)
  (add-to-list 'modalka-excluded-modes 'term-mode)
  (add-to-list 'modalka-excluded-modes 'eshell-mode)
  (add-to-list 'modalka-excluded-modes 'elfeed-search-mode)
  (add-to-list 'modalka-excluded-modes 'elfeed-show-mode)
  :config
  (define-key modalka-mode-map
			  (kbd "SPC")
			  'set-mark-command)
  (modalka-define-kbd ";" "M-;")
  (modalka-define-kbd ":" "M-:")
  (modalka-define-kbd "$" "M-$")
  (modalka-define-kbd "&" "M-&")
  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")
  (modalka-define-kbd "!" "M-!")
  (modalka-define-kbd "%" "M-%")
  (modalka-define-kbd "|" "M-|")
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (define-key modalka-mode-map "c" mode-specific-map)
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (define-key modalka-mode-map "g" goto-map)
  (define-key modalka-mode-map "h" help-map)
  (modalka-define-kbd "i" "C-i")
  (modalka-define-kbd "j" "C-j")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "l" "C-l")
  (modalka-define-kbd "m" "C-m")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "o" "C-o")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "q" "M-q")
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "t" "C-t")
  (modalka-define-kbd "u" "C-u")
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "w" "C-w")
  (define-key modalka-mode-map "x" ctl-x-map)
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "M-z")
  (modalka-define-kbd "A" "M-a")
  (modalka-define-kbd "B" "C-M-b")
  (modalka-define-kbd "C" "M-c")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "E" "M-e")
  (modalka-define-kbd "F" "C-M-f")
  (define-key modalka-mode-map "G" goto-map)
  (modalka-define-kbd "H" "M-h")
  (modalka-define-kbd "I" "M-i")
  (modalka-define-kbd "J" "M-j")
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "L" "M-l")
  (modalka-define-kbd "M" "M-m")
  (modalka-define-kbd "N" "C-M-n")
  (modalka-define-kbd "O" "M-o")
  (modalka-define-kbd "P" "C-M-p")
  (modalka-define-kbd "Q" "M-q")
  (modalka-define-kbd "R" "M-r")
  (modalka-define-kbd "S" "M-s")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "U" "M-u")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "W" "M-w")
  (define-key modalka-mode-map "X" 'execute-extended-command)
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "Z" "M-z")
  :bind
  (("<f13>" . modalka-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(use-package dired
  :bind
  ("C-c d" . 'dired-finder-path)
  (:map dired-mode-map
		("<mouse-2>" . dired-mouse-find-file)
		("SPC" . 'quicklook)
		("o" . 'macopen)))
(defun quicklook
	()
  "QuickLook the currently selected file in Dired."
  (interactive)
  (let
	  ((filename
		(dired-get-file-for-visit)))
	(shell-command
	 (format "qlmanage -p '%s'" filename))))
(defun macopen
	()
  "QuickLook the currently selected file in Dired."
  (interactive)
  (let
	  ((filename
		(dired-get-file-for-visit)))
	(shell-command
	 (format "open '%s'" filename))))
(defun dired-finder-path
	()
  "Open Dired in the frontmost Finder window path, if available."
  (interactive)
  (let
	  ((path
		(ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
	(if path
		(dired
		 (string-trim path))
	  (message "No Finder window found."))))
(defun yt-dlp
	(url)
  "Download the audio, video, or video with subs from a given URL."
  (interactive "sEnter URL to download: ")
  (let
	  ((choice
		(completing-read
         "Choose download option: "
		 '("video" "video with subtitles" "audio"))))
	(cond
	 ((string-equal choice "video")
	  (async-shell-command
	   (format "yt-dlp \"%s\"" url)))
	 ((async-string-equal choice "video with subtitles")
	  (shell-command
	   (format "yt-dlp --write-subs \"%s\"" url)))
	 ((string-equal choice "audio")
	  (async-shell-command
	   (format "yt-dlp -x --embed-thumbnail \"%s\"" url))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/minad/jinx
(use-package jinx
  :hook
  (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://codeberg.org/crmsnbleyd/flexoki-emacs-theme
(use-package flexoki-themes
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments t))
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
;; https://codeberg.org/martianh/mastodon.el
(use-package mastodon
  :custom
  (mastodon-instance-url "https://mastodon.social")
  (mastodon-active-user "leaferiksen"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :hook
  (elfeed-show-mode . variable-pitch-mode)
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . visual-fill-column-mode)
  (elfeed-show-mode .
					(lambda
					  ()
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
;; https://github.com/lorniu/go-translate
(use-package go-translate
  :custom
  (gt-langs
   '(en jp))
  (gt-default-translator
   (gt-translator :engines
				  (gt-google-engine))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :none)
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  :hook
  (html-ts-mode . lsp)
  (css-ts-mode . lsp)
  (js-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (tsx-ts-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)
;; https://github.com/merrickluo/lsp-tailwindcss
(use-package lsp-tailwindcss
  :after lsp-mode
  :custom
  (lsp-tailwindcss-add-on-mode t)
  (lsp-tailwindcss-skip-config-check t)
  (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server")
  :init
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme, taking current system APPEARANCE into consideration
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
;; Midnight automations
(defun update-homebrew
	()
  "Update all casks and formulae."
  (call-process-shell-command "brew update && brew upgrade --greedy"))
(defun backup-obsidian
	()
  "Run the zsh script to backup Obsidian and send a message."
  (call-process-shell-command "cd \"$HOME/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes\" && zip -r \"$HOME/Git/Obsidian Backups/$(date +%Y-%m-%d_%H%M).zip\" ."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
(defun send-to-self
	(message)
  "Send a MESSAGE to myself."
  (interactive "sMessage to send: ")
  (let
	  ((message
		(or message "")))
										; Ensure message isn't nil
	(shell-command
	 (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'"
			 (shell-quote-argument message)))))
(defun wrap-urls-with-parentheses
	(start end)
  "Wrap quoted URLs with parentheses from START to END."
  (interactive "r")
  (save-excursion
	(goto-char start)
	(while
		(re-search-forward "\"\\(https?://[^\"]+\\)\"" end t)
	  (replace-match "(\"\\1\")"))))

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
   '(("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://aftermath.site/feed" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://www.gameinformer.com/rss.xml" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://redstrate.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://www.girlonthenet.com/feed/" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://lwlies.com/feed/" journals) ("https://basicappleguy.com/basicappleblog?format=rss" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
 '(elfeed-search-filter "@1-month-ago +unread")
 '(fill-column 9999)
 '(frame-resize-pixelwise t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-flycheck-mode t)
 '(global-prettify-symbols-mode t)
 '(global-visual-line-mode t)
 '(initial-buffer-choice t)
 '(lsp-dired-mode t nil (lsp-dired))
 '(make-backup-files nil)
 '(markdown-enable-wiki-links t)
 '(markdown-header-scaling t)
 '(markdown-hide-markup nil)
 '(markdown-wiki-link-alias-first nil)
 '(midnight-delay 7200)
 '(midnight-hook '(update-homebrew backup-obsidian))
 '(midnight-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(obsidian-directory
   "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes" nil nil "Customized with use-package obsidian")
 '(package-selected-packages
   '(apheleia elfeed elfeed-protocol esxml flexoki-themes go-translate jinx lsp-mode lsp-tailwindcss mastodon minesweeper minions modalka nerd-icons-dired nov obsidian terminal-here treesit-auto undo-fu visual-fill-column))
 '(package-vc-selected-packages
   '((lsp-tailwindcss :url "https://github.com/merrickluo/lsp-tailwindcss" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(flymake-mode display-line-numbers-mode completion-preview-mode))
 '(project-mode-line t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
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

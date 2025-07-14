;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(defalias 'yes-or-no-p 'y-or-n-p)
;; Unmap default navigation bindings and text rescaling
(keymap-global-unset "<pinch>")
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")
(keymap-global-unset "M-<wheel-up>")
(keymap-global-unset "M-<wheel-down>")
(keymap-global-unset "C-M-<wheel-up>")
(keymap-global-unset "C-M-<wheel-down>")
(keymap-global-unset "C-p")
(keymap-global-unset "C-n")
(keymap-global-unset "C-f")
(keymap-global-unset "M-f")
(keymap-global-unset "C-b")
(keymap-global-unset "M-b")
(keymap-global-unset "C-M-p")
(keymap-global-unset "C-M-n")
(keymap-global-unset "C-M-f")
(keymap-global-unset "C-M-b")
(keymap-global-unset "C-d")
(keymap-global-unset "M-d")
(keymap-global-unset "C-w")
(keymap-global-unset "M-w")
;; Command key bindings
(keymap-global-set "s-1" 'execute-extended-command)
(global-set-key (kbd "s-2")
				`(lambda () "Simulates the `C-x' key-press" (interactive)
				   (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-x")))))
(global-set-key (kbd "s-3")
				`(lambda () "Simulates the `C-c' key-press" (interactive)
				   (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c")))))
(global-set-key (kbd "s-4")
				`(lambda () "Simulates the `C-h' key-press" (interactive)
				   (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-h")))))
(keymap-global-set "s-z" 'undo-fu-only-undo)
(keymap-global-set "s-Z" 'undo-fu-only-redo)
(keymap-global-set "s-o" 'find-file)
(keymap-global-set "s-w" 'kill-current-buffer)
(keymap-global-set "s-t" 'ghostty)
(keymap-global-set "s-e" 'elfeed)
(keymap-global-set "s-j" 'obsidian-daily-note)
(keymap-global-set "s-;" 'comment-box)
(keymap-global-set "s-y" 'yt-dlp)
(keymap-global-set "s-b" 'bookmark-jump)
(keymap-global-set "s-d" 'bookmark-delete)
;; Colemak extend bindings
(keymap-global-set "M-<up>" 'backward-up-list)
(keymap-global-set "M-<down>" 'down-list)
(keymap-global-set "M-<left>" 'backward-sexp)
(keymap-global-set "M-<right>" 'forward-sexp)
(keymap-global-set "<home>" 'move-beginning-of-line)
(keymap-global-set "<end>" 'move-end-of-line)
(keymap-global-set "M-<home>" 'completion-preview-prev-candidate)
(keymap-global-set "M-<end>" 'completion-preview-next-candidate)
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
;; https://github.com/meedstrom/massmapper
(use-package massmapper
  :init
  (add-hook 'massmapper-keymap-found-hook #'massmapper-homogenize -50)
  :config
  (massmapper-mode 1))
(use-package which-key
  :after massmapper-mode
  :init
  (cl-pushnew '((" .-." . nil) . t) which-key-replacement-alist
              :test #'equal))
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
(require 'ls-lisp)
(use-package dired
  :bind
  (:map dired-mode-map
		("<mouse-2>" . dired-mouse-find-file)
		("SPC" . 'quicklook)
		("C-f" . 'dired-finder-path)
		("o" . 'macopen)
		("<up>" . 'dired-previous-line)
		("<down>" . 'dired-next-line)
		("<right>" . 'dired-find-file)
		("<left>" . 'dired-up-directory)))
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
  :hook after-init
  :bind
  ("C-p" . 'prettier-prettify)
  ("C-S-p" . 'prettier-prettify-region))
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
			   '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :hook (html-mode css-mode js-mode typescript-mode swift-mode) . 'eglot-ensure)
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
  (markdown-mode . jinx-mode)
  (markdown-mode .
				 (lambda
				   ()
				   (setq-local fill-column 90)
				   (setq-local line-spacing 12))))
;; https://github.com/licht1stein/obsidian.el
(use-package obsidian
  :config
  (global-obsidian-mode t)
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind
  (:map obsidian-mode-map
		("s-i" . markdown-insert-italic)
		("s-b" . markdown-insert-bold)
		("s-<return>" . obsidian-follow-link-at-point)
		("s-S-<return>" . obsidian-backlink-jump)))
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
  (:map elfeed-search-mode-map
		("u" . previous-line)
		("e" . next-line)
		("n" . elfeed-search-fetch)
		("i" . elfeed-search-show-entry))
  (:map elfeed-show-mode-map
		("<mouse-1>" . elfeed-show-next)
		("<mouse-3>" . elfeed-show-prev)
		("<return>" . elfeed-show-next)
		("S-<return>" . elfeed-show-prev)
		("u" . elfeed-show-prev)
		("e" . elfeed-show-next)
		("n" . elfeed-kill-buffer)
		("i" . elfeed-show-visit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper
(use-package minesweeper
  :hook
  (minesweeper-mode .
					(lambda ()
					  (face-remap-add-relative 'default :height 200))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
(defun ghostty ()
  "Open current directory in Ghostty."
  (interactive)
  (shell-command (concat "open -a Ghostty --args --working-directory=" (expand-file-name default-directory))))
(defun insert-date ()
  "Insert today's date in iso format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(defun fix-node ()
  "Unlink and relink node binaries."
  (interactive)
  (async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link node"))
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
 '(cursor-type 'bar)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-alh")
 '(dired-mode-hook '(nerd-icons-dired-mode dired-omit-mode))
 '(dired-omit-files
   "\\`[.]?#\\|\\.DS_Store\\|\\`\\._\\|\\.CFUserTextEncoding\\|\\.Trash")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '(("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://aftermath.site/feed" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://basicappleguy.com/basicappleblog?format=rss" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
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
 '(initial-buffer-choice "~/Documents/")
 '(isearch-lazy-count t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format "   (%s/%s)")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(lsp-dired-mode t)
 '(make-backup-files nil)
 '(markdown-enable-wiki-links t)
 '(markdown-header-scaling t)
 '(markdown-hide-markup nil)
 '(markdown-wiki-link-alias-first nil)
 '(mouse-wheel-progressive-speed nil)
 '(ns-pop-up-frames nil)
 '(obsidian-directory
   "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes" nil nil "Customized with use-package obsidian")
 '(package-selected-packages
   '(apheleia eglot elfeed elfeed-protocol esxml exec-path-from-shell flexoki-themes flymake-eslint jinx massmapper minesweeper minions nerd-icons-dired nov obsidian prettier swift-mode treesit-auto undo-fu visual-fill-column))
 '(package-vc-selected-packages
   '((massmapper :url "https://github.com/meedstrom/massmapper")))
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
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(package-install-selected-packages)
(package-autoremove)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

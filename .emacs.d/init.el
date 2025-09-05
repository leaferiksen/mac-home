;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
(use-package emacs
  :hook (ns-system-appearance-change-functions . auto-theme)
  :bind (("s-z" . undo-fu-only-undo) ("s-Z" . undo-fu-only-redo)
		 ("s-o" . find-file) ("s-b" . bookmark-jump)
		 ("s-t" . ghostty) ("s-y" . yt-dlp)
		 ("M-<up>" . backward-up-list) ("M-<down>" . down-list) ("M-<left>" . backward-sexp) ("M-<right>" . forward-sexp) ("<home>" . move-beginning-of-line) ("<end>" . move-end-of-line) ;; Colemak extend bindings
		 ("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil) ;; Unmap default navigation bindings and text rescaling
		 ([escape] . keyboard-quit) (:map esc-map ([escape] . keyboard-quit)) (:map ctl-x-map ([escape] . keyboard-quit)) (:map help-map ([escape] . keyboard-quit)) (:map goto-map ([escape] . keyboard-quit)) (:map minibuffer-mode-map ([escape] . minibuffer-keyboard-quit)))
  :custom-face (default ((t (:family "Maple Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
  (variable-pitch ((t (:family "Atkinson Hyperlegible Next" :foundry "nil" :slant normal :weight regular :height 200 :width normal))))
  :custom ((auto-package-update-delete-old-versions t)
		   (backward-delete-char-untabify-method nil)
		   (completion-ignore-case t t)
		   (context-menu-mode t)
		   (cursor-type 'bar)
		   (custom-file (expand-file-name "/dev/null"))
		   (delete-by-moving-to-trash t)
		   (delete-selection-mode t)
		   (dired-mode-hook '(nerd-icons-dired-mode dired-omit-mode))
		   (editorconfig-mode t)
		   (electric-pair-mode t)
		   (fill-column 9999)
		   (flexoki-themes-use-bold-keywords t)
		   (flexoki-themes-use-bold-builtins t)
		   (flexoki-themes-use-italic-comments t)
		   (frame-resize-pixelwise t)
		   (gc-cons-threshold 100000000)
		   (global-auto-revert-mode t)
		   (global-auto-revert-non-file-buffers t)
		   (global-devil-mode t)
		   (global-visual-line-mode t)
		   (inhibit-startup-screen t)
		   (initial-buffer-choice "~/Documents/")
		   (isearch-lazy-count t)
		   (lazy-count-prefix-format nil)
		   (lazy-count-suffix-format "   (%s/%s)")
		   (make-backup-files nil)
		   (mouse-wheel-progressive-speed nil)
		   (ns-pop-up-frames nil)
		   (package-selected-packages '(apheleia captain company devil eglot elfeed esxml exec-path-from-shell flexoki-themes flymake-eslint jinx lorem-ipsum lsp-mode lsp-tailwindcss minesweeper minions nerd-icons-dired nov obsidian snow spacious-padding swift-mode treesit-langs undo-fu visual-fill-column yasnippet))
		   (package-vc-selected-packages (:vc-backend Git :url "https://github.com/emacs-tree-sitter/treesit-langs"))
		   (pixel-scroll-precision-mode t)
		   (prog-mode-hook '(apheleia-mode company-mode prettify-symbols-mode flymake-mode display-line-numbers-mode))
		   (project-mode-line t)
		   (project-vc-extra-root-markers '("project"))
		   (read-buffer-completion-ignore-case t)
		   (read-process-output-max (* 1024 1024))
		   (ring-bell-function 'ignore)
		   (scroll-bar-mode nil)
		   (sentence-end-double-space nil)
		   (shr-fill-text nil)
		   (shr-inhibit-images t)
		   (snow-pile-factor 1)
		   (spacious-padding-mode t)
		   (split-height-threshold 0)
		   (split-width-threshold nil)
		   (tab-width 4)
		   (tool-bar-mode nil)
		   (tooltip-mode nil)
		   (use-dialog-box nil)
		   (use-package-always-ensure t)
		   (visual-fill-column-width 85)
		   (warning-minimum-level :error)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(use-package ls-lisp
  :custom ((ls-lisp-dirs-first t)
		   (ls-lisp-ignore-case t)
		   (ls-lisp-use-insert-directory-program nil)
		   (ls-lisp-use-localized-time-format t)
		   (ls-lisp-verbosity nil)))
(use-package dired
  :after ls-lisp
  :bind (:map dired-mode-map (("<mouse-1>" . nil)
							  ("<mouse-2>" . nil)
							  ("SPC" . 'quicklook)
							  ("u" . 'dired-previous-line)
							  ("e" . 'dired-next-line)
							  ("i" . 'dired-find-file)
							  ("n" . 'dired-up-directory)
							  ("k" . 'dired-unmark)
							  ("p" . nil)
							  ("f" . dired-finder-path)
							  ("v" . nil)
							  ("o" . 'dired-do-open)
							  ("a" . 'afinfo)))
  :custom ((trash-directory "~/.Trash")
		   (dired-clean-confirm-killing-deleted-buffers nil)
		   (dired-create-destination-dirs 'ask)
		   (dired-kill-when-opening-new-dired-buffer t)
		   (dired-listing-switches "-Alh")
		   (dired-mouse-drag-files t)
		   (dired-omit-files
			"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
		   (dired-recursive-copies 'always)))
(use-package vc-dir
  :bind (:map vc-dir-mode-map
			  ("u" . 'vc-dir-previous-line)
			  ("e" . 'vc-dir-next-line)
			  ("i" . 'vc-dir-find-file)
			  ("n" . nil)
			  ("p" . nil)
			  ("k" . 'vc-dir-unmark)))
(use-package nerd-icons
  :load-path "elpa/nerd-icons.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-mode
(use-package markdown
  :mode ("README\\.md\\'" . gfm-mode)
  :custom ((markdown-enable-wiki-links t)
		   (markdown-hide-urls t)
		   (markdown-hide-markup t))
  :hook ((markdown-mode . visual-fill-column-mode)
		 (markdown-mode . jinx-mode)
		 (markdown-mode . (lambda ()
							(setq-local line-spacing 12)
							(face-remap-add-relative 'default :family "Old Timey Code" :height 180))))
  :custom-face (markdown-code-face ((t (:family "Maple Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))
(use-package obsidian
  :preface (global-obsidian-mode t)
  :custom (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind (("s-d" . obsidian-daily-note)
		 (:map obsidian-mode-map ([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog-mode
(use-package prog-mode
  :custom (major-mode-remap-alist
		   '((css-mode . css-ts-mode)
			 (dockerfile-mode . dockerfile-ts-mode)
			 (mhtml-mode . html-ts-mode)
			 (javascript-mode . js-ts-mode)
			 (json-mode . json-ts-mode)
			 (typescript-mode . typescript-ts-mode)
			 (yaml-mode . yaml-ts-mode)))
  (use-package company-mode
	:custom ((company-minimum-prefix-length 1)
			 (company-idle-delay 0.0))
	:bind (:map company-active-map ([escape] . company-abort)))
  (use-package flymake-mode
	:bind (:map flymake-diagnostics-buffer-mode-map (("u" . 'previous-line)
													 ("e" . 'next-line)
													 ("n" . nil)
													 ("p" . nil))))
  (use-package html-ts
	:custom (html-ts-mode-indent-offset 4)))
(use-package lsp-mode
  :hook ((html-mode . lsp)
		 (css-mode . lsp)
		 (js-mode . lsp)
		 (typescript-mode . lsp)
		 (tsx-mode . lsp))
  :commands lsp)
(use-package lsp-tailwindcss
  :after lsp-mode
  :custom ((lsp-tailwindcss-add-on-mode t)
		   (lsp-tailwindcss-skip-config-check t)
		   (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://depp.brause.cc/nov.el/
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
		 (nov-mode . visual-fill-column-mode)
		 (nov-mode . (lambda ()
					   (setq-local line-spacing 15)
					   (setq-local fill-column 90)
					   (face-remap-add-relative 'variable-pitch :family "kermit" :height 240))))
  :custom ((nov-text-width t)
		   (visual-fill-column-center-text t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :hook ((elfeed-show-mode . variable-pitch-mode)
		 (elfeed-show-mode . visual-line-mode)
		 (elfeed-show-mode . visual-fill-column-mode)
		 (elfeed-show-mode .
						   (lambda ()
							 (setq-local line-spacing 12))))
  :bind (("s-e" . elfeed)
		 (:map elfeed-search-mode-map
			   ("u" . previous-line)
			   ("e" . next-line)
			   ("n" . elfeed-search-fetch)
			   ("i" . elfeed-search-show-entry))
		 (:map elfeed-show-mode-map
			   ("<return>" . elfeed-show-next)
			   ("S-<return>" . elfeed-show-prev)
			   ("u" . elfeed-show-prev)
			   ("e" . elfeed-show-next)
			   ("n" . elfeed-kill-buffer)
			   ("i" . elfeed-show-visit)))
  :custom ((elfeed-feeds '(("https://buttondown.com/monteiro/rss" design) ("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
		   (elfeed-search-filter "@1-month-ago +unread")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper
(use-package minesweeper
  :hook (minesweeper-mode .	(lambda ()
							  (face-remap-add-relative 'default :height 200))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://codeberg.org/crmsnbleyd/flexoki-emacs-theme
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(defalias 'yes-or-no-p 'y-or-n-p)
(defun setup () "Install selected packages."
	   (interactive)
	   (package-refresh-contents)
	   (package-install-selected-packages)
	   (package-autoremove))
(defun auto-theme (appearance) "Load theme, taking current system APPEARANCE into consideration."
	   (mapc #'disable-theme custom-enabled-themes)
	   (pcase appearance
		 ('light (load-theme 'flexoki-themes-light t)
				 (custom-set-faces
				  '(markdown-italic-face ((t (:foreground "#100f0f" :slant italic))))
				  '(outline-1 ((t (:height 1.4 :inherit 'default :foreground "#AF3029" :weight semi-bold))))
				  '(outline-2 ((t (:height 1.3 :weight semi-bold :foreground "#BC5215" :inherit 'default))))
				  '(outline-3 ((t (:height 1.2 :weight semi-bold :foreground "#AD8301" :inherit 'default))))
				  '(outline-4 ((t (:height 1.1 :weight semi-bold :foreground "#66800B" :inherit 'default))))
				  '(outline-5 ((t (:height 1.0 :weight semi-bold :foreground "#24837B" :inherit 'default))))
				  '(outline-6 ((t (:height 1.0 :weight semi-bold :foreground "#205EA6" :inherit 'default))))))
		 ('dark (load-theme 'flexoki-themes-dark t)
				(custom-set-faces
				 '(markdown-italic-face ((t (:foreground "#CECDC3" :slant italic))))
				 '(outline-1 ((t (:height 1.4 :inherit 'default :foreground "#D14D41" :weight semi-bold))))
				 '(outline-2 ((t (:height 1.3 :weight semi-bold :foreground "#DA702C" :inherit 'default))))
				 '(outline-3 ((t (:height 1.2 :weight semi-bold :foreground "#D0A215" :inherit 'default))))
				 '(outline-4 ((t (:height 1.1 :weight semi-bold :foreground "#879A39" :inherit 'default))))
				 '(outline-5 ((t (:height 1.0 :weight semi-bold :foreground "#3AA99F" :inherit 'default))))
				 '(outline-6 ((t (:height 1.0 :weight semi-bold :foreground "#4385BE" :inherit 'default))))))))
(defun vc-amend () "Amend the previous commit title."
	   (interactive)
	   (vc-checkin nil 'git)
	   (vc-git-log-edit-toggle-amend))
(defun insert-date () "Insert today's date in iso format."
	   (interactive)
	   (insert (format-time-string "%Y-%m-%d")))
(defun ghostty () "Open current directory in Ghostty."
	   (interactive)
	   (shell-command (concat "open -a Ghostty --args --working-directory=" "\""(expand-file-name default-directory)"\"")))
(defun tailwind-server () "Start a tailwind in the current directory, sourcing app.css."
	   (interactive)
	   (let ((filename (concat "tailwind-server@ <" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
		 (start-process filename filename "/opt/homebrew/bin/npx" "@tailwindcss/cli" "-i" "app.css" "-o" "dist.css" "--watch")))
(defun http-server () "Start a local server at ./index.html, avoiding port conflicts."
	   (interactive)
	   (unless (boundp 'http-port-offset)
		 (setq http-port-offset 0))
	   (setq http-port (- 9999 http-port-offset))
	   (setq http-port-offset (1+ http-port-offset))
	   (let ((filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
		 (start-process filename filename "/opt/homebrew/bin/npx" "http-server" "-o" "-p" (number-to-string http-port))))
(defun fix-node () "Unlink and relink node binaries."
	   (interactive)
	   (async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node"))
(defun send-to-self	(message) "Send a MESSAGE to myself."
	   (interactive "sMessage to send: ")
	   (let ((message
			  (or message "")))			; Ensure message isn't nil
		 (shell-command (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'" (shell-quote-argument message)))))
(defun wrap-urls-with-parentheses (start end) "Wrap quoted URLs with parentheses from START to END."
	   (interactive "r")
	   (save-excursion (goto-char start)
					   (while (re-search-forward "\"\\(https?://[^\"]+\\)\"" end t) (replace-match "(\"\\1\")"))))
(defun quicklook () "QuickLook the currently selected file in Dired."
	   (interactive)
	   (let ((filename (dired-get-file-for-visit))) (shell-command (concat "qlmanage -p \"" filename "\" > /dev/null 2>&1"))))
(defun dired-finder-path () "Open Dired in the frontmost Finder window path, if available."
	   (interactive)
	   (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
		 (if path (dired (string-trim path)) (message "No Finder window found."))))
(defun afinfo () "Get metadata for focused file."
	   (interactive)
	   (let ((filename (dired-get-file-for-visit))) (async-shell-command (format "afinfo --info '%s'" filename))))
(defun yt-dlp (url) "Download the audio, video, or video with subs from a given URL."
	   (interactive "sEnter URL to download: ")
	   (let ((choice	(completing-read "Choose download option: " '("video" "video with subtitles" "audio"))))
		 (cond ((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
			   ((string-equal choice "video with subtitles") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url)))
			   ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x --embed-thumbnail \"%s\"" url))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

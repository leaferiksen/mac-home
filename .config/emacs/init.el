;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:

;; Built in packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (memq system-type '(darwin))
  ;; Enable rendering SF symbols on macOS.
  (set-fontset-font t nil "SF Pro Display" nil 'append)
  (setq browse-url-generic-program "open")
  (setq browse-url-mailto-function 'browse-url-generic)
  (use-package term/ns-win
	:custom (ns-pop-up-frames nil) (mac-option-modifier 'none)))
(use-package emacs ;;core c variables, startup, modus and paragraph
  :hook (window-setup-hook . toggle-frame-maximized) (ns-system-appearance-change-functions . auto-theme)
  :bind (("C-c s" . send-to) ("C-x 2" . split-and-follow-horizontally) ("C-x 3" . split-and-follow-vertically)
		 ("s-o" . find-file) ("s-b" . bookmark-jump) ("s-y" . yt-dlp) ("s-p" . backward-paragraph) ("s-n" . forward-paragraph)
		 ("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil))
  :custom-face
  (default ((t (:family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t (:family "Maple Mono NF CN" :height 140))))
  (variable-pitch ((t (:family "New York" :height 180))))
  :custom ((auto-insert-directory "~/.emacs.d/templates/")
		   (auto-insert-query nil)(backward-delete-char-untabify-method nil)
		   (completion-auto-help nil)
		   (completion-ignore-case t t)
		   (completion-styles '(basic flex partial-completion emacs22))
		   (completions-sort 'historical)
		   (cursor-type 'bar)
		   (custom-file (make-temp-file "~/.cache/emacs/custom"))
		   (delete-by-moving-to-trash t)
		   (delete-selection-mode t)
		   (disabled-command-function nil)
		   (display-line-numbers-type 'relative)
		   (editorconfig-mode t)
		   (electric-pair-mode t)
		   (fill-column 9999)
		   (find-file-visit-truename t)
		   (frame-resize-pixelwise t)
		   (fundamental-mode-hook '(display-line-numbers-mode))
		   (gc-cons-threshold 100000000)
		   (global-auto-revert-mode t) (global-auto-revert-non-file-buffers t)
		   (inhibit-startup-screen t)
		   (insert-directory-program "gls")
		   (isearch-lazy-count t)
		   (line-spacing 0.2)
		   (major-mode-remap-alist '((sh-mode . bash-ts-mode) (mhtml-mode . html-ts-mode) (css-mode . css-ts-mode) (javascript-mode . js-ts-mode) (typescript-mode . typescript-ts-mode) (dockerfile-mode . dockerfile-ts-mode) (json-mode . json-ts-mode) (yaml-mode . yaml-ts-mode)))
		   (make-backup-files nil)
		   (mode-line-collapse-minor-modes '(not lsp-mode flymake-mode))
		   (modus-themes-common-palette-overrides '((fringe unspecified) (bg-line-number-inactive unspecified) (bg-line-number-active unspecified) (underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified) (border-mode-line-active unspecified) (border-mode-line-inactive unspecified) (fg-heading-1 green) (fg-heading-2 green) (fg-heading-3 green) (fg-heading-4 green) (fg-heading-5 green) (fg-heading-6 green)))
		   (modus-themes-headings '((1 . (2.0)) (2 . (1.6)) (3 . (1.2))))
		   (modus-themes-italic-constructs t)
		   (modus-themes-mixed-fonts t)
		   (pixel-scroll-precision-mode t)
		   (prog-mode-hook '(visual-line-mode display-line-numbers-mode completion-preview-mode))
		   (project-mode-line t) (project-vc-extra-root-markers '("project"))
		   (read-buffer-completion-ignore-case t)
		   (read-process-output-max (* 1024 1024))
		   (ring-bell-function 'ignore)
		   (scroll-bar-mode nil)
		   (sentence-end-double-space nil)
		   (shr-fill-text nil)
		   (shr-inhibit-images t)
		   (tab-width 4)
		   (text-mode-hook '(display-line-numbers-mode text-mode-hook-identify))
		   (tool-bar-mode nil)
		   (tooltip-mode nil)
		   (use-dialog-box nil)
		   (visual-fill-column-center-text t)
		   (visual-fill-column-width 100)
		   (warning-minimum-level :error)
		   (which-key-mode t))
  :config
  (setenv "LSP_USE_PLISTS" "true")
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (fido-vertical-mode)
  (auto-insert-mode t) (define-auto-insert "\.html" "insert.html") (define-auto-insert "\.js" "insert.js")
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun vc-amend ()
	"Amend the previous commit title."
	(interactive)
	(vc-checkin nil 'git) (vc-git-log-edit-toggle-amend))
  (defun auto-theme (appearance)
	"Load theme, taking current system APPEARANCE into consideration."
	(mapc #'disable-theme custom-enabled-themes)
	(pcase appearance ('light (load-theme 'modus-operandi t)) ('dark (load-theme 'modus-vivendi t))))
  (defun fix-node ()
	"Unlink and relink node binaries."
	(interactive)
	(async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node"))
  (defun split-and-follow-horizontally ()
	"Move cursor to new window in horizontal split."
	(interactive)
	(split-window-below) (balance-windows) (other-window 1))
  (defun split-and-follow-vertically ()
	"Move cursor to new window in vertical split."
	(interactive)
	(split-window-right) (balance-windows) (other-window 1))
  (defun yt-dlp (url)
	"Download the audio, video, or video with subs from a given URL."
	(interactive "sEnter media source URL: ")
	(let ((choice (completing-read "Download (audio/video/subtitled video ) " '("audio" "video" "subtitled video"))))
	  (cond ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x \"%s\"" url)))
			((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
			((string-equal choice "subtitled video") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url))))))
  (defun http-server ()
	"Start a local server at ./index.html, avoiding port conflicts."
	(interactive)
	(let* ((http-port-offset (if (boundp 'http-port-offset) (1+ http-port-offset) 0)) (http-port (- 9999 http-port-offset)) (filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
	  (start-process filename filename "/opt/homebrew/bin/npx" "http-server" "-o" "-p" (number-to-string http-port))))
  (defun tailwind-server ()
	"Start a tailwind in the current directory, sourcing app.css."
	(interactive)
	(let ((filename (concat "tailwind-server@ <" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
	  (start-process filename filename "npx" "@tailwindcss/cli" "-i" "app.css" "-o" "dist.css" "--watch"))))
(use-package dired
  :bind (:map dired-mode-map (("f" . dired-finder-path) ("e" . 'dwim-shell-commands-macos-open-with) ("C-c p" . 'dwim-shell-commands-md-pdf) ))
  :custom ((dired-listing-switches "-alh --group-directories-first")
		   (dired-clean-confirm-killing-deleted-buffers nil)
		   (dired-create-destination-dirs 'ask)
		   (dired-mode-hook '(display-line-numbers-mode dired-extra-startup nerd-icons-dired-mode dired-omit-mode dired-hide-details-mode))
		   (dired-mouse-drag-files t)
		   (dired-recursive-copies 'always)
		   (dired-omit-files	"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd"))
  :config
  (defun dired-finder-path ()
	"Open Dired in the frontmost Finder window path, if available."
	(interactive)
	(let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
	  (if path (dired (string-trim path)) (message "No Finder window found.")))))

;; Everything else
(use-package agent-shell
  :custom (agent-shell-google-authentication (agent-shell-google-make-authentication :vertex-ai t))
  :bind ("C-c a" . agent-shell-add-region) ("C-c g" . agent-shell-google-start-gemini))
(use-package apheleia
  :custom (apheleia-global-mode t))
(use-package csv-align-mode
  :hook (csv-mode)
  :custom (csv-align-padding 2) (csv-align-max-width 80))
(use-package csv-mode
  :config
  (defun csv-highlight ()
	"Highlight CSV/TSV files."
	(interactive)
	(font-lock-mode 1)
	(let* ((separator (cond ((string-equal (file-name-extension (buffer-file-name)) "tsv") ?\t) (t ?\,)))
		   (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
		   (available-colors '("#848286" nil))
		   (colors (cl-loop for i from 0 below n collect (nth (mod i (length available-colors)) available-colors))))
	  (cl-loop for i from 1 to (1+ n) by 1
			   for c in colors
			   for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
			   do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c))))))))))
(use-package devil
  :config (global-devil-mode)
  (add-to-list 'devil-repeatable-keys `("%k v"))
  (add-to-list 'devil-repeatable-keys `("%k m v"))
  (add-to-list 'devil-repeatable-keys `("%k m d"))
  (add-to-list 'devil-repeatable-keys `("%k m m p" "%k m m n" "%k m m b" "%k m m f" "%k m m a" "%k m m e" "%k m m u" "%k m m d" "%k m m t")))
(use-package nerd-icons-dired)
(use-package dwim-shell-command
  :config
  (defun dwim-shell-command-pbcopy ()
	"Copy file to macOS system clipboard (via pbcopy)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "pbcopy file"
	 "pbcopy < '<<f>>'"
	 :silent-success t))
  (defun dwim-shell-commands-md-pdf ()
	"Convert md(s) to pdf (via pandoc and typst)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert md to pdf"
	 "pandoc --pdf-engine=typst '<<f>>' -o '<<fne>>.pdf'"
	 :extensions "md")))
(use-package elfeed
  :preface (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind (("C-c r" . elfeed)
		 (:map elfeed-search-mode-map ("m" . elfeed-search-show-entry)))
  :custom (elfeed-search-filter "@1-month-ago +unread")
  (elfeed-feeds '(("https://www.kosatenmag.com/home?format=rss" anime)
				  ("https://catandgirl.com/feed/" comics)
				  ("https://existentialcomics.com/rss.xml" comics)
				  ("https://feeds.feedburner.com/nerfnow/full" comics)
				  ("https://thesecretknots.com/feed/" comics)
				  ("https://todon.eu/@PinkWug.rss" comics)
				  ("https://www.berkeleymews.com/feed/" comics)
				  ("https://www.davidrevoy.com/feed/en/rss" comics)
				  ("https://www.penny-arcade.com/feed" comics)
				  ("https://www.smbc-comics.com/comic/rss" comics)
				  ("http://danluu.com/atom.xml" design)
				  ("https://buttondown.com/monteiro/rss" design)
				  ("https://css-tricks.com/feed/" design)
				  ("https://localghost.dev/feed.xml" design)
				  ("https://piccalil.li/feed.xml" design)
				  ("https://rachelandrew.co.uk/feed/" design)
				  ("https://www.smashingmagazine.com/feed/" design)
				  ("https://enikofox.com/feed.xml" gaming)
				  ("https://modmagazine.net/feed.xml" gaming)
				  ("https://panic.com/blog/feed/" gaming)
				  ("https://remapradio.com/rss/" gaming)
				  ("https://tomorrowcorporation.com/feed" gaming)
				  ("https://www.codeweavers.com/blog/?rss=1" gaming)
				  ("https://acidiclight.dev/rss.xml" linux)
				  ("https://asahilinux.org/blog/index.xml" linux)
				  ("https://blog.fyralabs.com/rss/" linux)
				  ("https://blog.xfce.org/feed" linux)
				  ("https://blogs.kde.org/index.xml" linux)
				  ("https://carlschwan.eu/index.xml" linux)
				  ("https://coffee-and-dreams.uk/feed.xml" linux)
				  ("https://drewdevault.com/blog/index.xml" linux)
				  ("https://fireborn.mataroa.blog/rss/" linux)
				  ("https://kde.org/index.xml" linux)
				  ("https://lxqt-project.org/feed.xml" linux)
				  ("https://rabbitictranslator.com/blog/index.xml" linux)
				  ("https://rosenzweig.io/feed.xml" linux)
				  ("https://theevilskeleton.gitlab.io/feed.xml" linux)
				  ("https://thelibre.news/rss/" linux)
				  ("https://www.ypsidanger.com/rss/" linux)
				  ("https://anhvn.com/feed.xml" journals)
				  ("https://annas-archive.li/blog/rss.xml" journals)
				  ("https://blogsystem5.substack.com/feed" journals)
				  ("https://carsonellis.substack.com/feed" journals)
				  ("https://daverupert.com/atom.xml" journals)
				  ("https://hypercritical.co/feeds/main" journals)
				  ("https://indi.ca/rss/" journals)
				  ("https://ryanleetaylor.com/rss.xml" journals)
				  ("https://themkat.net/feed.xml" journals)
				  ("https://tnywndr.cafe/index.xml" journals)
				  ("https://wokescientist.substack.com/feed" journals)
				  ("https://www.jessesquires.com/feed.xml" journals)
				  ("https://www.tinylogger.com/90koil/rss" journals)
				  ("https://www.wordsbywes.ink/feed.xml" journals))))
(use-package elfeed-webkit
  :demand ;; !
  :config (elfeed-webkit-enable)
  :bind (:map elfeed-show-mode-map ("%" . elfeed-webkit-toggle)))
(use-package elisp-mode
  :bind (:map lisp-mode-shared-map ("C-c e" . (lambda () (interactive) (eval-buffer)))))
(use-package exec-path-from-shell
  :if (memq window-system '(ns x))
  :config (exec-path-from-shell-initialize))
(use-package lorem-ipsum)
(use-package lsp-mode
  :hook (html-mode . lsp) (css-mode . lsp) (js-mode . lsp) (typescript-mode . lsp) (tsx-mode . lsp)
  :custom (lsp-enable-indentation nil) (lsp-completion-provider :none)
  :commands lsp)
(use-package lsp-tailwindcss
  :after lsp-mode
  :custom (lsp-tailwindcss-add-on-mode t) (lsp-tailwindcss-skip-config-check t))
;; (use-package lua-mode)
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom ((markdown-mode-hook '(typo-mode visual-line-mode visual-fill-column-mode variable-pitch-mode flyspell-mode))
		   (markdown-enable-wiki-links t)
		   (markdown-hide-markup t)
		   (markdown-unordered-list-item-prefix "- ")
		   (markdown-asymmetric-header t)
		   (markdown-fontify-code-blocks-natively t)
		   (markdown-special-ctrl-a/e t))
  :bind (:map markdown-mode-map (("C-c h" . insert-title) ("C-c d" . insert-date)))
  :config
  (defun insert-date ()
	"Insert an atx heading with today's date in iso format."
	(interactive)
	(insert "## " (format-time-string "%Y-%m-%d") "\n"))
  (defun insert-title ()
	"Insert an atx heading with the name of the file."
	(interactive)
	(insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n")))
(use-package obsidian
  :custom (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind (("s-d" . obsidian-daily-note)
		 (:map markdown-mode-map ([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point))))
(use-package reader
  :vc (:url "https://codeberg.org/divyaranjan/emacs-reader" :make "all"))
(use-package typo)
(use-package undo-fu
  :bind ("s-z" . undo-fu-only-undo) ("s-Z" . undo-fu-only-redo))
(use-package vterm
  :bind ("s-t" . vterm))
;;; init.el ends here

;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
;; test
(defalias 'yes-or-no-p 'y-or-n-p)
(use-package acp
  :ensure t :vc (:url "https://github.com/xenodium/acp.el"))
(use-package agent-shell
  :ensure t :vc (:url "https://github.com/xenodium/agent-shell")
  :custom (agent-shell-google-authentication (agent-shell-google-make-authentication :vertex-ai t))
  :bind ("C-c a" . agent-shell-add-region) ("C-c o" . agent-shell-opencode-start-agent) ("C-c g" . agent-shell-google-start-gemini))
(use-package apheleia
  :ensure t :vc (:url "https://github.com/radian-software/apheleia")
  :custom (apheleia-global-mode t))
(use-package auth-source
  :custom (auth-sources "~/.authinfo.gpg"))
(use-package autoinsert
  :custom (auto-insert-directory "~/.emacs.d/templates/") (auto-insert-query nil)
  :config (auto-insert-mode t) (define-auto-insert "\.html" "insert.html") (define-auto-insert "\.js" "insert.js"))
(use-package autorevert
  :custom (global-auto-revert-mode t) (global-auto-revert-non-file-buffers t))
(use-package browse-url
  :custom (browse-url-generic-program "open") (browse-url-mailto-function 'browse-url-generic))
(use-package completion-preview
  :hook (html-mode prog-mode)
  :bind (:map completion-preview-active-mode-map (("M-n" . completion-preview-next-candidate) ("M-p" . completion-preview-prev-candidate))))
(use-package csv-align-mode
  :hook (csv-mode)
  :custom (csv-align-padding 2) (csv-align-max-width 60))
(use-package csv-mode
  :ensure t :vc (:url "https://github.com/emacsmirror/csv-mode")
  :hook (csv-mode . csv-highlight)
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
(use-package delsel
  :custom (delete-selection-mode t))
(use-package dired
  :bind (:map dired-mode-map (("<mouse-1>" . nil) ("<mouse-2>" . nil) ("SPC" . 'quicklook) ("C-c p" . 'dwim-shell-commands-md-pdf)))
  :custom (dired-clean-confirm-killing-deleted-buffers nil) (dired-create-destination-dirs 'ask) (dired-listing-switches "-alh --group-directories-first") (dired-mouse-drag-files t) (dired-recursive-copies 'always)
  :config
  (use-package dired-hide-details
	:hook (dired-mode))
  (use-package dired-omit-mode
	:hook (dired-mode))
  ;; (use-package nerd-icons
  ;;   :ensure t :vc (:url "https://github.com/rainstormstudio/nerd-icons.el"))
  ;; (use-package nerd-icons-dired
  ;; :ensure t :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired")
  ;; :hook (dired-mode))
  (defun afinfo ()
	"Get metadata for focused file."
	(interactive)
	(let ((filename (dired-get-file-for-visit))) (async-shell-command (format "afinfo --info '%s'" filename))))
  (defun dired-finder-path ()
	"Open Dired in the frontmost Finder window path, if available."
	(interactive)
	(let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
	  (if path (dired (string-trim path)) (message "No Finder window found."))))
  (defun quicklook ()
	"QuickLook the currently selected file in Dired."
	(interactive)
	(let ((filename (dired-get-file-for-visit))) (shell-command (concat "qlmanage -p \"" filename "\" > /dev/null 2>&1"))))
  :bind (:map dired-mode-map ("C-c f" . dired-finder-path) ("C-c c" . dwim-shell-command-pbcopy)))
(use-package dired-x
  :custom (dired-omit-files	"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd"))
(use-package display-line-numbers
  :custom (display-line-numbers-type 'relative))
(use-package dwim-shell-command
  :ensure t :vc (:url "https://github.com/xenodium/dwim-shell-command")
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
(use-package editorconfig
  :custom (editorconfig-mode t))
(use-package ef-themes
  :ensure t :vc (:url "https://github.com/protesilaos/ef-themes")
  :custom ((modus-themes-common-palette-overrides
			'((underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified)
			  (border-mode-line-active unspecified) (border-mode-line-inactive unspecified)
			  (fg-heading-1 green-intense) (fg-heading-2 green) (fg-heading-3 green-faint)
			  (fg-heading-4 fg-sage) (fg-heading-5 fg-sage) (fg-heading-6 fg-sage)))
		   (modus-themes-headings '((1 . (1.8)) (2 . (1.6)) (3 . (1.4)) (4 . (1.2))))
		   (modus-themes-italic-constructs t)
		   (modus-themes-mixed-fonts t)))
(use-package elec-pair
  :custom (electric-pair-mode t))
(use-package elfeed
  :ensure t :vc (:url "https://github.com/skeeto/elfeed")
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
  :ensure t :vc (:url "https://github.com/fritzgrabo/elfeed-webkit")
  :demand ;; !
  :config (elfeed-webkit-enable)
  :bind (:map elfeed-show-mode-map ("%" . elfeed-webkit-toggle)))
(use-package elgrep ;; obsidian dependency
  :ensure t :vc (:url "https://github.com/TobiasZawada/elgrep"))
(use-package elisp-mode
  :bind (:map lisp-mode-shared-map ("C-c e" . (lambda () (interactive) (eval-buffer)))))
(use-package emacs ;;core c variables, startup, modus and paragraph
  :hook (ns-system-appearance-change-functions . auto-theme)
  :bind (("C-z" . nil) ("M-z" . nil)("C-x 2" . split-and-follow-horizontally) ("C-x 3" . split-and-follow-vertically)
		 ("C-c t" . ghostty) ("C-c y" . yt-dlp) ("C-c p" . backward-paragraph) ("C-c n" . forward-paragraph)
		 ("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil))
  :custom-face
  (default ((t (:family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t (:family "Maple Mono NF CN" :height 140))))
  (variable-pitch ((t (:family "New York" :height 180))))
  :custom ((completion-ignore-case t t)
		   (cursor-type 'bar)
		   (delete-by-moving-to-trash t)
		   (fill-column 9999)
		   (frame-resize-pixelwise t)
		   (gc-cons-threshold 100000000)
		   (inhibit-startup-screen t)
		   (initial-buffer-choice "~/Documents/")
		   (line-spacing 0.1)
		   (mode-line-collapse-minor-modes '(not lsp-mode flymake-mode))
		   (mac-command-modifier 'meta)
		   (mac-option-modifier 'none)
		   (read-buffer-completion-ignore-case t)
		   (read-process-output-max (* 1024 1024))
		   (ring-bell-function 'ignore)
		   (sentence-end-double-space nil)
		   (tab-width 4)
		   (tool-bar-mode nil)
		   (use-dialog-box nil))
  :config 
  (defun auto-theme (appearance)
	"Load theme, taking current system APPEARANCE into consideration."
	(mapc #'disable-theme custom-enabled-themes)
	(pcase appearance ('light (load-theme 'ef-summer t)) ('dark (load-theme 'ef-winter t))))
  (defun fix-node ()
	"Unlink and relink node binaries."
	(interactive)
	(async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node"))
  (defun ghostty ()
	"Open current directory in Ghostty."
	(interactive)
	(shell-command (concat "open -a Ghostty --args --working-directory=" "\""(expand-file-name default-directory)"\"")))
  (defun split-and-follow-horizontally ()
	"Move cursor to new window in horizontal split."
	(interactive)
	(split-window-below) (balance-windows) (other-window 1))
  (defun split-and-follow-vertically ()
	"Move cursor to new window in vertical split."
	(interactive)
	(split-window-right) (balance-windows) (other-window 1))
  (defun unpackaged/sort-sexps (beg end)
	"Sort sexps from BEG to END. Comments stay with the code below."
	(interactive "r")
	(cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n")))) (goto-char (match-end 0))))
			  (skip-both ()
				(while (cond ((or (nth 4 (syntax-ppss))
								  (ignore-errors (save-excursion (forward-char 1) (nth 4 (syntax-ppss)))))
							  (forward-line 1))
							 ((looking-at (rx (1+ (or space "\n")))) (goto-char (match-end 0)))))))
	  (save-excursion
		(save-restriction (narrow-to-region beg end) (goto-char beg) (skip-both)
						  (cl-destructuring-bind (sexps markers)
							  (cl-loop do (skip-whitespace)
									   for start = (point-marker)
									   for sexp = (ignore-errors (read (current-buffer)))
									   for end = (point-marker)
									   while sexp
									   ;; Collect the real string, then one used for sorting.
									   collect (cons (buffer-substring (marker-position start) (marker-position end))
													 (save-excursion
													   (goto-char (marker-position start))
													   (skip-both)
													   (buffer-substring (point) (marker-position end))))
									   into sexps
									   collect (cons start end)
									   into markers
									   finally return (list sexps markers))
							(setq sexps (sort sexps (lambda (a b) (string< (cdr a) (cdr b)))))
							(cl-loop for sexp-pair in sexps
									 for marker-pair in markers
									 do (let* ((real (car sexp-pair)) (start (car marker-pair)) (end (cdr marker-pair)))
										  (goto-char (marker-position start))
										  (insert-before-markers real)
										  (delete-region (point) (marker-position end)))))))))
  (defun yt-dlp (url)
	"Download the audio, video, or video with subs from a given URL."
	(interactive "sEnter media source URL: ")
	(let ((choice (completing-read "Download (audio/video/subtitled video ) " '("audio" "video" "subtitled video"))))
	  (cond ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x \"%s\"" url)))
			((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
			((string-equal choice "subtitled video") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url)))))))
;; (use-package emmet-mode
;;   :ensure t :vc (:url "https://github.com/smihica/emmet-mode/")
;;   :hook (html-mode css-mode)
;;   :bind ((:map html-mode ("C-c e" . emmet-expand-line))
;; 		 (:map css-mode ("C-c e" . emmet-expand-line))))
(use-package epg-config
  :custom (epg-pinentry-mode 'loopback))
(use-package exec-path-from-shell
  :if (memq window-system '(ns x))
  :ensure t :vc (:url "https://github.com/purcell/exec-path-from-shell")
  :config (exec-path-from-shell-initialize))
(use-package f ;; lsp-mode dependency
  :ensure t :vc (:url "https://github.com/rejeep/f.el"))
(use-package files
  :custom ((find-file-visit-truename t)
		   (insert-directory-program "gls")
		   (large-file-warning-threshold nil)
		   (major-mode-remap-alist '((sh-mode . bash-ts-mode) (css-mode . css-ts-mode) (dockerfile-mode . dockerfile-ts-mode) (mhtml-mode . html-ts-mode) (javascript-mode . js-ts-mode) (json-mode . json-ts-mode) (typescript-mode . typescript-ts-mode) (yaml-mode . yaml-ts-mode)))
		   (make-backup-files nil)
		   (trash-directory "~/.Trash")))
(use-package ht ;; lsp-mode dependency
  :ensure t :vc (:url "https://github.com/Wilfred/ht.el"))
(use-package html-ts-mode
  :config
  (defun http-server ()
	"Start a local server at ./index.html, avoiding port conflicts."
	(interactive)
	(let* ((http-port-offset (if (boundp 'http-port-offset) (1+ http-port-offset) 0)) (http-port (- 9999 http-port-offset)) (filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
	  (start-process filename filename "/opt/homebrew/bin/npx" "http-server" "-o" "-p" (number-to-string http-port)))))
(use-package icomplete
  :custom ((completion-styles '(basic flex partial-completion emacs22))
		   (completions-sort 'historical))
  :config (fido-vertical-mode))
(use-package isearch
  :custom (isearch-lazy-count t) (lazy-count-prefix-format nil) (lazy-count-suffix-format "   (%s/%s)"))
(use-package jinx
  :ensure t :vc (:url "https://github.com/minad/jinx")
  :hook (markdown-mode)
  :bind (:map jinx-mode-map ("C-c c" . jinx-correct))
  :custom (jinx-languages "en_US ja-JP")
  :config
  (defun insert-date ()
	"Insert an atx heading with today's date in iso format."
	(interactive)
	(insert "## " (format-time-string "%Y-%m-%d") "\n"))
  (defun insert-title ()
	"Insert an atx heading with the name of the file."
	(interactive)
	(insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n")))
(use-package lorem-ipsum
  :ensure t :vc (:url "https://github.com/jschaf/emacs-lorem-ipsum"))
(use-package lsp-completion
  :custom (lsp-completion-provider :none))
(use-package lsp-mode
  :ensure t :vc (:url "https://github.com/emacs-lsp/lsp-mode")
  :hook (html-mode . lsp) (css-mode . lsp) (js-mode . lsp) (typescript-mode . lsp) (tsx-mode . lsp)
  :custom (lsp-enable-indentation nil)
  :commands lsp)
(use-package lsp-tailwindcss
  :ensure t :vc (:url "https://github.com/merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :custom (lsp-tailwindcss-add-on-mode t) (lsp-tailwindcss-skip-config-check t) (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server")
  :config
  (defun tailwind-server ()
	"Start a tailwind in the current directory, sourcing app.css."
	(interactive)
	(let ((filename (concat "tailwind-server@ <" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
	  (start-process filename filename "/opt/homebrew/bin/npx" "@tailwindcss/cli" "-i" "app.css" "-o" "dist.css" "--watch"))))
(use-package lua-mode
  :ensure t :vc (:url "https://github.com/immerrr/lua-mode"))
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-enable-wiki-links t) (markdown-hide-urls t) (markdown-hide-markup t) (markdown-asymmetric-header t)
  :bind (:map markdown-mode-map (("C-c h" . insert-title) ("C-c d" . insert-date))))
(use-package moody
  :ensure t :vc (:url "https://github.com/tarsius/moody")
  :config (moody-replace-mode-line-front-space) (moody-replace-mode-line-buffer-identification) (moody-replace-vc-mode))
(use-package novice
  :custom (disabled-command-function nil))
(use-package obsidian
  :ensure t :vc (:url "https://github.com/licht1stein/obsidian.el")
  :custom (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind (("C-c d" . obsidian-daily-note)
		 (:map markdown-mode-map ([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point))))
(use-package osawm
  :vc (:url "https://github.com/andykuszyk/osawm.el")
  :config (global-osawm-mode)
  :bind ("C-c w" . (lambda () (interactive) (osawm-launch-chrome "" "Google Chrome" 'normal))))
(use-package pixel-scroll
  :custom (pixel-scroll-precision-mode t))
(use-package project
  :custom (project-mode-line t) (project-vc-extra-root-markers '("project")))
(use-package repeat
  :custom (repeat-mode t))
(use-package s ;; f dependency
  :ensure t :vc (:url "https://github.com/magnars/s.el"))
(use-package scroll-bar
  :custom (scroll-bar-mode nil))
(use-package send-to
  :bind ("C-c s" . send-to))
(use-package shell-maker
  :ensure t :vc (:url "https://github.com/xenodium/shell-maker"))
(use-package shr
  :custom (shr-fill-text nil) (shr-inhibit-images t))
(use-package simple
  :custom (backward-delete-char-untabify-method nil) (column-number-mode t))
(use-package snow
  :ensure t :vc (:url "https://github.com/alphapapa/snow.el")
  :custom (snow-pile-factor 1))
(use-package term/ns-win
  :if (memq window-system '(ns))
  :custom (ns-pop-up-frames nil))
(use-package tooltip
  :custom (tooltip-mode nil)) (use-package treesit-langs
  :ensure t :vc (:url "https://github.com/emacs-tree-sitter/treesit-langs"))
(use-package typo
  :ensure t :vc (:url "https://github.com/jorgenschaefer/typoel")
  :hook (markdown-mode))
(use-package undo-fu
  :vc (:url "https://github.com/emacsmirror/undo-fu")
  :bind ("M-z" . undo-fu-only-undo) ("M-Z" . undo-fu-only-redo))
(use-package valign
  :ensure t :vc (:url "https://github.com/casouri/valign")
  :custom (valign-fancy-bar t)
  :hook (markdown-mode))
(use-package variable-pitch
  :hook (markdown-mode) (variable-pitch-mode . (lambda () (setq-local line-spacing 0.4))))
(use-package vc
  :config
  (defun vc-amend ()
	"Amend the previous commit title."
	(interactive)
	(vc-checkin nil 'git) (vc-git-log-edit-toggle-amend)))
(use-package visual-fill-column
  :ensure t :vc (:url "https://codeberg.org/joostkremers/visual-fill-column")
  :custom (visual-fill-column-center-text t) (visual-fill-column-width 80)
  :hook (markdown-mode))
(use-package visual-line-mode
  :hook (markdown-mode html-mode prog-mode))
(use-package warnings
  :custom (warning-minimum-level :error))
(use-package which-key
  :custom (which-key-mode t))
(use-package yasnippet
  :ensure t :vc (:url "https://github.com/joaotavora/yasnippet"))
;;; init.el ends here

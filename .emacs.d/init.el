;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(defalias 'yes-or-no-p 'y-or-n-p)
(defun afinfo ()
  "Get metadata for focused file."
  (interactive)
  (let ((filename (dired-get-file-for-visit))) (async-shell-command (format "afinfo --info '%s'" filename))))
(defun auto-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance ('light (load-theme 'modus-operandi-tinted t)) ('dark (load-theme 'modus-vivendi-tinted t))))
(defun dired-finder-path ()
  "Open Dired in the frontmost Finder window path, if available."
  (interactive)
  (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
	(if path (dired (string-trim path)) (message "No Finder window found."))))
(defun fix-node ()
  "Unlink and relink node binaries."
  (interactive)
  (async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node"))
(defun ghostty ()
  "Open current directory in Ghostty."
  (interactive)
  (shell-command (concat "open -a Ghostty --args --working-directory=" "\""(expand-file-name default-directory)"\"")))
(defun http-server ()
  "Start a local server at ./index.html, avoiding port conflicts."
  (interactive)
  (let* ((http-port-offset (if (boundp 'http-port-offset) (1+ http-port-offset) 0))
         (http-port (- 9999 http-port-offset))
         (filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
    (start-process filename filename "/opt/homebrew/bin/npx" "http-server" "-o" "-p" (number-to-string http-port))))
(defun insert-date ()
  "Insert an atx heading with today's date in iso format."
  (interactive)
  (insert "## " (format-time-string "%Y-%m-%d") "\n"))
(defun insert-title ()
  "Insert an atx heading with the name of the file."
  (interactive)
  (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
(defun quicklook ()
  "QuickLook the currently selected file in Dired."
  (interactive)
  (let ((filename (dired-get-file-for-visit))) (shell-command (concat "qlmanage -p \"" filename "\" > /dev/null 2>&1"))))
(defun send-to-self	(message)
  "Send a MESSAGE to myself."
  (interactive "sMessage to send: ")
  (let ((message
		 (or message "")))
	(shell-command (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'" (shell-quote-argument message)))))
(defun split-and-follow-horizontally ()
  "Move cursor to new window in horizontal split."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun split-and-follow-vertically ()
  "Move cursor to new window in vertical split."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(defun tailwind-server ()
  "Start a tailwind in the current directory, sourcing app.css."
  (interactive)
  (let ((filename (concat "tailwind-server@ <" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
	(start-process filename filename "/opt/homebrew/bin/npx" "@tailwindcss/cli" "-i" "app.css" "-o" "dist.css" "--watch")))
(defun unpackaged/sort-sexps (beg end)
  "Sort sexps from BEG to END. Comments stay with the code below."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
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
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))
(defun vc-amend ()
  "Amend the previous commit title."
  (interactive)
  (vc-checkin nil 'git)
  (vc-git-log-edit-toggle-amend))

(defun wrap-urls-with-parentheses
	(start end)
  "Wrap quoted URLs with parentheses from START to END."
  (interactive "r")
  (save-excursion
	(goto-char start)
	(while
		(re-search-forward "\"\\(https?://[^\"]+\\)\"" end t)
	  (replace-match "(\"\\1\")"))))
(defun yt-dlp (url)
  "Download the audio, video, or video with subs from a given URL."
  (interactive "sEnter media source URL: ")
  (let ((choice (completing-read "Download (audio/video/subtitled video ) " '("audio" "video" "subtitled video"))))
	(cond ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x \"%s\"" url)))
		  ((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
		  ((string-equal choice "subtitled video") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url))))))
(use-package acp
  :ensure t :vc (:url "https://github.com/xenodium/acp.el"))
(use-package agent-shell
  :ensure t :vc (:url "https://github.com/xenodium/agent-shell")
  :custom (agent-shell-google-authentication (agent-shell-google-make-authentication :vertex-ai t))
  :bind (("C-c g" . agent-shell-google-start-gemini)
		 ("C-c a" . agent-shell-add-region)))
(use-package apheleia
  :ensure t :vc (:url "https://github.com/radian-software/apheleia")
  :custom (apheleia-global-mode t))
(use-package auth-source
  :custom (auth-sources "~/.authinfo.gpg"))
(use-package autorevert
  :custom ((global-auto-revert-mode t)
		   (global-auto-revert-non-file-buffers t)))
(use-package bookmark
  :bind ("s-b" . bookmark-jump))
(use-package browse-url
  :custom ((browse-url-generic-program "open")
		   (browse-url-mailto-function 'browse-url-generic)))
(use-package completion-preview
  :hook (html-mode prog-mode)
  :bind (:map completion-preview-active-mode-map (("M-n" . completion-preview-next-candidate)
												  ("M-p" . completion-preview-prev-candidate))))
(use-package delsel
  :custom (delete-selection-mode t))
(use-package dired
  :bind (:map dired-mode-map (("<mouse-1>" . nil)
							  ("<mouse-2>" . nil)
							  ("SPC" . 'quicklook)
							  ("C-c p" . 'dwim-shell-commands-md-pdf)))
  :custom ((dired-clean-confirm-killing-deleted-buffers nil)
		   (dired-create-destination-dirs 'ask)
		   (dired-listing-switches "-alh --group-directories-first")
		   (dired-mouse-drag-files t)
		   
		   (dired-recursive-copies 'always)))
(use-package dired-hide-details
  :hook (dired-mode))
(use-package dired-omit-mode
  :hook (dired-mode))
(use-package dired-x
  :custom (dired-omit-files	"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd"))
(use-package dwim-shell-command
  :ensure t :vc (:url "https://github.com/xenodium/dwim-shell-command")
  :config
  (defun dwim-shell-commands-md-pdf ()
	"Convert md(s) to pdf (via pandoc and typst)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to PDF"
	 "pandoc --pdf-engine=typst '<<f>>' -o '<<fne>>.pdf'"
	 :extensions "md")))
(use-package editorconfig
  :custom (editorconfig-mode t))
(use-package elec-pair
  :custom (electric-pair-mode t))
(use-package elfeed
  :ensure t :vc (:url "https://github.com/skeeto/elfeed")
  :preface (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind (("C-c r" . elfeed)
		 (:map elfeed-search-mode-map ("m" . elfeed-search-show-entry)))
  :custom ((elfeed-feeds '(("https://buttondown.com/monteiro/rss" design) ("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals) ("https://indi.ca/rss/" journals)))
		   (elfeed-search-filter "@1-month-ago +unread")))
(use-package elfeed-webkit
  :ensure t :vc (:url "https://github.com/fritzgrabo/elfeed-webkit")
  :demand ;; !
  :config (elfeed-webkit-enable)
  :bind (:map elfeed-show-mode-map
              ("%" . elfeed-webkit-toggle)))
(use-package elgrep ;; obsidian dependency
  :ensure t :vc (:url "https://github.com/TobiasZawada/elgrep"))
(use-package emacs ;;core c variables, startup, modus and paragraph
  :hook (ns-system-appearance-change-functions . auto-theme)
  :bind (("C-x 2" . split-and-follow-horizontally) ("C-x 3" . split-and-follow-vertically) ("s-t" . ghostty) ("s-y" . yt-dlp)  ("C-z" . nil)("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil)) ;; Unmap default text rescaling
  :custom-face (default ((t (:family "Maple Mono NF CN" :height 160)))) (variable-pitch ((t (:family "New York" :height 200))))
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
		   (modus-themes-common-palette-overrides '((fringe unspecified) (border-mode-line-active unspecified) (border-mode-line-inactive unspecified) (bg-tab-bar bg-main) (bg-tab-current bg-active) (bg-tab-other bg-dim) (fg-heading-1 green-intense) (fg-heading-2 green) (fg-heading-3 green-faint) (fg-heading-4 fg-sage) (fg-heading-5 fg-sage) (fg-heading-6 fg-sage)))
		   (modus-themes-headings '((1 . (1.8)) (2 . (1.6)) (3 . (1.4)) (4 . (1.2))))
		   (modus-themes-italic-constructs t)
		   (read-buffer-completion-ignore-case t)
		   (read-process-output-max (* 1024 1024))
		   (ring-bell-function 'ignore)
		   (sentence-end-double-space nil)
		   (tab-width 4)
		   (tool-bar-mode nil)
		   (use-dialog-box nil)))
(use-package emmet-mode
  :ensure t :vc (:url "https://github.com/smihica/emmet-mode/")
  :hook (html-mode css-mode)
  :bind (:map html-mode ("C-c e" . emmet-expand-line)) (:map css-mode ("C-c e" . emmet-expand-line)))
(use-package epg-config
  :custom (epg-pinentry-mode 'loopback))
(use-package f ;; lsp-mode dependency
  :ensure t :vc (:url "https://github.com/rejeep/f.el"))
(use-package files
  :custom ((find-file-visit-truename t)
		   (insert-directory-program "gls")
		   (large-file-warning-threshold nil)
		   (major-mode-remap-alist '((sh-mode . bash-ts-mode) (css-mode . css-ts-mode) (dockerfile-mode . dockerfile-ts-mode) (mhtml-mode . html-ts-mode) (javascript-mode . js-ts-mode) (json-mode . json-ts-mode) (typescript-mode . typescript-ts-mode) (yaml-mode . yaml-ts-mode)))
		   (make-backup-files nil)
		   (trash-directory "~/.Trash"))
  :bind ("s-o" . find-file))
(use-package flymake
  :hook (emacs-lisp-mode))
(use-package ht ;; lsp-mode dependency
  :ensure t :vc (:url "https://github.com/Wilfred/ht.el"))
(use-package isearch
  :custom (isearch-lazy-count t) (lazy-count-prefix-format nil) (lazy-count-suffix-format "   (%s/%s)"))
(use-package jinx
  :ensure t :vc (:url "https://github.com/minad/jinx")
  :hook (markdown-mode)
  :bind ("C-c c" . jinx-correct)
  :custom (jinx-languages "en_US ja-JP"))
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
  :custom (lsp-tailwindcss-add-on-mode t) (lsp-tailwindcss-skip-config-check t) (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server"))
(use-package lua-mode
  :ensure t :vc (:url "https://github.com/immerrr/lua-mode"))
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-enable-wiki-links t) (markdown-hide-urls t) (markdown-hide-markup t) (markdown-asymmetric-header t)
  :custom-face (markdown-link-face ((t (:underline nil :inherit link)))) (markdown-code-face ((t (:family "Maple Mono NF CN" :inherit default)))) (markdown-table-face ((t (:inherit default))))
  :bind (:map markdown-mode-map (("C-c h" . insert-title) ("C-c d" . insert-date) ([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point))))
(use-package moody
  :ensure t :vc (:url "https://github.com/tarsius/moody")
  :config (moody-replace-mode-line-front-space) (moody-replace-mode-line-buffer-identification) (moody-replace-vc-mode))
;; (use-package nerd-icons
;;   :ensure t :vc (:url "https://github.com/rainstormstudio/nerd-icons.el"))
;; (use-package nerd-icons-dired
;;   :ensure t :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired")
;;   :hook (dired-mode))
(use-package novice
  :custom (disabled-command-function nil))
(use-package obsidian
  :ensure t :vc (:url "https://github.com/licht1stein/obsidian.el")
  :custom (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind ("s-d" . obsidian-daily-note))
(use-package pixel-scroll
  :custom (pixel-scroll-precision-mode t))
(use-package project
  :custom ((project-mode-line t)
		   (project-vc-extra-root-markers '("project"))))
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
(use-package tooltip
  :custom (tooltip-mode nil))
(use-package treesit-langs
  :ensure t :vc (:url "https://github.com/emacs-tree-sitter/treesit-langs"))
(use-package typo
  :ensure t :vc (:url "https://github.com/jorgenschaefer/typoel")
  :hook (text-mode))
(use-package undo-fu
  :ensure t :vc (:url "https://github.com/emacsmirror/undo-fu")
  :bind ("s-z" . undo-fu-only-undo) ("s-Z" . undo-fu-only-redo))
(use-package use-package-core
  :custom (use-package-vc-prefer-newest t)) ;; (use-package-always-ensure t)
(use-package valign
  :ensure t :vc (:url "https://github.com/casouri/valign")
  :custom (valign-fancy-bar t)
  :hook (markdown-mode))
(use-package variable-pitch
  :hook (text-mode) (variable-pitch-mode . (lambda () (setq-local line-spacing 0.4))))
(use-package visual-fill-column
  :ensure t :vc (:url "https://codeberg.org/joostkremers/visual-fill-column")
  :custom (visual-fill-column-center-text t) (visual-fill-column-width 80)
  :hook (text-mode))
(use-package warnings
  :custom (warning-minimum-level :error))
(use-package yasnippet
  :ensure t :vc (:url "https://github.com/joaotavora/yasnippet"))
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
	:ensure t :vc (:url "https://github.com/purcell/exec-path-from-shell")
    :config (exec-path-from-shell-initialize))
  (use-package term/ns-win
	:custom (ns-pop-up-frames nil)))
;;; init.el ends here

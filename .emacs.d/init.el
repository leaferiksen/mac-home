;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(defalias 'yes-or-no-p 'y-or-n-p)
(defun afinfo () "Get metadata for focused file."
	   (interactive)
	   (let ((filename (dired-get-file-for-visit))) (async-shell-command (format "afinfo --info '%s'" filename))))
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
(defun config () "Open init.el."
	   (interactive)
	   (find-file "~/.emacs.d/init.el"))
(defun dired-finder-path () "Open Dired in the frontmost Finder window path, if available."
	   (interactive)
	   (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
		 (if path (dired (string-trim path)) (message "No Finder window found."))))
(defun fix-node () "Unlink and relink node binaries."
	   (interactive)
	   (async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node"))
(defun ghostty () "Open current directory in Ghostty."
	   (interactive)
	   (shell-command (concat "open -a Ghostty --args --working-directory=" "\""(expand-file-name default-directory)"\"")))
(defun http-server () "Start a local server at ./index.html, avoiding port conflicts."
	   (interactive)
	   (unless (boundp 'http-port-offset)
		 (setq http-port-offset 0))
	   (setq http-port (- 9999 http-port-offset))
	   (setq http-port-offset (1+ http-port-offset))
	   (let ((filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
		 (start-process filename filename "/opt/homebrew/bin/npx" "http-server" "-o" "-p" (number-to-string http-port))))
(defun insert-date () "Insert an atx heading with today's date in iso format."
	   (interactive)
	   (insert "## " (format-time-string "%Y-%m-%d") "\n"))
(defun insert-title () "Insert an atx heading with the name of the file."
	   (interactive)
	   (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
(defun quicklook () "QuickLook the currently selected file in Dired."
	   (interactive)
	   (let ((filename (dired-get-file-for-visit))) (shell-command (concat "qlmanage -p \"" filename "\" > /dev/null 2>&1"))))
(defun send-to-self	(message) "Send a MESSAGE to myself."
	   (interactive "sMessage to send: ")
	   (let ((message
			  (or message "")))			; Ensure message isn't nil
		 (shell-command (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'" (shell-quote-argument message)))))
(defun split-and-follow-horizontally () "Move cursor to new window in horizontal split."
	   (interactive)
	   (split-window-below)
	   (balance-windows)
	   (other-window 1))
(defun split-and-follow-vertically () "Move cursor to new window in vertical split."
	   (interactive)
	   (split-window-right)
	   (balance-windows)
	   (other-window 1))
(defun tailwind-server () "Start a tailwind in the current directory, sourcing app.css."
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
(defun vc-amend () "Amend the previous commit title."
	   (interactive)
	   (vc-checkin nil 'git)
	   (vc-git-log-edit-toggle-amend))
(defun wrap-urls-with-parentheses (start end) "Wrap quoted URLs with parentheses from START to END."
	   (interactive "r")
	   (save-excursion (goto-char start)
					   (while (re-search-forward "\"\\(https?://[^\"]+\\)\"" end t) (replace-match "(\"\\1\")"))))
(defun yt-dlp (url) "Download the audio, video, or video with subs from a given URL."
	   (interactive "sEnter media source URL: ")
	   (let ((choice (completing-read "Download (audio/video/subtitled video ) " '("audio" "video" "subtitled video"))))
		 (cond ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x \"%s\"" url)))
			   ((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
			   ((string-equal choice "subtitled video") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url))))))
(use-package apheleia
  :vc (:url "https://github.com/radian-software/apheleia")
  :custom (apheleia-global-mode t))
(use-package completion-preview
  :hook (html-mode prog-mode)
  :bind (:map completion-preview-active-mode-map (("M-n" . completion-preview-next-candidate)
												  ("M-p" . completion-preview-prev-candidate))))
(use-package dired
  ;; :after ls-lisp
  :preface (require 'ls-lisp)
  :bind (:map dired-mode-map (("<mouse-1>" . nil)
							  ("<mouse-2>" . nil)
							  ("SPC" . 'quicklook)))
  :custom ((delete-by-moving-to-trash t)
		   (dired-clean-confirm-killing-deleted-buffers nil)
		   (dired-create-destination-dirs 'ask)
		   (dired-listing-switches "-Alh")
		   (dired-mouse-drag-files t)
		   (dired-omit-files	"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
		   (dired-recursive-copies 'always)))
(use-package dired-hide-details
  :hook (dired-mode))
(use-package dired-omit-mode
  :hook (dired-mode))
(use-package display-line-numbers
  :hook (html-mode prog-mode))
(use-package dwim-shell-command
  :vc (:url "https://github.com/xenodium/dwim-shell-command"))
(use-package elfeed
  :vc (:url "https://github.com/skeeto/elfeed")
  :preface (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind ("C-c e" . elfeed)
  :custom ((elfeed-feeds '(("https://buttondown.com/monteiro/rss" design) ("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://annas-archive.li/blog/rss.xml" journals webkit) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals) ("https://indi.ca/rss/" journals)))
		   (elfeed-search-filter "@1-month-ago +unread")))
(use-package elfeed-webkit
  :vc (:url "https://github.com/fritzgrabo/elfeed-webkit")
  :after elfeed
  :demand ;; !
  :config (elfeed-webkit-enable)
  :bind (:map elfeed-show-mode-map
              ("t" . elfeed-webkit-toggle)))
(use-package elgrep ;; obsidian dependency
  :vc (:url "https://github.com/TobiasZawada/elgrep"))
(use-package emacs
  :hook (ns-system-appearance-change-functions . auto-theme)
  :bind (("C-x 2" . split-and-follow-horizontally) ("C-x 3" . split-and-follow-vertically)
		 ("s-o" . find-file) ("s-b" . bookmark-jump)
		 ("s-," . config) ("s-t" . ghostty) ("s-y" . yt-dlp)
		 ("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil)) ;; Unmap default text rescaling
  :custom-face (default ((t (:family "Maple Mono NF CN" :height 160)))) (variable-pitch ((t (:family "New York" :height 220))))
  :custom ((auth-sources "~/.authinfo.gpg")
		   (auto-package-update-delete-old-versions t)
		   (backward-delete-char-untabify-method nil)
		   (browse-url-generic-program "open")
		   (browse-url-mailto-function 'browse-url-generic)
		   (completion-ignore-case t t)
		   (context-menu-mode t)
		   (cursor-type 'bar)
		   (custom-file (make-temp-file "~/.cache/emacs/custom"))
		   (delete-selection-mode t)
		   (disabled-command-function nil)
		   (editorconfig-mode t)
		   (electric-pair-mode t)
		   (epg-pinentry-mode 'loopback)
		   (fill-column 9999)
		   (frame-resize-pixelwise t)
		   (gc-cons-threshold 100000000)
		   (global-auto-revert-mode t)
		   (global-visual-line-mode t)
		   ;; (insert-directory-program "gls")
		   (inhibit-startup-screen t)
		   (initial-buffer-choice "~/Documents/")
		   (isearch-lazy-count t)
		   (large-file-warning-threshold nil)
		   (lazy-count-prefix-format nil)
		   (lazy-count-suffix-format "   (%s/%s)")
		   (line-spacing 0.1)
		   (major-mode-remap-alist '((css-mode . css-ts-mode)
									 (dockerfile-mode . dockerfile-ts-mode)
									 (mhtml-mode . html-ts-mode)
									 (javascript-mode . js-ts-mode)
									 (json-mode . json-ts-mode)
									 (typescript-mode . typescript-ts-mode)
									 (yaml-mode . yaml-ts-mode)))
		   (make-backup-files nil)
		   (mode-line-collapse-minor-modes '(not lsp-mode flymake-mode))
		   (mouse-wheel-progressive-speed nil)
		   (ns-pop-up-frames nil)
		   (pixel-scroll-precision-mode t)
		   (project-mode-line t)
		   (project-vc-extra-root-markers '("project"))
		   (read-buffer-completion-ignore-case t)
		   (read-process-output-max (* 1024 1024))
		   (repeat-mode 1)
		   (ring-bell-function 'ignore)
		   (scroll-bar-mode nil)
		   (sentence-end-double-space nil)
		   (shr-fill-text nil)
		   (shr-inhibit-images t)
		   (tab-width 4)
		   (tool-bar-mode nil)
		   (tooltip-mode nil)
		   (use-dialog-box nil)
		   (use-package-always-ensure t)
		   (use-package-vc-prefer-newest t)
		   (warning-minimum-level :error)
		   (find-file-visit-truename t)
		   (global-auto-revert-non-file-buffers t)
		   (trash-directory "~/.Trash")))
(use-package emmet-mode
  :vc (:url "https://github.com/smihica/emmet-mode/")
  :hook (html-mode css-mode))
;; (use-package esxml ;; nov-mode dependency
;;   :vc (:url "https://github.com/tali713/esxml"))
(use-package f ;; lsp-mode dependency
  :vc (:url "https://github.com/rejeep/f.el"))
(use-package flexoki-themes
  :vc (:url "https://codeberg.org/crmsnbleyd/flexoki-emacs-theme")
  :custom ((flexoki-themes-use-bold-builtins t)
		   (flexoki-themes-use-bold-keywords t)
		   (flexoki-themes-use-italic-comments t)))
(use-package flymake
  :hook (html-mode prog-mode))
(use-package ht ;; lsp-mode dependency
  :vc (:url "https://github.com/Wilfred/ht.el"))
(use-package jinx
  :vc (:url "https://github.com/minad/jinx")
  :hook (markdown-mode)
  :custom (jinx-languages "en_US ja-JP"))
(use-package lorem-ipsum
  :vc (:url "https://github.com/jschaf/emacs-lorem-ipsum"))
(use-package ls-lisp
  :custom ((ls-lisp-dirs-first t)
		   (ls-lisp-ignore-case t)
		   (ls-lisp-use-insert-directory-program nil)
		   (ls-lisp-use-localized-time-format t)))
(use-package lsp-mode
  :vc (:url "https://github.com/emacs-lsp/lsp-mode")
  :hook ((html-mode . lsp)
		 (css-mode . lsp)
		 (js-mode . lsp)
		 (typescript-mode . lsp)
		 (tsx-mode . lsp))
  :custom ((lsp-completion-provider :none)
		   (lsp-enable-indentation nil))
  :commands lsp)
(use-package lsp-tailwindcss
  :vc (:url "https://github.com/merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :custom ((lsp-tailwindcss-add-on-mode t)
		   (lsp-tailwindcss-skip-config-check t)
		   (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server")))
(use-package lua-mode
  :vc (:url "https://github.com/immerrr/lua-mode"))
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom ((markdown-enable-wiki-links t)
		   (markdown-hide-urls t)
		   (markdown-hide-markup t)
		   (markdown-asymmetric-header t))
  :custom-face (markdown-code-face ((t (:family "Maple Mono NF CN")))) (markdown-table-face ((t (:inherit 'variable-pitch))))
  :bind (:map markdown-mode-map (("C-c h" . insert-title)
								 ("C-c d" . insert-date)
								 ([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point))))
(use-package moody
  :vc (:url "https://github.com/tarsius/moody")
  :config (moody-replace-mode-line-front-space) (moody-replace-mode-line-buffer-identification) (moody-replace-vc-mode))
;; (use-package nerd-icons
;;   :vc (:url "https://github.com/rainstormstudio/nerd-icons.el"))
;; (use-package nerd-icons-dired
;;   :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired")
;;   :hook (dired-mode))
(use-package nov
  ;; :vc (:url "https://depp.brause.cc/nov.el.git")
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width t))
(use-package nyan-mode
  :vc (:url "https://github.com/TeMPOraL/nyan-mode")
  :config (nyan-mode +1))
(use-package obsidian
  :vc (:url "https://github.com/licht1stein/obsidian.el")
  :custom (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind ("s-d" . obsidian-daily-note))
(use-package s ;; f dependency
  :vc (:url "https://github.com/magnars/s.el"))
(use-package snow
  :vc (:url "https://github.com/alphapapa/snow.el")
  :custom (snow-pile-factor 1))
;; (use-package treesit-langs
;;   :vc (:url "https://github.com/emacs-tree-sitter/treesit-langs"))
;; (use-package tree-sitter-langs
;;   :vc (:url "https://github.com/emacs-tree-sitter/tree-sitter-langs/"))
(use-package typo
  :vc (:url "https://github.com/jorgenschaefer/typoel")
  :hook (text-mode))
(use-package undo-fu
  :vc (:url "https://github.com/emacsmirror/undo-fu")
  :bind (("s-z" . undo-fu-only-undo)
		 ("s-Z" . undo-fu-only-redo)))
(use-package valign
  :vc (:url "https://github.com/casouri/valign")
  :custom (valign-fancy-bar t)
  :hook (markdown-mode))
(use-package variable-pitch
  :hook ((markdown-mode nov-mode)
		 (variable-pitch-mode . (lambda ()
								  (setq-local line-spacing 0.4)))))
(use-package visual-fill-column
  :custom ((visual-fill-column-center-text t)
		   (visual-fill-column-width 80))
  :hook (markdown-mode nov-mode))
(use-package yasnippet
  :vc (:url "https://github.com/joaotavora/yasnippet"))
;;; init.el ends here

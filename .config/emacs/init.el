;;; -*- lexical-binding: t -*-

(when (memq system-type '(darwin))
  (add-to-list 'default-frame-alist '(undecorated-round . t)) ;; rounded with no title
  (set-fontset-font t nil "SF Pro Display" nil 'append)  ;; Enable SF symbols
  (use-package emacs
    :bind
    ("C-c h" . ns-do-hide-emacs)
    ("C-c ˙" . ns-do-hide-others)
    :custom
    (browse-url-generic-program "open")
    (browse-url-mailto-function 'browse-url-generic)
    (ns-pop-up-frames nil)
    (mac-command-modifier 'meta)
    (mac-option-modifier 'none)
    (auth-sources '(macos-keychain-generic macos-keychain-internet))))
(use-package emacs
  :hook
  (window-setup-hook . toggle-frame-maximized)
  (ns-system-appearance-change-functions . auto-theme)
  (html-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :bind
  ([remap customize] . open-init)
  ("C-c ," . open-init)
  ("C-c y" . yt-dlp)
  ("C-<wheel-up>" . nil)
  ("C-<wheel-down>" . nil)
  ("C-c c" . ispell-word)
  ("M-[" . backward-paragraph)
  ("M-]" . forward-paragraph)
  :custom-face
  (default ((t (:family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t (:family "Maple Mono NF CN" :height 140))))
  (variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height 180))))
  :custom
  (auto-insert-directory "~/.config/emacs/templates/")
  (auto-insert-query nil)
  (backward-delete-char-untabify-method nil)
  (column-number-mode t)
  (completion-auto-help nil)
  (completion-ignore-case t t)
  (completion-styles '(basic flex partial-completion emacs22))
  (completions-sort 'historical)
  (cursor-type 'bar)
  (custom-file (make-temp-file "~/.cache/emacs/custom"))
  (delete-selection-mode t)
  (disabled-command-function nil)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 3)
  (electric-pair-mode t)
  (find-file-visit-truename t)
  (frame-resize-pixelwise t)
  (gc-cons-threshold 100000000)
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t)
  (global-visual-line-mode t)
  (ispell-personal-dictionary "~/.config/emacs/ispell-wordbook")
  (inhibit-startup-screen t)
  (insert-directory-program "gls")
  (isearch-lazy-count t)
  (large-file-warning-threshold 1000000000)
  (major-mode-remap-alist '((sh-mode . bash-ts-mode)
			    (mhtml-mode . html-ts-mode)
			    (css-mode . css-ts-mode)
			    (javascript-mode . js-ts-mode)
			    (dockerfile-mode . dockerfile-ts-mode)
			    (json-mode . json-ts-mode)
			    (yaml-mode . yaml-ts-mode)
			    (lua-mode . lua-ts-mode)))
  (make-backup-files nil)
  (mode-line-collapse-minor-modes '(not lsp-mode flymake-mode))
  (modus-themes-common-palette-overrides '((fringe unspecified)
					   (bg-line-number-inactive unspecified)
					   (bg-line-number-active unspecified)
					   (underline-link unspecified)
					   (underline-link-visited unspecified)
					   (underline-link-symbolic unspecified)
					   ;; (border-mode-line-active unspecified)
					   ;; (border-mode-line-inactive unspecified)
					   ))
  (modus-themes-headings '((1 . (2.0)) (2 . (1.6)) (3 . (1.2))))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (project-mode-line t)
  (project-vc-extra-root-markers '("project"))
  (read-buffer-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (ring-bell-function 'ignore)
  (scroll-bar-mode nil)
  (sentence-end-double-space nil)
  (shr-fill-text nil)
  (shr-inhibit-images t)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (use-dialog-box nil)
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  (which-key-mode t)
  (word-wrap-by-category t)
  :config
  (add-to-list 'imagemagick-enabled-types 'JXL)
  (auto-insert-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-auto-insert "\.html" "insert.html")
  (define-auto-insert "\.js" "insert.js")
  (define-key key-translation-map (kbd "M-o") (kbd "C-x o"))
  (define-key key-translation-map (kbd "M-r") (kbd "C-x r"))
  (editorconfig-mode)
  (fido-vertical-mode)
  (repeat-mode)
  (defun auto-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t))))
  (defun dired-finder-path ()
    "Open Dired in the frontmost Finder window path, if available."
    (interactive)
    (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
      (if path (dired (string-trim path)) (message "No Finder window found."))))
  (defun open-init ()
    "Open ~/.config/emacs/init.el"
    (interactive)
    (find-file "~/.config/emacs/init.el"))
  (defun unfill ()
    "Unfill the current region if active, or the current paragraph."
    (interactive)
    (let ((fill-column (point-max)))
      (if (use-region-p)
	  (fill-region (region-beginning) (region-end) nil)
	(fill-paragraph nil))))
  (defun vc-amend ()
    "Amend the previous commit title."
    (interactive)
    (vc-checkin nil 'git) (vc-git-log-edit-toggle-amend))
  (defun yt-dlp (url)
    "Download the audio, video, or video with subs from a given URL."
    (interactive "sEnter media source URL: ")
    (let ((flag (pcase (completing-read "Download: " '("audio" "video" "subtitled video") nil t)
		  ("audio" "-x")
		  ("subtitled video" "--write-subs")
		  (_ ""))))
      (async-shell-command (format "yt-dlp %s \"%s\"" flag url)))))
(use-package completion-preview
  :bind
  (:map completion-preview-active-mode
	("M-n" . completion-preview-next-candidate)
	("M-p" . completion-preview-prev-candidate)))
(use-package dired
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-omit-files	"^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
  :bind
  (:map dired-mode-map
	("e" . dwim-shell-commands-macos-open-with)
	("d" . dwim-macos-move-to-trash)
	("SPC" . nil)
	("SPC p" . dwim-convert-to-pdf)
        ("SPC h" . dwim-npx-http-server)
	("SPC t" . dwim-tailwindcss)))
(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . completion-preview-mode))
(use-package html-mode
  :hook
  (html-mode . display-line-numbers-mode)
  (html-mode . completion-preview-mode))
(use-package text-mode
  :hook
  (text-mode . display-line-numbers-mode)
  (text-mode . typo-mode)
  (text-mode . flyspell-mode))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setopt use-package-always-ensure t)

(use-package exec-path-from-shell
  :if
  (memq window-system '(ns x))
  :config
  (exec-path-from-shell-initialize))
(use-package dwim-shell-command
  :config
  (defun dwim-macos-move-to-trash ()
    "Move marked files to macOS trash."
    (interactive)
    (when (y-or-n-p "Move marked files to macOS trash? ")
      (dwim-shell-command-on-marked-files
       "Move marked files to macOS trash"
       "trash '<<f>>'"
       :silent-success t)))
  (defun dwim-npx-http-server ()
    "npx HTTP serve current directory."
    (interactive)
    (dwim-shell-command-on-marked-files
     "HTTP serve current dir"
     "npx http-server -o -p 9999"
     :focus-now t
     :no-progress t))
  (defun dwim-tailwindcss ()
    "Tailwindcss in current directory."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Tailwindcss in current dir"
     "npx @tailwindcss/cli -i app.css -o dist.css --watch"
     :focus-now t
     :no-progress t))
  (defun dwim-convert-to-pdf ()
    "Convert file to pdf via pandoc and typst.
fonttools varLib.mutator '/Users/leaf/Library/Fonts/AtkinsonHyperlegibleNext[wght].ttf' wght=400"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to pdf"
     "pandoc --pdf-engine=typst --template=/Users/leaf/.config/typst/template.typ '<<f>>' -o '<<fne>>.pdf'")))
(use-package spacious-padding
  :config
  (spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 4
      :custom-button-width 3
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 0)))

(setopt use-package-always-defer t)

(use-package agent-shell
  :bind
  ("C-c a" . agent-shell-add-region)
  ("C-c s" . agent-shell))
(use-package apheleia
  :custom
  (apheleia-global-mode t))
(use-package appine
  :vc (:url "https://github.com/chaoswork/appine")
  :bind (("C-x a a" . appine)
         ("C-x a k" . appine-kill)
         ("C-x a u" . appine-open-url)
         ("C-x a o" . appine-open-file)))
(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode)
  :custom
  (csv-align-padding 2)
  (csv-align-max-width 72))
(use-package nerd-icons-dired
  :hook dired-mode)
(use-package elfeed
  :preface
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind
  ("C-c f" . elfeed)
  (:map elfeed-search-mode-map
	("f" . elfeed-search-show-entry)
	("m" . elfeed-search-show-entry))
  :init
  (load (expand-file-name "elfeed-feeds.el" user-emacs-directory))
  :custom
  (elfeed-search-filter "@1-month-ago +unread"))
(use-package elfeed-webkit
  :demand
  :config
  (elfeed-webkit-enable)
  :bind
  (:map elfeed-show-mode-map
	("%" . elfeed-webkit-toggle)))
(use-package gterm
  :vc (:url "https://github.com/rwc9u/emacs-libgterm" :branch "main")
  :custom
  (gterm-always-compile-module t)
  :bind
  ("C-c v" . gterm))
(use-package hackernews)
(use-package lorem-ipsum)
(use-package lua-mode
  :hook
  (lua-mode . eglot-ensure))
(use-package markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  :hook
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . visual-fill-column-mode)
  :custom-face
  (markdown-list-face ((t (:family "Atkinson Hyperlegible Mono"))))
  :custom
  (markdown-asymmetric-header t)
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  (markdown-link-space-sub-char " ") ;; Follow link style of Obsidian
  (markdown-special-ctrl-a/e t)
  (markdown-unordered-list-item-prefix "- ")
  (markdown-wiki-link-retain-case t)
  :config
  ;; Handle wiki links with explicit file extensions
  ;; e.g., [[example.mp4]] opens example.mp4 directly, not example.mp4.md
  (advice-add 'markdown-convert-wiki-link-to-filename :around
	      (lambda (orig-fn name)
		"Convert NAME to filename. If NAME has explicit extension, use it directly."
		(if (file-name-extension name)
		    (replace-regexp-in-string "[[:space:]\n]" markdown-link-space-sub-char name)
		  (funcall orig-fn name))))
  ;; Override markdown-follow-wiki-link to not force markdown-mode on opened files
  ;; This allows files opened via wikilinks to use their correct major mode
  ;; (e.g., csv-mode for .csv files, python-mode for .py files, etc.)
  (advice-add 'markdown-follow-wiki-link :override
	      (lambda (name &optional other)
		"Follow the wiki link NAME, respecting buffer's major mode."
		(unless buffer-file-name (user-error "Must be visiting a file"))
		(when other (other-window 1))
		(let ((default-directory (file-name-directory buffer-file-name)))
		  (find-file (markdown-convert-wiki-link-to-filename name)))))
  (defun daily-note ()
    "Make a new daily note in my obsidian vault"
    (interactive)
    (find-file (concat "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes/" (format-time-string "%Y-%m-%d") ".md")))
  (defun h1-title ()
    "Insert an atx level 1 heading with the name of the file."
    (interactive)
    (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
  (defun h2-today ()
    "Insert an atx level 2 heading with today's date in iso format."
    (interactive)
    (insert "## " (format-time-string "%Y-%m-%d") "\n"))
  :bind
  ("C-c d" . daily-note)
  (:map markdown-mode-map
        ("C-c SPC 1" . h1-title)
	("C-c SPC 2" . h2-today)))
(use-package mines)
(use-package reader
  :vc
  (:url "https://codeberg.org/MonadicSheep/emacs-reader" :make "all")
  :config
  (defun fix-reader ()
    "Recompile Reader Libraries"
    (interactive)
    (let ((default-directory "~/.config/emacs/elpa/reader/")) (shell-command "make clean all"))))
(use-package typo)
(use-package typst-ts-mode
  ;; (typst-ts-mc-install-grammar)
  :vc
  (:url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :mode
  ("\\.typ\\'" . typst-ts-mode))
(use-package undo-fu
  :bind
  ("M-z" . undo-fu-only-undo)
  ("M-Z" . undo-fu-only-redo))

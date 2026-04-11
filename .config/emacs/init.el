;;; -*- lexical-binding: t -*-

;;; Internal packages and internal hooks
(add-to-list 'default-frame-alist '(undecorated-round . t))

(set-fontset-font t nil "SF Pro Display" nil 'append)

(use-package emacs
  :hook
  (window-setup-hook . toggle-frame-maximized)
  (ns-system-appearance-change-functions . auto-theme)
  (text-mode . flyspell-mode)
  (html-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :bind
  ("C-<wheel-up>" . nil)
  ("C-<wheel-down>" . nil)
  ("C-c c" . ispell-word)
  ("M-[" . backward-paragraph)
  ("M-]" . forward-paragraph)
  :custom-face
  (default ((t ( :family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t ( :family "Maple Mono NF CN" :height 140))))
  (variable-pitch ((t ( :family "Atkinson Hyperlegible Next" :height 180))))
  (mode-line ((t ( :family "Atkinson Hyperlegible Next" :height 160))))
  (mode-line-inactive ((t ( :family "Atkinson Hyperlegible Next" :height 160))))
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
  (eglot-autoshutdown t)
  (electric-pair-mode t)
  (find-file-visit-truename t)
  (frame-resize-pixelwise t)
  (gc-cons-threshold 100000000)
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t)
  (global-visual-line-mode t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/.config/emacs/ispell-wordbook")
  (inhibit-startup-screen t)
  (insert-directory-program "gls")
  (isearch-lazy-count t)
  (large-file-warning-threshold 1000000000)
  (mac-command-modifier 'meta)
  (mac-option-modifier 'none)
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
  (modus-themes-common-palette-overrides '((underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified)))
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
  (use-package-vc-prefer-newest t)
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  (which-key-mode t)
  (word-wrap-by-category t)
  :config
  (defun auto-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t))))
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
  (defun async-shell-command-no-window (command)
    (interactive)
    (let ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))))
      (async-shell-command command))))

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

(require 'eglot)

;; Internal Packages

(use-package completion-preview
  :hook
  (prog-mode html-mode agent-shell-mode)
  :bind
  ( :map completion-preview-active-mode
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)))

(use-package open-init
  :bind
  ([remap customize] . open-init)
  ("C-c ," . open-init)
  :init
  (defun open-init ()
    (interactive)
    (find-file "~/.config/emacs/init.el")))

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
  ( :map dired-mode-map
    ("e" . dwim-shell-commands-macos-open-with)
    ("d" . dwim-macos-move-to-trash)
    ("x" . dired-finder-path)
    ("SPC" . nil)
    ("SPC p" . dwim-convert-to-pdf)
    ("SPC h" . dwim-npx-http-server)
    ("SPC t" . dwim-tailwindcss))
  :config
  (defun dired-finder-path ()
    "Open Dired in the frontmost Finder window path, if available."
    (interactive)
    (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
      (if path (dired (string-trim path)) (message "No Finder window found.")))))

(use-package yt-dlp
  :bind
  ( :prefix "C-c y"
    :prefix-map yt-dlp-map
    ("a" . yt-dlp-audio)
    ("v" . yt-dlp-video)
    ("s" . yt-dlp-video-subtitled))
  :init
  (defun yt-dlp--download (flag)
    (let ((url (read-string "URL: ")))
      (async-shell-command (format "yt-dlp %s \"%s\"" flag url))))
  (defun yt-dlp-audio ()
    (interactive)
    (yt-dlp--download "-x"))
  (defun yt-dlp-video ()
    (interactive)
    (yt-dlp--download ""))
  (defun yt-dlp-video-subtitled ()
    (interactive)
    (yt-dlp--download "--write-subs")))

;;; External packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package apheleia
  :ensure t
  :custom
  (apheleia-global-mode t))

(use-package elfeed-webkit
  :ensure t
  :demand
  :config
  (elfeed-webkit-enable)
  :bind
  ( :map elfeed-show-mode-map
    ("%" . elfeed-webkit-toggle)))

(use-package exec-path-from-shell
  :ensure t
  :if
  (memq window-system '(ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package dwim-shell-command
  :ensure t
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
  :ensure t
  :config
  (spacious-padding-mode))

(use-package treesit-langs
  :ensure t
  :vc
  ( :url "https://github.com/kiennq/treesit-langs"))

;; Deferred External Packages

(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-opencode-default-model-id "ollama/gemma4:26b-32k")
  :bind
  ( :prefix "C-c a"
    :prefix-map favorite-agents
    ("a" . agent-shell)
    ("o" . agent-shell-opencode-start-agent)
    ("g" . agent-shell-google-start-gemini)
    ("c" . agent-shell-github-start-copilot)))

(use-package appine
  :ensure t :defer t
  :vc ( :url "https://github.com/chaoswork/appine")
  :if
  (memq window-system '(ns))
  :bind
  ( :prefix "C-c m"
    :prefix-map macos-views
    ("a" . appine)
    ("k" . appine-kill)
    ("u" . appine-open-url)
    ("o" . appine-open-file)))

(use-package csv-mode
  :ensure t :defer t
  :hook
  (csv-mode . csv-align-mode)
  :custom
  (csv-align-padding 2)
  (csv-align-max-width 72))

(use-package elfeed
  :ensure t :defer t
  :preface
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind
  ("C-c f" . elfeed)
  ( :map elfeed-search-mode-map
    ("f" . elfeed-search-show-entry)
    ("m" . elfeed-search-show-entry))
  :init
  (load (expand-file-name "elfeed-feeds.el" user-emacs-directory))
  :custom
  (elfeed-search-filter "@1-month-ago +unread"))

(use-package gterm
  :ensure t :defer t
  ;; Specify custom Emacs include path (for emacs-module.h)
  ;; zig build -Demacs-include=/path/to/emacs/include
  :vc ( :url "https://github.com/rwc9u/emacs-libgterm" :branch "main")
  :custom
  (gterm-always-compile-module t)
  :bind
  ("C-c v" . gterm))

(use-package hackernews
  :ensure t :defer t
  :bind
  ("C-c h" . hackernews))

(use-package lorem-ipsum
  :ensure t :defer t)

(use-package lua-mode
  :ensure t :defer t
  :hook
  (lua-mode . eglot-ensure))

(use-package magit
  :ensure t :defer t)

(use-package markdown-mode
  :ensure t :defer t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :hook
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . visual-fill-column-mode)
  :custom-face
  (markdown-list-face ((t ( :family "Atkinson Hyperlegible Mono"))))
  :custom
  (markdown-asymmetric-header t)
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  (markdown-link-space-sub-char " ")
  (markdown-special-ctrl-a/e t)
  (markdown-unordered-list-item-prefix "- ")
  (markdown-wiki-link-retain-case t)
  :config
  ;; Open wikilinks with explicit file extensions without also adding .md extension
  (advice-add 'markdown-convert-wiki-link-to-filename :around
	      (lambda (orig-fn name)
		"Convert NAME to filename. If NAME has explicit extension, use it directly."
		(if (file-name-extension name) (replace-regexp-in-string "[[:space:]\n]" markdown-link-space-sub-char name) (funcall orig-fn name))))
  ;; Files opened via wikilinks use their correct major mode instead of markdown-mode
  (advice-add 'markdown-follow-wiki-link :override
	      (lambda (name &optional other)
		"Follow the wiki link NAME, respecting buffer's major mode."
		(unless buffer-file-name (user-error "Must be visiting a file"))
		(when other (other-window 1))
		(let ((default-directory (file-name-directory buffer-file-name))) (find-file (markdown-convert-wiki-link-to-filename name)))))
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
  ( :map markdown-mode-map
    ("C-c SPC 1" . h1-title)
    ("C-c SPC 2" . h2-today)))

(use-package markdown-indent-mode
  :ensure t :defer t
  :hook (markdown-mode))

(use-package mines
  :ensure t :defer t)

(use-package music-control
  :defer t
  :load-path "elpa/music-control/"
  :config
  (music-control-mode 1))

(use-package nerd-icons-dired
  :defer t
  :hook dired-mode)

(use-package periphery
  :vc ( :url "https://github.com/konrad1977/periphery" :rev :newest)
  :custom
  ;; Adjust severity badge background darkness (0-100, higher = darker)
  (periphery-background-darkness 85)
  ;; Use theme colors instead of default Catppuccin colors
  (periphery-use-theme-colors t)
  ;; Trim message prefixes for cleaner display
  (periphery-trim-message-prefix t)
  ;; Enable debug mode if needed
  (periphery-debug nil)
  :config
  ;; Optional: Clear color cache when switching themes
  (add-hook 'after-load-theme-hook #'periphery--clear-color-cache))

(use-package reader
  :ensure t :defer t
  :vc
  ( :url "https://codeberg.org/MonadicSheep/emacs-reader" :make "all")
  :config
  (defun fix-reader ()
    "Recompile Reader Libraries"
    (interactive)
    (let ((default-directory "~/.config/emacs/elpa/reader/")) (shell-command "make clean all"))))

(use-package swift-ts-mode
  :ensure t :defer t
  :mode "\\.swift\\'"
  :hook (swift-ts-mode . (lambda () (add-hook 'after-save-hook #'xcode-build nil t)))
  :config
  (defun xcode-build ()
    (interactive)
    (async-shell-command-no-window "/Users/leaf/.config/emacs/xcode-build.sh")))

(use-package typo
  :ensure t :defer t
  :hook text-mode)

(use-package typst-ts-mode
  :ensure t :defer t
  ;; (typst-ts-mc-install-grammar)
  :vc
  ( :url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :mode "\\.typ\\'")

(use-package undo-fu
  :ensure t :defer t
  :bind
  ("M-z" . undo-fu-only-undo)
  ("M-Z" . undo-fu-only-redo))

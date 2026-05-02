;;; -*- lexical-binding: t -*-

;;; Internal packages and internal hooks

(require 'package)

(use-package emacs
  :init
  (setenv "GIT_EDITOR" "emacsclient")
  :hook
  (emacs-startup . server-start)
  (ns-system-appearance-change-functions . auto-theme)
  (text-mode . flyspell-mode)
  (html-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :bind
  ("M-z" . undo-only)
  ("M-Z" . undo-redo)
  ("C-<wheel-up>" . nil)
  ("C-<wheel-down>" . nil)
  ("C-c c" . ispell-word)
  ("M-[" . backward-paragraph)
  ("M-]" . forward-paragraph)
  ("C-x 2" . split-and-follow-horizontally)
  ("C-x 3" . split-and-follow-vertically)
  :custom-face
  (default ((t ( :family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t ( :inherit default))))
  :custom
  (auto-insert-directory "~/.config/emacs/templates/")
  (auto-insert-query nil)
  (auto-save-default nil)
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
  (eglot-code-action-indicator "*") 
  (eglot-code-action-indications '(mode-line))
  (eglot-autoshutdown t)
  (eldoc-echo-area-use-multiline-p t)
  (electric-pair-mode t)
  (find-file-visit-truename t)
  (gc-cons-threshold 100000000)
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t)
  (global-visual-line-mode t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/.config/emacs/ispell-wordbook")
  (inhibit-startup-screen t)
  (isearch-lazy-count t)
  (large-file-warning-threshold 1000000000)
  (mac-command-modifier 'meta)
  (mac-option-modifier 'none)
  (major-mode-remap-alist '((sh-mode . bash-ts-mode) (mhtml-mode . html-ts-mode) (css-mode . css-ts-mode) (javascript-mode . js-ts-mode) (dockerfile-mode . dockerfile-ts-mode) (json-mode . json-ts-mode) (yaml-mode . yaml-ts-mode) (lua-mode . lua-ts-mode)))
  (make-backup-files nil)
  (mode-line-collapse-minor-modes '(not flymake-mode))
  (modus-themes-common-palette-overrides '((underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified)))
  (modus-themes-headings '((1 . (regular 1.8)) (2 . (regular 1.6)) (3 . (regular 1.4)) (4 . (regular 1.2)) (t . (regular))))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (package-vc-allow-build-commands t)
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
  (which-key-mode t)
  (word-wrap-by-category t)
  :config
  (defun async-shell-command-no-window (command)
    (interactive)
    (let ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))))
      (async-shell-command command)))
  (defun auto-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t))))
  (defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
  (defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
  (defun unfill ()
    "Unfill the current region if active, or the current paragraph."
    (interactive)
    (let ((fill-column (point-max)))
      (if (use-region-p)
	  (fill-region (region-beginning) (region-end) nil)
	(fill-paragraph nil))))
  (defun get-apw-password (domain)
    "Fetch the password for DOMAIN using the apw tool."
    ;; Usage: (get-apw-password "example.com")
    (let* ((json-str (shell-command-to-string (format "apw pw get %s" domain)))
           (data (json-parse-string json-str :object-type 'alist))
           (results (alist-get 'results data)))
      (when (> (length results) 0)
	(alist-get 'password (elt results 0)))))
  (add-to-list 'imagemagick-enabled-types 'JXL)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-auto-insert "\.html" "insert.html")
  (define-auto-insert "\.js" "insert.js")
  (define-key key-translation-map (kbd "M-o") (kbd "C-x o"))
  (define-key key-translation-map (kbd "M-r") (kbd "C-x r"))
  (set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display"))

;; Internal Packages

(auto-insert-mode 1)

(auto-save-visited-mode 1)

(editorconfig-mode 1)

(fido-vertical-mode 1)

(global-hl-line-mode 1)

(repeat-mode 1)

(require 'eglot)

(use-package almost-maximize-frame
  :hook
  (emacs-startup . almost-maximize-frame)
  :init
  ;; (set-frame-size (selected-frame) (- (display-pixel-width) 80) (- (display-pixel-height) 500) t)
  (defun almost-maximize-frame()
    "Borderless maximise with margins for tiling"
    (add-to-list 'default-frame-alist '(undecorated-round . t)))
  (setopt frame-resize-pixelwise t))

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
  :after ls-lisp
  :preface (require 'ls-lisp)
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
  :bind
  ( :map dired-mode-map
    ("f" . dired-finder-path))
  :config
  (defun dired-finder-path ()
    "Open Dired in the frontmost Finder window path, if available."
    (interactive)
    (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
      (if path (dired (string-trim path)) (message "No Finder window found.")))))

(use-package ls-lisp
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-localized-time-format t))

(use-package markdown-ts-mode
  :mode
  ("\\.md\\'" . markdown-ts-mode)
  :hook
  (markdown-ts-mode . (lambda () (face-remap-add-relative 'default :height 180)))
  (markdown-ts-mode . (lambda () (font-lock-add-keywords nil '(("\\[\\[\\([^]]+\\)\\]\\]" 0 'link t)))))
  (markdown-ts-mode-hook . orgtbl-mode)
  :bind
  ("C-c d" . daily-note)
  (:map markdown-ts-mode-map
	("C-c C-i" . (lambda () (interactive) (md-toggle-markup "*")))
	("C-c C-b" . (lambda () (interactive) (md-toggle-markup "**")))
	("M-RET" . md-follow-wiki-link)
	("C-c C-o" . md-follow-wiki-link)
	("C-c C-1" . h1-title)
	("C-c C-2" . h2-today))
  :config
  (defun md-toggle-markup (delim)
    "Add DELIM around region/word, or remove it if already present."
    (let* ((len (length delim))
           (beg (if (use-region-p) (region-beginning) (car (bounds-of-thing-at-point 'word))))
           (end (if (use-region-p) (region-end) (cdr (bounds-of-thing-at-point 'word)))))
      (when (and beg end)
	(save-excursion
          (let ((before (buffer-substring-no-properties (- beg len) beg))
		(after  (buffer-substring-no-properties end (+ end len))))
            (if (and (string= before delim) (string= after delim))
		(progn (delete-region end (+ end len)) (delete-region (- beg len) beg))
              (goto-char end) (insert delim)
              (goto-char beg) (insert delim)))))))
  (defun h1-title ()
    "Insert an atx level 1 heading with the name of the file."
    (interactive)
    (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
  (defun h2-today ()
    "Insert an atx level 2 heading with today's date in iso format."
    (interactive)
    (insert "## " (format-time-string "%Y-%m-%d") "\n"))
  (defun daily-note ()
    "Make a new daily note in my obsidian vault"
    (interactive)
    (find-file (concat "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes/" (format-time-string "%Y-%m-%d") ".md")))
  (defun md-follow-wiki-link ()
    (interactive)
    (save-excursion
      ;; 1. Move to end of current link (if cursor is inside it)
      (search-forward "]]" (line-end-position) t)
      ;; 2. Search backward for the link start
      (if (re-search-backward "\\[\\[\\([^]]+\\)\\]\\]" (line-beginning-position) t)
          (let ((path (match-string 1)))
            (find-file (if (file-name-extension path) path (concat path ".md"))))
        (message "No wikilink found on this line."))))
  (require 'org-table)
  (defun my/markdown-table-fix (&rest _args)
    (when (and (buffer-file-name) (string-match-p "\\.md$" (buffer-file-name)))
      (save-excursion
	(let ((end (org-table-end)))
          (goto-char (org-table-begin))
          (while (search-forward "+" end t)
            (replace-match "|"))))))
  (advice-add 'org-table-align :after #'my/markdown-table-fix))

(use-package project
  :bind
  ( :map project-prefix-map
    ("s" . project-gterm)
    ("S" . project-npx-serve)
    ("t" . project-tailwindcss))
  :config
  (defun project-gterm ()
    "Open gterm in project's root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t)))) (gterm)))
  (defun project-run (label msg &rest args)
    "Run ARGS as a process LABEL in project root, showing MSG."
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (buf (format "*%s:%s*" label (project-name project))))
      (when (get-buffer buf) (kill-buffer buf))
      (apply #'start-process label buf args)
      (when msg (message msg (project-name project)))))
  (defun project-tailwindcss ()
    "npx @tailwindcss/cli -i app.css -o dist.css --watch the project's root directory"
    (interactive)
    (project-run "tailwindcss" "Tailwind is running in %s" "npx" "@tailwindcss/cli" "-i" "app.css" "-o" "dist.css" "--watch"))
  (defun project-npx-serve ()
    "Clear clipboard, npx serve the project's root directory, call clipboard watcher."
    (interactive)
    (gui-set-selection 'CLIPBOARD "")
    (project-run "serve" "Serving %s..." "npx" "serve")
    (watch-clipboard-xwidget-webkit-browse-url))
  (defun watch-clipboard-xwidget-webkit-browse-url ()
    "Watch for clipboard data and open in Xwidgets."
    (let ((current-clip (gui-get-selection 'CLIPBOARD 'STRING)))
      (if (and current-clip (not (string-empty-p current-clip)))
          (progn (split-and-follow-horizontally) (xwidget-webkit-browse-url current-clip) (message "Clipboard update detected! Opened %s in Xwidgets" current-clip))
        (run-at-time "0.5 sec" nil #'watch-clipboard-xwidget-webkit-browse-url)))))

(use-package xwidget
  :bind
  ( :map xwidget-webkit-mode-map
    ("u". xwidget-webkit-browse-url)))

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
  :bind-keymap
  :bind
  ( :prefix "C-c x"
    :prefix-map my-dwim-shell-commands-map
    ("m" . dwim-file-to-mla-pdf)
    ("g" . dwim-file-to-generic-pdf)
    ("p" . dwim-md-to-pptx))
  (([remap shell-command] . dwim-shell-command)
   ( :map dired-mode-map
     ([remap dired-do-async-shell-command] . dwim-shell-command)
     ([remap dired-do-shell-command] . dwim-shell-command)
     ([remap dired-smart-shell-command] . dwim-shell-command)
     ("e" . dwim-shell-commands-macos-open-with)
     ("d" . dwim-macos-move-to-trash)
     ("x" . my-dwim-shell-commands-map)))
  :config
  (defun dwim-macos-move-to-trash ()
    "Move marked files to macOS trash."
    (interactive)
    (when (y-or-n-p "Move marked files to macOS trash? ")
      (dwim-shell-command-on-marked-files
       "Move marked files to macOS trash"
       "trash '<<f>>'"
       :silent-success t)))
  (defun dwim-file-to-mla-pdf ()
    "Convert file to MLA pdf via pandoc and typst."
    ;; fonttools varLib.mutator '/Users/leaf/Library/Fonts/AtkinsonHyperlegibleNext[wght].ttf' wght=400
    ;; pandoc --print-default-template=typst
    (interactive)
    (dwim-shell-command-on-marked-files
     "Converting to MLA pdf"
     "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=/Users/leaf/.config/typst/template.typ"))
  (defun dwim-md-to-pptx ()
    "Convert md files to pptx."
    (interactive)
    (let ((files (dwim-shell-command--files)))
      (if (cl-every (lambda (f) (string-suffix-p ".md" f t)) files)
          (dwim-shell-command-on-marked-files 
           "Converting md to pptx" 
           "npx @marp-team/marp-cli@latest '<<f>>' --pptx")
	(user-error "Selection contains non-markdown files!")))))

(use-package reader
  :ensure t
  :vc ( :url "https://codeberg.org/MonadicSheep/emacs-reader" :make "all")
  :config
  (defun fix-reader ()
    "Recompile Reader Libraries"
    (interactive)
    (let ((default-directory "~/.config/emacs/elpa/reader/")) (shell-command "make clean all"))))

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

(use-package treesit-langs
  :ensure t
  :vc ( :url "https://github.com/kiennq/treesit-langs"))

(use-package google-translate
  :ensure t
  :bind
  ("C-c t" . google-translate-smooth-translate)
  ("C-c T" . google-translate-at-point)
  :custom
  (google-translate-output-destination 'echo-area)
  (google-translate-show-phonetic t)
  (google-translate-translation-directions-alist
   '(("ja" . "en") ("en" . "ja"))))

;; Deferred External Packages

(use-package agent-shell
  :ensure t :defer t
  :custom
  (agent-shell-opencode-default-model-id "ollama/gemma4:26b-64k")
  (agent-shell-github-default-model-id "claude-haiku-4.5")
  :bind
  ( :prefix "C-c a"
    :prefix-map favorite-agents
    ("a" . agent-shell)
    ("p" . agent-shell-pi-start-agent)
    ("o" . agent-shell-opencode-start-agent)
    ("g" . agent-shell-google-start-gemini)
    ("c" . agent-shell-github-start-copilot)))

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
  :vc ( :url "https://github.com/rwc9u/emacs-libgterm" :branch "main")
  :custom
  (gterm-always-compile-module t)
  :bind
  ("C-c s" . gterm))

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

(use-package markdown-indent-mode
  :ensure t :defer t
  :hook (markdown-ts-mode))

(use-package mines
  :ensure t :defer t)

(use-package music-control
  :defer t
  :load-path "elpa/music-control/"
  :config
  (music-control-mode 1))

(use-package nerd-icons-dired
  :ensure t :defer t
  :hook dired-mode)

(use-package swift-ts-mode
  :ensure t :defer t
  :mode "\\.swift\\'"
  :hook
  (swift-ts-mode . eglot-ensure)
  :bind
  ( :map swift-ts-mode-map
    ("C-c SPC" . xcode-build))
  :config
  (add-to-list 'apheleia-mode-alist
	       '(swift-ts-mode . swift-format))
  (add-to-list 'apheleia-formatters
	       '(swift-format "xcrun" "swift-format" (buffer-file-name)))
  (add-to-list 'eglot-server-programs '(swift-ts-mode . ("xcrun" "sourcekit-lsp")))
  (defun xcode-build ()
    (interactive)
    (async-shell-command-no-window "/Users/leaf/.config/emacs/xcode-build.sh")))

(use-package typo
  :ensure t :defer t
  :hook text-mode)

(use-package typst-ts-mode
  :ensure t :defer t
  ;; (typst-ts-mc-install-grammar)
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :mode "\\.typ\\'")

(use-package visual-fill-column
  :ensure t :defer t
  :hook (markdown-ts-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80))

(server-start)
(setenv "GIT_EDITOR" "emacsclient")

;;; init.el --- Emacs 31 Initialization -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Leaf Eriksen <leaferiksen@gmail.com>

;;; Commentary:

;; Top level functions are sorted primarily by priority,
;; secondarily by alphabet.

;; `use-package' :key sort order
;; what to install (:ensure :vc)
;; when to load what (:demand :mode :commands :hook)
;; if to load or not (:if :after)
;; what to keys to bind (:bind :prefix :map)
;; what variables to set (:custom-face :custom)
;; what functions to run when (:init :config)

;;; Code:

;; Internal features and hooks

(use-package emacs
  :hook (emacs-startup . server-start)
  (emacs-startup . almost-maximize-frame)
  :bind ([remap customize] . open-init)
  :custom (auto-insert-directory "~/.config/emacs/templates/")
  (auto-insert-mode t)
  (auto-insert-query nil)
  (auto-save-default nil)
  (auto-save-visited-mode t)
  (backward-delete-char-untabify-method nil)
  (column-number-mode t)
  (cursor-type 'bar)
  (custom-file null-device)
  (delete-selection-mode t)
  (disabled-command-function nil)
  (display-line-numbers-width-start 3)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-help-at-pt t)
  (electric-pair-mode t)
  (fido-vertical-mode t)
  (find-file-visit-truename t)
  (frame-resize-pixelwise t)
  (gc-cons-threshold 100000000)
  (global-hl-line-mode t)
  (global-visual-line-mode t)
  (ibuffer-human-readable-size t)
  (inhibit-startup-screen t)
  (isearch-lazy-count t)
  (large-file-warning-threshold 1000000000)
  (make-backup-files nil)
  (mode-line-collapse-minor-modes '(not flymake-mode))
  (package-vc-allow-build-commands t)
  (package-vc-register-as-project nil)
  (read-buffer-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (repeat-mode t)
  (ring-bell-function 'ignore)
  (scroll-bar-mode nil)
  (sentence-end-double-space nil)
  (shr-max-image-proportion 0.6)
  (shr-width 80)
  (speedbar-window-default-width 30)
  (speedbar-window-max-width 20)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes t)
  (use-dialog-box nil)
  (use-package-vc-prefer-newest t)
  (use-short-answers t)
  (user-full-name "Leaf Eriksen")
  (user-mail-address "leaferiksen@gmail.com")
  (vc-allow-rewriting-published-history t)
  (vc-auto-revert-mode t)
  (vc-dir-auto-hide-up-to-date 'revert)
  (which-key-mode t)
  (word-wrap-by-category t)
  (display-line-numbers-type 'relative)
  :config (setenv "GIT_EDITOR" "emacsclient")
  (defun open-init ()
    "Visit `user-init-file'."
    (interactive)
    (find-file user-init-file))
  (defun almost-maximize-frame ()
    "Borderless maximise with margins for tiling."
    (interactive)
    ;; (add-to-list 'default-frame-alist '(undecorated-round . t))
    (set-frame-width
     (selected-frame)
     (- (display-pixel-width) 85)
     nil t))
  (defun unfill ()
    "Unfill the current region if active, or the current paragraph."
    (interactive)
    (let ((fill-column (point-max)))
      (if (use-region-p)
          (fill-region (region-beginning) (region-end) nil)
        (fill-paragraph nil))))
  (add-to-list 'imagemagick-enabled-types 'JXL)
  (define-auto-insert "\\.html\\'" "insert.html")
  (define-auto-insert "\\.js\\'" "insert.js"))

(use-package term/ns-win
  :if (eq window-system 'ns)
  :bind ;; modernize undo and remove s-Z to s-z translation map
  ("s-z" . undo-only)
  ("s-Z" . nil)
  ("s-Z" . undo-redo)
  ("s-w" . kill-current-buffer)
  ("C-M-y" . yank-pop)
  ;; enable standard macOS emoji binding
  ("H-e" . ns-do-show-character-palette)
  ("H-f" . toggle-frame-fullscreen)
  :custom (delete-by-moving-to-trash t)
  (mac-function-modifier 'hyper)
  (mac-option-modifier 'none)
  ;; (mac-control-modifier 'meta)
  ;; (mac-right-control-modifier 'control)
  (mouse-wheel-scroll-amount
   '(1
     ((shift)
      . hscroll)
     ((meta))
     ((control)
      . 1)
     ((control meta)
      . 1)))
  :config ;; Nerd Font Core Icons: Unicode Plane 0 (BMP)
  (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font")
  ;; Nerd Fonts Material Design Icons: Unicode Plane 15 (PUA-A)
  (set-fontset-font t '(#xF0001 . #xF1AF0) "Symbols Nerd Font")
  ;; SF Symbols: Unicode Plane 16 (PUA-B)
  (set-fontset-font t '(#x100000 . #x10FFFD) "SF Pro Display")
  ;; Transpose unwanted s- bindings to project, bookmark, and treesit navigation
  (keymap-set key-translation-map "s-g" "M-g")
  (keymap-set key-translation-map "s-o" "C-x p")
  (keymap-set key-translation-map "s-r" "C-x r")
  (dolist (key
	   '("a" "b" "d" "e" "f" "k" "l" "n" "p" "t" "u" "y" "<backspace>"))
    (keymap-set key-translation-map
		(concat "s-" key)
		(concat "C-M-" key)))
  (defun dired-install-dmg ()
    "Mount a .dmg file at point, copy its .app to ~/Applications/, then eject and optionally delete .dmg."
    (interactive)
    (if-let* ((dmg (dired-get-filename))
	      (mount-output
	       (shell-command-to-string
		(format "yes | hdiutil attach -nobrowse %s"
			(shell-quote-argument dmg))))
	      ((string-match "/Volumes/[^\t\n]+" mount-output))
	      (volume (string-trim-right (match-string 0 mount-output)))
	      (app (car (file-expand-wildcards (concat volume "/*.app")))))
	(progn
	  (make-directory "~/Applications/" t)
	  (shell-command
	   (format "cp -R %s ~/Applications/"
		   (shell-quote-argument app)))
	  (shell-command
	   (format "hdiutil detach %s" (shell-quote-argument volume)))
	  (when (y-or-n-p
		 (format "Installed %s to ~/Applications/ — trash the DMG?"
			 (file-name-nondirectory app)))
	    (shell-command
	     (format "trash %s" (shell-quote-argument dmg)))
	    (revert-buffer)))
      (message "Installation failed: could not mount DMG or find .app bundle"))))

(use-package modus-themes
  :hook (ns-system-appearance-change-functions . auto-theme)
  :custom (modus-themes-common-palette-overrides
	   '((underline-link unspecified)
	     (underline-link-visited unspecified)
	     (underline-link-symbolic unspecified)))
  ;; (modus-themes-headings '((t . (rainbow))))
  (modus-themes-italic-constructs t)
  ;; (modus-themes-mode-line '(accented borderless padded))
  (modus-themes-mixed-fonts t)
  :init (set-face-attribute 'default nil :family "Maple Mono CN" :height 140)
  (set-face-attribute 'fixed-pitch nil :inherit 'default)
  (set-face-attribute 'variable-pitch nil :family "Atkinson Hyperlegible Next" :height 180)
  (defun auto-theme (appearance)
    "Load theme matching system APPEARANCE."
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme
     (if (eq appearance 'dark)
	 'modus-vivendi-tinted 'modus-operandi-tinted)
     t)))

(use-package completion-preview
  :hook (prog-mode html-mode)
  :bind (:map completion-preview-active-mode
	      ("M-]" . completion-preview-next-candidate)
	      ("M-[" . completion-preview-prev-candidate))
  :custom (completion-auto-help nil)
  (completion-eager-update t)
  (completion-eager-display nil) ;Disable duplicate menu
  (completion-ignore-case t)
  (completions-sort 'historical))

(use-package dired
  :hook (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :custom (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-omit-files "\\`[.][.]?\\'\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
  (dired-omit-verbose nil)
  (dired-recursive-copies 'always)
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-localized-time-format t)
  :config (require 'ls-lisp))

(use-package editorconfig :init
  (editorconfig-mode 1)
  :config (add-to-list 'editorconfig-indentation-alist
		       '(js-json-mode js-indent-level)))

(use-package eglot
  :demand :hook
  (html-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :bind (:prefix "C-c a" :prefix-map eglot-actions
		 ("r" . eglot-rename)
		 ("a" . eglot-code-actions)
		 ("o" . eglot-code-action-organize-imports)
		 ("d" . eldoc)
		 ("f" . eglot-format))
  (:map eglot-mode-map ("H-<mouse-1>" . eglot-code-actions-at-mouse))
  :custom (eglot-code-action-indicator "*")
  (eglot-code-action-indications '(mode-line))
  (eglot-autoshutdown t))

(use-package flymake
  :hook (emacs-lisp-mode . flymake-avoid-scratch)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error))
  :config (defun flymake-avoid-scratch
	      ()
	    (when (buffer-file-name) (flymake-mode 1))))

(use-package html-mode
  ;; mhtml-mode causes issues with apheleia
  :mode ("\\.html\\'" . html-mode))

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :hook (markdown-ts-mode . eglot-ensure)
  (markdown-ts-mode . variable-pitch-mode)
  :bind (:map markdown-ts-mode
	      ("<tab>" . markdown-ts-demote)
	      ("<backtab>" . markdown-ts-promote))
  (:prefix "C-c m" :prefix-map markdown-actions
	   ("1" . markdown-h1-title)
	   ("2" . markdown-h2-today)
	   ("f" . markdown-mla-frontmatter))
  :custom (markdown-ts-inline-images t)
  :config (require 'markdown-ts-mode-x)
  (dolist (n (number-sequence 1 6))
    (set-face-attribute
     (intern (format "markdown-ts-heading-%d" n))
     nil :inherit
     (intern (format "modus-themes-heading-%d" n))))
  ;; Fix extensionless wikilinks
  (advice-add 'markdown-ts--make-link-button :around #'markdown-ts-make-link-button-advice)
  (defun markdown-ts-make-link-button-advice (orig-fn beg end url)
    (funcall orig-fn beg end
             (if (string-match-p "\\`#\\|\\`[a-z]+:\\|\\.[a-zA-Z]+" url)
		 url
	       (concat url ".md"))))
  ;; https://writewithharper.com/docs/integrations/emacs#Optional-Configuration
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(markdown-ts-mode . ("harper-ls" "--stdio"))))
  (defun markdown-h1-title ()
    "Insert an atx level 1 heading with the name of the file."
    (interactive)
    (insert "# "
	    (file-name-nondirectory
	     (file-name-sans-extension (buffer-file-name)))
	    "\n"))
  (defun markdown-h2-today ()
    "Insert an atx level 2 heading with today's date in iso format."
    (interactive)
    (insert "## " (format-time-string "%Y-%m-%d") "\n"))
  (defun markdown-mla-frontmatter ()
    "Insert frontmatter template for typst MLA export"
    (interactive)
    (insert "---\nprofessor: \nclass: \nword-count: true\n---\n")))

(use-package project
  :bind (:map project-prefix-map
	      ("s" . project-gterm)
	      ("n" . project-npm-run))
  :custom (project-mode-line t)
  (project-vc-extra-root-markers '("project"))
  :config (defun project-gterm
	      ()
	    "Open gterm in project's root directory."
	    (interactive)
	    (let ((default-directory (project-root (project-current t))))
	      (gterm)))
  (defun project-npm-run ()
    "Run an npm script from this project's package.json."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
	   (scripts
	    (with-temp-buffer
              (unless (file-exists-p "package.json")
		(user-error "No package.json in %s" default-directory))
              (insert-file-contents "package.json")
              (mapcar #'car
		      (alist-get 'scripts
				 (json-parse-buffer :object-type 'alist)))))
	   (script (completing-read "npm run: " scripts nil t)))
      (compile (format "npm run %s" script)))))

(use-package visual-wrap-prefix-mode :hook (prog-mode html-mode))

(use-package xwidget :bind
  (:map xwidget-webkit-mode-map ("u" . xwidget-webkit-browse-url)))

(use-package yt-dlp
  :bind ("C-c y" . yt-dlp-download)
  :init (defun yt-dlp-download
	    ()
	  "Download the URL in the clipboard with yt-dlp."
	  (interactive)
	  (let* ((url
		  (or (current-kill 0) (user-error "Nothing in clipboard")))
		 (video (y-or-n-p "Video? "))
		 (flags
		  (concat
		   (if video (and (y-or-n-p "Subs? ") "--write-subs") "-x")
		   (and video
			(y-or-n-p "Backwards-compatible (h264)? ")
			" -S vcodec:h264"))))
	    (async-shell-command
	     (format "yt-dlp %s %s" flags (shell-quote-argument url))))))

;;; External packages

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

(use-package agent-shell
  :ensure t
  :bind (:map project-prefix-map ("a" . agent-shell))
  :hook (agent-shell-mode . completion-preview-mode)
  (agent-shell-mode . variable-pitch-mode)
  :bind ("C-c c" . agent-shell-new-temp-shell)
  :custom (agent-shell-preferred-agent-config 'opencode))

(use-package agent-shell-macext
  :vc (:url "https://github.com/cxa/agent-shell-macext")
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom (agent-shell-macext-file-copy-policy 'auto)
  (agent-shell-macext-notifications t)
  (agent-shell-macext-notify-current-buffer nil))

(use-package anglish :ensure t :vc
  (:url "git@github.com:leaferiksen/anglish.el.git"))

(use-package apheleia
  :ensure t
  :config (cl-defun apheleia-elfmt
	      (&key buffer scratch callback &allow-other-keys)
	    "Format SCRATCH with `elfmt', then invoke CALLBACK.
Indentation settings are copied from BUFFER so the result matches
what you'd get by typing TAB there."
	    (let ((fc (buffer-local-value 'fill-column buffer))
		  (tabs (buffer-local-value 'indent-tabs-mode buffer))
		  (indent-fn
		   (buffer-local-value 'lisp-indent-function buffer))
		  (original (with-current-buffer scratch (buffer-string))))
	      (with-current-buffer scratch
		(delay-mode-hooks (emacs-lisp-mode))
		;; after the major mode, so these aren't clobbered
		(setq-local fill-column fc indent-tabs-mode tabs lisp-indent-function indent-fn)
		(condition-case err
		    (let ((gc-cons-threshold most-positive-fixnum)
			  (inhibit-message t)
			  (message-log-max nil))
		      (goto-char (point-max))
		      (while (not (bobp))
			(backward-sexp)
			(elfmt--sexp)))
		  ;; elfmt errors on unbalanced parens and old-style backquotes;
		  ;; roll back so apheleia applies an empty patch instead of garbage
		  (error
		   (erase-buffer)
		   (insert original)
		   (message "elfmt: %s" (error-message-string err))))))
	    (funcall callback))

  (setf (alist-get 'elfmt apheleia-formatters) #'apheleia-elfmt)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'elfmt)
  (apheleia-global-mode +1))

(use-package clojure-mode :ensure t)

(use-package csv-mode :ensure t :hook
  (csv-mode . csv-align-mode)
  :custom (csv-align-padding 2)
  (csv-align-max-width 72))

(use-package dwim-shell-command
  :ensure t
  :demand :bind
  ("s-i" . dwim-file-mediainfo)
  ([remap shell-command] . dwim-shell-command)
  ("C-c p" . dwim-file-to-pdf)
  (:map dired-mode-map
	([remap dired-do-async-shell-command] . dwim-shell-command)
	([remap dired-do-shell-command] . dwim-shell-command)
	([remap dired-smart-shell-command] . dwim-shell-command)
	("e" . dwim-shell-commands-macos-open-with)
	("i" . dwim-file-mediainfo)
	("x" . dwim-export-to))
  :config (with-eval-after-load 'dwim-shell-commands
	    (add-to-list 'dwim-shell-commands-git-clone-dirs "~/Git"))
  (defun dwim-file-mediainfo ()
    "Run mediainfo on the current buffer's file or marked dired files."
    (interactive)
    (dwim-shell-command-on-marked-files "MediaInfo" "mediainfo '<<f>>'" :utils "mediainfo"))
  (defun dwim-file-to-pdf (&optional mla)
    "Convert file to PDF via pandoc and typst; with prefix arg, use the MLA template."
    (interactive "P")
    (dwim-shell-command-on-marked-files
     "Converting to pdf"
     (format "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=%s"
             (expand-file-name
	      (if mla "mla-template.typ" "resume.typ")
	      "~/.config/typst/")))))

(use-package elfeed :ensure t :after elfeed-org :bind
  ("C-c f" . elfeed)
  :custom (elfeed-search-filter "@6months"))

(use-package elfeed-org :ensure t :config (elfeed-org))

(use-package elfeed-webkit
  :ensure t
  :demand ;; !
  :bind (:map elfeed-show-mode-map ("w" . elfeed-webkit-toggle))
  :custom (elfeed-webkit-auto-enable-tags '(webkit comics))
  :config (elfeed-webkit-auto-toggle-by-tag))

(use-package elfmt :ensure t :vc
  (:url "https://github.com/riscy/elfmt"))

(use-package exec-path-from-shell :ensure t :if
  (memq window-system '(ns x))
  :config (exec-path-from-shell-initialize))

(use-package ghostel :ensure t :bind ("C-c s" . ghostel))

(use-package google-translate
  :ensure t
  :bind ("C-c t" . google-translate-smooth-translate)
  ("C-c T" . google-translate-at-point)
  :custom (google-translate-output-destination '(echo-area))
  (google-translate-show-phonetic t)
  (google-translate-translation-directions-alist
   '(("ja" . "en")
     ("en" . "ja"))))

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-word-at-point)
  (:map osx-dictionary-mode-map ("q" . my/osx-dictionary-quit))
  :config (defun my/osx-dictionary-quit
	      (&optional kill)
	    "Quit the dictionary window; with a prefix arg KILL the buffer instead of burying it."
	    (interactive "P")
	    (let ((prev osx-dictionary-previous-window-configuration))
	      (if kill (kill-buffer) (bury-buffer))
	      (when (window-configuration-p prev)
		(set-window-configuration prev)
		(setq osx-dictionary-previous-window-configuration nil)))))

(use-package lorem-ipsum :ensure t)

(use-package markdown-indent-mode :ensure t :hook (markdown-ts-mode))

(use-package nerd-icons-dired :ensure t :hook dired-mode)

(use-package obsidian-cli
  :ensure t
  :vc (:url "git@github.com:leaferiksen/obsidian-cli.el.git")
  :hook (markdown-ts-mode)
  :bind (:prefix "C-c o" :prefix-map obsidian-cli-actions
		 ("s" . obsidian-cli-search-notes)
		 ("d" . obsidian-cli-open-daily-note)
		 ("z" . obsidian-cli-zip-vault)
		 ("b" . obsidian-cli-jump-to-backlink))
  :custom (obsidian-cli-note-extensions '("md" "tsv"))
  (obsidian-cli-rename-on-save t))

(use-package spacious-padding :ensure t :config
  (spacious-padding-mode))

(use-package swift-mode
  :ensure t
  :if (memq window-system '(ns))
  :mode "\\.swift\\'"
  :hook (swift-mode . eglot-ensure)
  :bind (:prefix "C-c x" :prefix-map xcode
		 ("b" . xcode-build)
		 ("r" . xcode-run)
		 ("t" . xcode-test))
  :config (with-eval-after-load 'eglot
	    (add-to-list 'eglot-server-programs
			 '(swift-mode . ("xcrun" "sourcekit-lsp"))))
  ;; https://danielde.dev/blog/emacs-for-swift-development
  (defun xcode--do (&rest verbs)
    (ns-do-applescript
     (format "tell application \"Xcode\"
if (count of workspace documents) > 0 then
set d to active workspace document
%s
end if
end tell"
             (mapconcat (lambda (v) (concat v " d")) verbs "\n"))))
  (defun xcode-build ()
    "Build the active workspace."
    (interactive)
    (xcode--do "build"))
  (defun xcode-run ()
    "Stop and run the active workspace."
    (interactive)
    (xcode--do "stop" "run"))
  (defun xcode-test ()
    "Stop and test the active workspace."
    (interactive)
    (xcode--do "stop" "test")))

(use-package typo :ensure t :hook text-mode)

(use-package typst-ts-mode :ensure t :vc
  (:url "https://codeberg.org/meow_king/typst-ts-mode")
  :mode "\\.typ\\'" :config
  (add-to-list 'treesit-language-source-alist
	       '(typst "https://github.com/uben0/tree-sitter-typst")))

(use-package visual-fill-column :ensure t :hook
  (org-mode markdown-ts-mode)
  :custom (visual-fill-column-center-text t)
  (visual-fill-column-width 90))

(use-package writegood-mode :ensure t :vc
  (:url "https://github.com/bnbeckwith/writegood-mode")
  :bind ("C-c g" . writegood-mode))

(provide 'init)
;;; init.el ends here

;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-

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
  :custom (auto-insert-directory "~/.config/emacs/templates/")
  (auto-insert-query nil)
  (auto-save-default nil)
  (backward-delete-char-untabify-method nil)
  (column-number-mode t)
  (cursor-type 'bar)
  (custom-file (make-temp-file "~/.cache/emacs/custom"))
  (delete-selection-mode t)
  (disabled-command-function nil)
  (eldoc-help-at-pt t)
  (eldoc-echo-area-prefer-doc-buffer)
  (eldoc-echo-area-use-multiline-p t)
  (electric-pair-mode t)
  (find-file-visit-truename t)
  (gc-cons-threshold 100000000)
  (ibuffer-human-readable-size t)
  (inhibit-startup-screen t)
  (isearch-lazy-count t)
  (large-file-warning-threshold 1000000000)
  (make-backup-files nil)
  (mode-line-collapse-minor-modes '(not flymake-mode))
  (package-vc-allow-build-commands t)
  (read-buffer-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (shr-width 80)
  (shr-max-image-proportion 0.6)
  (speedbar-window-default-width 30)
  (speedbar-window-max-width 20)
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes t)
  (use-dialog-box nil)
  (use-package-vc-prefer-newest t)
  (package-vc-register-as-project nil)
  (user-full-name "Leaf Eriksen")
  (user-mail-address "leaferiksen@gmail.com")
  (vc-auto-revert-mode t)
  (vc-allow-rewriting-published-history t)
  (vc-dir-auto-hide-up-to-date 'revert)
  (which-key-mode t)
  (word-wrap-by-category t)
  :config (setenv "GIT_EDITOR" "emacsclient")
  (defun unfill ()
    "Unfill the current region if active, or the current paragraph."
    (interactive)
    (let ((fill-column (point-max)))
      (if (use-region-p)
          (fill-region (region-beginning) (region-end) nil)
        (fill-paragraph nil))))
  (add-to-list 'imagemagick-enabled-types 'JXL)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Enable or disable global minor modes
  (auto-insert-mode 1)
  (define-auto-insert "\.html" "insert.html")
  (define-auto-insert "\.js" "insert.js")
  (auto-save-visited-mode 1)
  (delete-selection-mode 1)
  (fido-vertical-mode 1)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)
  (repeat-mode 1))

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
  ;; remove scroll zoom (highly incompatible with macos native inertia)
  ("C-<wheel-up>" . mwheel-scroll)
  ("C-<wheel-down>" . mwheel-scroll)
  ("C-M-<wheel-up>" . mwheel-scroll)
  ("C-M-<wheel-down>" . mwheel-scroll)
  :custom (delete-by-moving-to-trash t)
  (mac-function-modifier 'hyper)
  ;; (mac-control-modifier 'meta)
  ;; (mac-right-control-modifier 'control)
  (mac-option-modifier 'none)
  :config ;; Nerd Font Core Icons: Unicode Plane 0 (BMP)
  (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font")
  ;; Nerd Fonts Material Design Icons: Unicode Plane 15 (PUA-A)
  (set-fontset-font t '(#xF0001 . #xF1AF0) "Symbols Nerd Font")
  ;; SF Symbols: Unicode Plane 16 (PUA-B)
  (set-fontset-font t '(#x100000 . #x10FFFD) "SF Pro Display")
  ;; Transpose unwanted s- bindings to project, bookmark, and treesit navigation
  (define-key key-translation-map (kbd "s-g") (kbd "M-g"))
  (define-key key-translation-map (kbd "s-o") (kbd "C-x p"))
  (define-key key-translation-map (kbd "s-r") (kbd "C-x r"))
  (dolist (key
	   '("a" "b" "d" "e" "f" "k" "l" "n" "p" "t" "u" "y" "<backspace>"))
    (define-key key-translation-map
		(kbd (concat "s-" key))
		(kbd (concat "C-M-" key))))
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

(defun almost-maximize-frame ()
  "Borderless maximise with margins for tiling."
  (interactive)
  ;; (add-to-list 'default-frame-alist '(undecorated-round . t))
  (set-frame-width
   (selected-frame)
   (- (display-pixel-width) 85)
   nil t))
(use-package window
  :hook (emacs-startup . almost-maximize-frame)
  :custom (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 3)
  (frame-resize-pixelwise t)
  :config (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

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
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light
       (load-theme 'modus-operandi-tinted t))
      ('dark
       (load-theme 'modus-vivendi-tinted t)))))

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
  :after ls-lisp
  :hook (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :custom (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-omit-verbose nil)
  (dired-recursive-copies 'always)
  (dired-omit-files "\\`[.][.]?\\'\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd"))

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
  :hook (eglot-managed-mode-hook)
  (emacs-lisp-mode . flymake-avoid-scratch)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error))
  :config (defun flymake-avoid-scratch
	      ()
	    (when (buffer-file-name) (flymake-mode 1))))

(use-package html-mode
  ;; mhtml-mode causes issues with apheleia
  :mode ("\\.html\\'" . html-mode))

(use-package ls-lisp
  :custom (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-localized-time-format t))

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :hook (markdown-ts-mode . eglot-ensure)
  (markdown-ts-mode . variable-pitch-mode)
  ;; https://writewithharper.com/docs/integrations/emacs#Optional-Configuration
  (markdown-ts-mode
   .
   (lambda ()
     (setq-local eglot-workspace-configuration
		 '(:harper-ls
		   (:dialect "American" :linters
			     (:LongSentences :json-false :AvoidCurses :json-false))))))
  :bind ("<tab>" . markdown-ts-demote)
  ("<backtab>" . markdown-ts-promote)
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
    (if (and
	 (not (string-prefix-p "#" url))
	 (not (string-match-p "\\`[a-z]+:" url))
	 (not (string-match-p "mailto:" url))
	 (not (string-match-p "\\.[a-zA-Z]+" url)))
        (funcall orig-fn beg end (concat url ".md"))
      (funcall orig-fn beg end url)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(markdown-ts-mode . ("harper-ls" "--stdio")))
    (add-hook
     'eglot-managed-mode-hook
     (lambda ()
       (when anglish-mode
	 (add-hook 'flymake-diagnostic-functions #'anglish--check-buffer nil t)
	 (flymake-start)))))
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

(use-package open-init
  :bind ([remap customize] . open-init)
  :init (defun open-init
	    ()
	  (interactive)
	  (find-file "~/.config/emacs/init.el")))

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
  (defun project-run (label msg &rest args)
    "Run ARGS as a process LABEL in project root, showing MSG."
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (buf (format "*%s:%s*" label (project-name project))))
      (when (get-buffer buf) (kill-buffer buf))
      (apply #'start-process label buf args)
      (when msg (message msg (project-name project)))))
  (defun project-npm-run ()
    "Prompt to run an npm script listed in the project's package.json.

Reads the script names from package.json's `scripts` field, prompts among
the available names, and runs the chosen one via `project-run`."
    (interactive)
    (let* ((project (project-current t))
           (root (project-root project))
           (pj (expand-file-name "package.json" root)))
      (unless (file-exists-p pj)
	(user-error "No package.json found at %s" root))
      (let ((names
             (with-temp-buffer
               (insert-file-contents pj)
               (let ((scripts (cdr (assoc 'scripts (json-read)))))
                 (when (listp scripts)
                   (mapcar
                    #'(lambda (x)
			(let ((k (if (consp x) (car x) x)))
                          (if (symbolp k) (symbol-name k) k)))
                    scripts))))))
        (unless names (user-error "No scripts defined in %s" pj))
        (let ((script
	       (completing-read
		(format "npm run (available: %s)"
			(mapconcat #'identity names ", "))
		names nil t)))
          (project-run script
		       (format "Running npm run %s in %s" script "%s")
		       "npm" "run" script))))))

(use-package visual-wrap-prefix-mode :hook (prog-mode html-mode))

(use-package xwidget :bind
  (:map xwidget-webkit-mode-map ("u" . xwidget-webkit-browse-url)))

(use-package yt-dlp
  :bind ("C-c y" . yt-dlp-download)
  :init (defun yt-dlp-download
	    ()
	  (interactive)
	  (let* ((v (y-or-n-p "Video? "))
		 (s (and v (y-or-n-p "Subs? ")))
		 (c (and v (y-or-n-p "Backwards-compatible (h264)? ")))
		 (u
		  (or (current-kill 0) (user-error "Nothing in clipboard")))
		 (f
		  (concat
		   (or (and s "--write-subs") (and v "") "-x")
		   (and c " -S vcodec:h264"))))
	    (unless (string-empty-p u)
              (async-shell-command
	       (format "yt-dlp %s %s" f (shell-quote-argument u)))))))

;;; External packages

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

(use-package agent-shell
  :ensure t
  :hook (agent-shell-mode . completion-preview-mode)
  (agent-shell-mode . variable-pitch-mode)
  :bind ("C-c c" . agent-shell-new-temp-shell)
  :custom (agent-shell-preferred-agent-config 'opencode)
  :init (with-eval-after-load 'project
	  (define-key project-prefix-map (kbd "a") #'agent-shell)))

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
  (:prefix "C-c p" :prefix-map dwim-print
	   ("m" . dwim-file-to-mla-pdf)
	   ("s" . dwim-file-to-pdf)
	   ("p" . dwim-md-to-pptx))
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
  (defun dwim-file-to-pdf ()
    "Convert file to pdf via pandoc and typst."
    (interactive)
    (dwim-shell-command-on-marked-files "Converting to pdf" "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=/Users/leaf/.config/typst/resume.typ"))
  (defun dwim-file-to-mla-pdf ()
    "Convert file to MLA-compliant pdf via pandoc and typst."
    ;; fonttools varLib.mutator '/Users/leaf/Library/Fonts/AtkinsonHyperlegibleNext[wght].ttf' wght=400
    ;; pandoc --print-default-template=typst
    (interactive)
    (dwim-shell-command-on-marked-files "Converting to MLA-compliant pdf" "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=/Users/leaf/.config/typst/mla-template.typ"))
  (defun dwim-md-to-pptx ()
    "Convert md files to pptx."
    (interactive)
    (if-let* ((files (dwim-shell-command--files)) ;; * may break
              ((seq-every-p
		(apply-partially #'string-suffix-p ".md")
		files)))
        (dwim-shell-command-on-marked-files "Converting md to pptx" "npx @marp-team/marp-cli@latest '<<f>>' --pptx")
      (user-error "Selection contains non-markdown files!"))))

(use-package elfeed :ensure t :after elfeed-org :bind
  ("C-c f" . elfeed)
  :custom (elfeed-search-filter "@6months"))

(use-package elfeed-org :ensure t :config (elfeed-org))

(use-package elfeed-webkit
  :ensure t
  :demand ;; !
  :init (setq elfeed-webkit-auto-enable-tags '(webkit comics))
  :config (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map ("w" . elfeed-webkit-toggle)))

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
  :init (setopt
	 google-translate-output-destination
	 '(echo-area)
	 google-translate-show-phonetic t
	 google-translate-translation-directions-alist
	 '(("ja" . "en")
	   ("en" . "ja"))))

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-word-at-point)
  (:map osx-dictionary-mode-map ("C-u q" . my/osx-dictionary-kill))
  :config (defun my/osx-dictionary-kill
	      ()
	    "Like `osx-dictionary-quit', but kill the dictionary buffer."
	    (interactive)
	    (let ((prev
		   (when (and osx-dictionary-previous-window-configuration
			      (window-configuration-p osx-dictionary-previous-window-configuration))
		     osx-dictionary-previous-window-configuration)))
	      (kill-buffer)
	      (when prev
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

(use-package reader
  :ensure t
  :vc (:url "https://codeberg.org/MonadicSheep/emacs-reader" :make "all")
  :config (defun fix-reader
	      ()
	    "Recompile Reader Libraries"
	    (interactive)
	    (let ((default-directory "~/.config/emacs/elpa/reader/"))
	      (shell-command "make clean all"))))

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
			 '(swift-ts-mode . ("xcrun" "sourcekit-lsp"))))
  ;; https://danielde.dev/blog/emacs-for-swift-development
  (defun xcode-build ()
    "Build the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        build targetProject
      end if
    end tell"))
  (defun xcode-run ()
    "Stop and run the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        stop targetProject
        run targetProject
      end if
    end tell"))
  (defun xcode-test ()
    "Stop and test the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        stop targetProject
        test targetProject
      end if
    end tell")))

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

;; Local variables:
;; no-byte-compile: t
;; end:

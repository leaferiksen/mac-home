;;; init.el --- Emacs 31 Initialization -*- lexical-binding: t; no-byte-compile: t; fill-column: 120;-*-

;; Author: Leaf Eriksen <leaferiksen@gmail.com>

;;; Commentary:

;; No `use-package'. Per package, in this order:
;;   1. loading declarations (auto-mode-alist, add-hook)
;;   2. keybindings (keymap-set / keymap-global-set / defvar-keymap)
;;   3. config, wrapped in `with-eval-after-load' unless the package is
;;      already guaranteed to be loaded at that point (built-ins that
;;      load at startup, or packages just `require'd above the config).
;;
;; Top level functions are sorted primarily by priority,
;; secondarily by alphabet.

;;; Code:

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; environment
(setenv "GIT_EDITOR" "emacsclient")

;; emacs
(add-to-list 'default-frame-alist '(internal-border-width . 15))
(add-to-list 'default-frame-alist '(right-divider-width . 1))
(add-to-list 'default-frame-alist '(bottom-divider-width . 1))
(add-to-list 'default-frame-alist '(left-fringe . 10))
(add-to-list 'default-frame-alist '(right-fringe . 10))
(defun almost-maximize-frame ()
  "Borderless maximise with margins for tiling."
  (interactive)
  ;; (add-to-list 'default-frame-alist '(undecorated-round . t))
  (set-frame-width (selected-frame) (- (display-pixel-width) 85) nil t))
(defun unfill ()
  "Unfill the current region if active, or the current paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (if (use-region-p)
	(fill-region (region-beginning) (region-end) nil)
      (fill-paragraph nil))))
(add-hook 'after-init-hook #'almost-maximize-frame)
(add-hook 'emacs-startup-hook #'server-start)
(windmove-default-keybindings 'super)
;; Hide menu-bar entries that are not from the global keymap
(let ((map (make-sparse-keymap)))
  (add-to-list 'emulation-mode-map-alists `((t . ,map)))
  (add-hook 'menu-bar-update-hook
            (lambda ()
              (setcdr map nil)
              (dolist (m (remq map (current-active-maps)))
                (let ((menu (lookup-key m [menu-bar])))
                  (when (keymapp menu)
                    (map-keymap
                     (lambda (key _)
                       (unless (memq key '(file edit options buffer tools help-menu))
                         (define-key map (vector 'menu-bar key) 'undefined)))
                     menu)))))))

;; speedbar
(add-hook 'emacs-startup-hook #'speedbar)
(add-hook 'emacs-startup-hook #'nerd-icons-speedbar-mode)
(defun speedbar-window-width-threshold ()
  "Frame width, in columns, below which `speedbar-window' is hidden.
Always three times the current `speedbar-window-default-width'."
  (* 3 speedbar-window-default-width))
(defun my-speedbar-auto-toggle (frame)
  "Show or hide `speedbar-window' on FRAME based on its width.
`speedbar-window' is pinned at `speedbar-window-default-width'."
  (and-let*
      (((frame-live-p frame))
       ((< (frame-width frame) (speedbar-window-width-threshold)))
       ((window-live-p speedbar--window)))
    (speedbar-window-mode -1))
  (and-let*
      (((frame-live-p frame))
       ((>= (frame-width frame) (speedbar-window-width-threshold)))
       ((not (window-live-p speedbar--window))))
    (setq speedbar--window-width speedbar-window-default-width)
    (speedbar-window-mode 1))
  (when-let* (((window-live-p speedbar--window))
	      (delta (- speedbar-window-default-width (window-width speedbar--window)))
	      ((not (zerop delta))))
    (ignore-errors (window-resize speedbar--window delta t))))
(add-hook 'window-size-change-functions #'my-speedbar-auto-toggle)

;; auto-insert
(define-auto-insert "\\.html\\'" "insert.html")
(define-auto-insert "\\.js\\'" "insert.js")

;; completion-preview-mode (built-in; keymap only exists once loaded)
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "M-]" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-[" #'completion-preview-prev-candidate))

;; editorconfig
(with-eval-after-load 'editorconfig (add-to-list 'editorconfig-indentation-alist '(js-json-mode js-indent-level)))

;; image-mode
(add-to-list 'imagemagick-enabled-types 'JXL)

;; ns (macOS)
(when (eq window-system 'ns)
  ;; fonts
  ;; Nerd Font Core Icons: Unicode Plane 0 (BMP)
  (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font")
  ;; Nerd Fonts Material Design Icons: Unicode Plane 15 (PUA-A)
  (set-fontset-font t '(#xF0001 . #xF1AF0) "Symbols Nerd Font")
  ;; SF Symbols: Unicode Plane 16 (PUA-B)
  (set-fontset-font t '(#x100000 . #x10FFFD) "SF Pro Display")

  ;; key-translation-map
  ;; Transpose unwanted s- bindings to project, bookmark, and treesit navigation
  (keymap-set key-translation-map "s-g" "M-g")
  (keymap-set key-translation-map "s-o" "C-x p")
  (keymap-set key-translation-map "s-r" "C-x r")
  (dolist (key '("a" "b" "d" "e" "f" "k" "l" "n" "p" "t" "u" "y" "<backspace>"))
    (keymap-set key-translation-map (concat "s-" key) (concat "C-M-" key)))

  ;; dired (ns)
  (defun dired-install-dmg ()
    "Mount a .dmg file at point, copy its .app to ~/Applications/, then eject and optionally delete .dmg."
    (interactive)
    (if-let* ((dmg (dired-get-filename))
	      (mount-output
	       (shell-command-to-string (format "yes | hdiutil attach -nobrowse %s" (shell-quote-argument dmg))))
	      ((string-match "/Volumes/[^\t\n]+" mount-output))
	      (volume (string-trim-right (match-string 0 mount-output)))
	      (app (car (file-expand-wildcards (concat volume "/*.app")))))
	(progn
	  (make-directory "~/Applications/" t)
	  (shell-command (format "cp -R %s ~/Applications/" (shell-quote-argument app)))
	  (shell-command (format "hdiutil detach %s" (shell-quote-argument volume)))
	  (when (y-or-n-p (format "Installed %s to ~/Applications/ — trash the DMG?" (file-name-nondirectory app)))
	    (shell-command (format "trash %s" (shell-quote-argument dmg)))
	    (revert-buffer)))
      (message "Installation failed: could not mount DMG or find .app bundle")))

  ;; xcode
  ;; https://danielde.dev/blog/emacs-for-swift-development
  (defun xcode--do (&rest verbs)
    (dolist (v verbs)
      (ns-do-applescript
       (format "tell application \"Xcode\" to if (count of workspace documents) > 0 then %s (active workspace document)" v))))
  (defun xcode-build () "Build the active workspace." (interactive) (xcode--do "build"))
  (defun xcode-run () "Stop and run the active workspace." (interactive) (xcode--do "stop" "run"))
  (defun xcode-test () "Stop and test the active workspace." (interactive) (xcode--do "stop" "test"))

  (defun auto-theme (appearance)
    "Load theme matching system APPEARANCE."
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (if (eq appearance 'dark) 'modus-vivendi-tinted 'modus-operandi-tinted) t))
  ;; term/ns-win (loaded by the NS port during startup, before init.el runs)
  (add-hook 'ns-system-appearance-change-functions #'auto-theme)
  (keymap-global-set "s-z" #'undo-only)
  (keymap-global-set "s-Z" #'undo-redo)
  (keymap-global-set "s-w" #'kill-current-buffer)
  (keymap-global-set "C-M-y" #'yank-pop)
  ;; enable standard macOS emoji binding
  (keymap-global-set "H-e" #'ns-do-show-character-palette)
  (keymap-global-set "H-f" #'toggle-frame-fullscreen)

  ;; osx-dictionary
  (keymap-global-set "C-c d" #'osx-dictionary-search-word-at-point)

  ;; swift-mode
  ;; Swift ts-modes are reliant on unfinished tree sitters
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
  (add-hook 'swift-mode-hook #'eglot-ensure)
  (defvar-keymap xcode-prefix-map "b" #'xcode-build "r" #'xcode-run "t" #'xcode-test)
  (keymap-set global-map "C-c x" xcode-prefix-map)
  (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp"))))

  ;; exec-path-from-shell (no deferring hook/bind/mode, so load it now)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; project
(defun project-npm-run ()
  "Run an npm script from this project's package.json."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
	 (scripts
	  (with-temp-buffer
	    (unless (file-exists-p "package.json") (user-error "No package.json in %s" default-directory))
	    (insert-file-contents "package.json")
	    (mapcar #'car (alist-get 'scripts (json-parse-buffer :object-type 'alist)))))
	 (script (completing-read "npm run: " scripts nil t)))
    (compile (format "npm run %s" script))))
(keymap-set project-prefix-map "s" #'ghostel-project)
(keymap-set project-prefix-map "n" #'project-npm-run)

;; yt-dlp
(defun yt-dlp-download ()
  "Download the URL in the clipboard with yt-dlp, then jump to it in Dired."
  (interactive)
  (let* ((url (or (current-kill 0) (user-error "Nothing in clipboard")))
	 (video (y-or-n-p "Video? "))
	 (flags
          (concat
           (if video (and (y-or-n-p "Subs? ") "--write-subs") "-x")
           (and video (y-or-n-p "Backwards-compatible (h264)? ") " -S vcodec:h264")))
	 (command (format "yt-dlp --quiet %s %s --print after_move:filepath" flags (shell-quote-argument url))))
    (make-process
     :name "yt-dlp-download"
     :buffer (generate-new-buffer " *yt-dlp-output*")
     :command (list shell-file-name shell-command-switch command)
     :sentinel (lambda
		 (proc _event)
		 (when-let* (((eq (process-status proc) 'exit))
			     (buf (process-buffer proc)))
		   (if (/= (process-exit-status proc) 0)
		       (progn (message "yt-dlp failed") (display-buffer buf))
		     (if-let* ((filepath (string-trim (with-current-buffer buf (buffer-string))))
			       ((file-exists-p filepath)))
			 (progn (kill-buffer buf) (dired-jump nil filepath))
		       (message "yt-dlp: unexpected output: %s" filepath))))))
    (message "yt-dlp: downloading…")))
(keymap-global-set "C-c y" #'yt-dlp-download)

;; agent-shell
(add-hook 'agent-shell-mode-hook #'variable-pitch-mode)
(keymap-global-set "C-c c" #'agent-shell-new-temp-shell)
(keymap-set project-prefix-map "a" #'agent-shell)

;; apheleia / elfmt
(require 'elfmt)
(require 'apheleia)
(defconst apheleia-elfmt-skip-forms-re
  (rx bol "(custom-set-" (or "variables" "faces") symbol-end)
  "Top-level forms elfmt should leave untouched.
Custom owns the formatting of these and will rewrite them anyway.")
(cl-defun apheleia-elfmt (&key buffer scratch callback &allow-other-keys)
  "Format SCRATCH with `elfmt', then invoke CALLBACK.
Indentation settings are copied from BUFFER so the result matches
what you'd get by typing TAB there. Skips `custom-set-variables'
and `custom-set-faces' forms, which Custom formats itself."
  (let ((fc (buffer-local-value 'fill-column buffer))
	(tabs (buffer-local-value 'indent-tabs-mode buffer))
	(indent-fn (buffer-local-value 'lisp-indent-function buffer))
	(original (with-current-buffer scratch (buffer-string))))
    (with-current-buffer scratch
      (delay-mode-hooks (emacs-lisp-mode))
      (setq-local fill-column fc indent-tabs-mode tabs lisp-indent-function indent-fn)
      (condition-case err
          (let ((gc-cons-threshold most-positive-fixnum)
		(inhibit-message t)
		(message-log-max nil))
            (goto-char (point-max))
            (while (not (bobp))
              (backward-sexp)
              (unless (looking-at-p apheleia-elfmt-skip-forms-re) (elfmt--sexp))))
        (error
         (erase-buffer)
         (insert original)
         (message "elfmt: %s" (error-message-string err))))))
  (funcall callback))
(setf (alist-get 'elfmt apheleia-formatters) #'apheleia-elfmt)
(setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'elfmt)

;; csv-mode
(add-hook 'csv-mode-hook #'csv-align-mode)

;; dired
;; Requires ls-lisp for directory sorting
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(with-eval-after-load 'dired (require 'ls-lisp))

;; dwim-shell-command (demanded: loaded eagerly)
(require 'dwim-shell-command)
(defun dwim-file-mediainfo ()
  "Run mediainfo on the current buffer's file or marked Dired files."
  (interactive)
  (dwim-shell-command-on-marked-files "MediaInfo" "mediainfo '<<f>>'" :utils "mediainfo"))
(defun dwim-file-to-pdf ()
  "Convert marked files to PDF via pandoc and typst.
Prompts for a template: [m]LA, [r]esume, or [d]efault (no template)."
  (interactive)
  (let* ((choice (read-char-choice "Template: [m]LA, [r]esume, [d]efault? " '(?m ?r ?d)))
	 (template-flag
          (pcase choice
            (?m (format " --template=%s" (expand-file-name "mla-template.typ" "~/.config/typst/")))
            (?r (format " --template=%s" (expand-file-name "resume.typ" "~/.config/typst/")))
            (?d ""))))
    (dwim-shell-command-on-marked-files "Converting to pdf"
					(format "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst%s" template-flag)
					:silent-success t)))
(keymap-global-set "s-i" #'dwim-file-mediainfo)
(keymap-set global-map "<remap> <shell-command>" #'dwim-shell-command)
(keymap-global-set "C-c p" #'dwim-file-to-pdf)
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "<remap> <dired-do-async-shell-command>" #'dwim-shell-command)
  (keymap-set dired-mode-map "<remap> <dired-do-shell-command>" #'dwim-shell-command)
  (keymap-set dired-mode-map "<remap> <dired-smart-shell-command>" #'dwim-shell-command)
  (keymap-set dired-mode-map "e" #'dwim-shell-commands-macos-open-with)
  (keymap-set dired-mode-map "i" #'dwim-file-mediainfo))

;; eglot
;; Flymake is called by eglot automatically
;; No eager `require': loading eglot during init calls `char-displayable-p' before the
;; frame's fonts are ready, which crashes Emacs 31 (SIGBUS in font_style_to_value)
;; about half the time on macOS.
(dolist (hook '(html-mode-hook css-ts-mode-hook js-ts-mode-hook markdown-ts-mode-hook)) (add-hook hook #'eglot-ensure))
(defvar-keymap eglot-actions-prefix-map "r" #'eglot-rename "a" #'eglot-code-actions "o" #'eglot-code-action-organize-imports "d" #'eldoc "f" #'eglot-format)
(keymap-global-set "C-c a" eglot-actions-prefix-map)
(with-eval-after-load 'eglot (keymap-set eglot-mode-map "H-<mouse-1>" #'eglot-code-actions-at-mouse))

;; elfeed
(keymap-global-set "C-c f" #'elfeed)
(with-eval-after-load 'elfeed (keymap-set elfeed-show-mode-map "w" #'elfeed-webkit-toggle) (elfeed-org))

;; flymake
(dolist (hook '(emacs-lisp-mode-hook)) (add-hook hook #'flymake-mode))
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))

;; google-translate
(keymap-global-set "C-c t" #'google-translate-smooth-translate)
(keymap-global-set "C-c T" #'google-translate-at-point)

;; hackernews
(keymap-global-set "C-c h" #'hackernews)

;; html-mode
;; mhtml-mode causes issues with apheleia
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(add-hook 'html-mode-hook #'visual-wrap-prefix-mode)

;; markdown-ts-mode
(defun markdown-h1-title ()
  "Insert an atx level 1 heading with the name of the file."
  (interactive)
  (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
(defun markdown-h2-today ()
  "Insert a level 2 heading with today's date in iso format."
  (interactive)
  (insert "## " (format-time-string "%Y-%m-%d") "\n"))
(defun markdown-mla-frontmatter ()
  "Insert frontmatter for an MLA heading."
  (interactive)
  (insert "---\nprofessor: \nclass: \nword-count: true\n---\n"))
(defun markdown-ts-make-link-button-advice (orig-fn beg end url)
  "Treat extensionless wiki links as markdown files."
  (funcall orig-fn beg end (if (string-match-p "\\`#\\|\\`[a-z]+:\\|\\.[a-zA-Z]+" url) url (concat url ".md"))))
;; Built into Emacs 31, but still experimental and not yet self-autoloading,
;; so it has to be pulled in explicitly before it can go in auto-mode-alist.
(require 'markdown-ts-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
(dolist (hook '(variable-pitch-mode visual-fill-column-mode markdown-indent-mode obsidian-cli-mode typo-mode))
  (add-hook 'markdown-ts-mode-hook hook))
(defvar-keymap markdown-actions-prefix-map "1" #'markdown-h1-title "2" #'markdown-h2-today "f" #'markdown-mla-frontmatter)
(keymap-global-set "C-c m" markdown-actions-prefix-map)
(with-eval-after-load 'markdown-ts-mode
  (keymap-set markdown-ts-mode-map "<tab>" #'markdown-ts-demote)
  (keymap-set markdown-ts-mode-map "<backtab>" #'markdown-ts-promote)
  ;; Match modus headings
  (dolist (n (number-sequence 1 6))
    (set-face-attribute
     (intern (format "markdown-ts-heading-%d" n))
     nil :inherit
     (intern (format "modus-themes-heading-%d" n))))
  ;; Fix extensionless wikilinks
  (advice-add 'markdown-ts--make-link-button :around #'markdown-ts-make-link-button-advice)
  ;; https://writewithharper.com/docs/integrations/emacs#Optional-Configuration
  (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(markdown-ts-mode . ("harper-ls" "--stdio")))))

;; obsidian-cli
(defvar-keymap obsidian-cli-actions-prefix-map "s" #'obsidian-cli-search-notes "d" #'obsidian-cli-open-daily-note "z" #'obsidian-cli-zip-vault "b" #'obsidian-cli-jump-to-backlink)
(keymap-global-set "C-c o" obsidian-cli-actions-prefix-map)

;; prog-mode
;; does not cover languages that inherit from sgml
(add-hook 'prog-mode-hook #'visual-wrap-prefix-mode)

;; typst-ts-mode
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
(with-eval-after-load 'typst-ts-mode
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst")))

;; writegood-mode
(keymap-global-set "C-c g" #'writegood-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-preferred-agent-config 'opencode)
 '(apheleia-global-mode t)
 '(auto-insert-directory "~/.config/emacs/templates/")
 '(auto-insert-mode t)
 '(auto-insert-query nil)
 '(auto-save-default nil)
 '(auto-save-visited-mode t)
 '(backward-delete-char-untabify-method nil)
 '(column-number-mode t)
 '(completion-auto-help nil)
 '(completion-eager-display nil)
 '(completion-eager-update t)
 '(completion-ignore-case t t)
 '(completions-sort 'historical)
 '(context-menu-mode t)
 '(csv-align-max-width 72)
 '(csv-align-padding 3)
 '(cursor-type 'bar)
 '(delete-selection-mode t)
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-create-destination-dirs 'ask)
 '(dired-dwim-target t)
 '(dired-mouse-drag-files t)
 '(dired-omit-files "^\\(\\.\\.?$\\|\\.DS_Store$\\|\\.localized$\\)")
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(disabled-command-function nil t)
 '(display-line-numbers-type 'relative)
 '(display-line-numbers-width-start 3)
 '(dwim-shell-commands-git-clone-dirs '("~/Git"))
 '(editorconfig-mode t)
 '(eglot-autoshutdown t)
 '(eglot-code-action-indications '(mode-line))
 '(eglot-code-action-indicator "*")
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-help-at-pt t)
 '(electric-pair-mode t)
 '(elfeed-search-filter "@6-months")
 '(elfeed-webkit-auto-enable-tags '(webkit comics))
 '(fido-vertical-mode t)
 '(find-file-visit-truename t)
 '(frame-resize-pixelwise t)
 '(gc-cons-threshold 100000000)
 '(global-completion-preview-mode t)
 '(global-hl-line-mode t)
 '(global-nerd-icons-multimodal-mode t)
 '(global-visual-line-mode t)
 '(google-translate-output-destination '(echo-area))
 '(google-translate-show-phonetic t)
 '(google-translate-translation-directions-alist '(("ja" . "en") ("en" . "ja")))
 '(ibuffer-human-readable-size t)
 '(imenu-auto-rescan t)
 '(imenu-flatten 'group)
 '(imenu-space-replacement " ")
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(large-file-warning-threshold 1000000000)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(make-backup-files nil)
 '(markdown-ts-inline-images t)
 '(mode-line-collapse-minor-modes '(not flymake-mode))
 '(modus-themes-common-palette-overrides
   '((fringe unspecified) (border bg-inactive) (border-mode-line-active unspecified) (border-mode-line-inactive unspecified) (underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified) (fg-heading-0 fg-main) (fg-heading-1 fg-main) (fg-heading-2 fg-main) (fg-heading-3 fg-main) (fg-heading-4 fg-main) (fg-heading-5 fg-main) (fg-heading-6 fg-main) (fg-heading-7 fg-main) (fg-heading-8 fg-main)))
 '(modus-themes-italic-constructs t)
 '(modus-themes-mixed-fonts t)
 '(mouse-wheel-scroll-amount
   '(1 ((shift) . hscroll) ((meta)) ((control) . 1) ((control meta) . 1)))
 '(nov-text-width t)
 '(ns-alternate-modifier 'none)
 '(ns-function-modifier 'hyper)
 '(obsidian-cli-note-extensions '("md" "tsv"))
 '(obsidian-cli-rename-on-save t)
 '(package-selected-packages
   '(agent-shell anglish apheleia betweenle clojure-mode csv-mode dwim-shell-command elfeed elfeed-org elfmt exec-path-from-shell ghostel google-translate hackernews lorem-ipsum markdown-indent-mode nerd-icons-multimodal nerd-icons-speedbar obsidian-cli osx-dictionary swift-mode typo typst-ts-mode visual-fill-column writegood-mode))
 '(package-vc-allow-build-commands t)
 '(package-vc-register-as-project nil)
 '(package-vc-selected-packages
   '((nerd-icons-speedbar :vc-backend Git :url "https://github.com/Akane-6730/nerd-icons-speedbar") (betweenle :vc-backend Git :url "https://github.com/vikram-mandyam/betweenle.el") (nerd-icons-multimodal :vc-backend Git :url "https://github.com/abougouffa/nerd-icons-multimodal") (obsidian-cli :url "git@github.com:leaferiksen/obsidian-cli.el.git") (elfmt :url "https://github.com/riscy/elfmt") (anglish :url "git@github.com:leaferiksen/anglish.el.git")))
 '(pop-up-windows nil)
 '(project-mode-line t)
 '(project-vc-extra-root-markers '("project"))
 '(read-buffer-completion-ignore-case t)
 '(read-process-output-max (* 1024 1024) t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(shr-max-image-proportion 0.6)
 '(shr-width 80)
 '(speedbar-directory-unshown-regexp "^\\(\\.\\.?$\\|\\.DS_Store$\\|\\.localized$\\)")
 '(speedbar-initial-expansion-list-name "quick buffers" t)
 '(speedbar-prefer-window t)
 '(speedbar-show-unknown-files t)
 '(speedbar-window-default-width 30)
 '(tool-bar-mode nil)
 '(treesit-auto-install-grammar 'always)
 '(treesit-enabled-modes t)
 '(use-short-answers t)
 '(user-full-name "Leaf Eriksen")
 '(user-mail-address "leaferiksen@gmail.com")
 '(vc-allow-rewriting-published-history t)
 '(vc-auto-revert-mode t)
 '(vc-dir-auto-hide-up-to-date 'revert)
 '(visual-fill-column-width 90)
 '(which-key-mode t)
 '(window-divider-mode t)
 '(word-wrap-by-category t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Maple Mono CN" :height 140))))
 '(fixed-pitch ((t (:inherit default))))
 '(markdown-indent-mode-hide-hash ((t (:inherit shadow))))
 '(variable-pitch ((t (:height 180 :family "Atkinson Hyperlegible Next")))))

(provide 'init)
;;; init.el ends here

;;; init.el --- Emacs 31 Initialization -*- lexical-binding: t; no-byte-compile: t; fill-column: 1000;-*-

;; Author: Leaf Eriksen <leaferiksen@gmail.com>

;;; Commentary:

;; Top level functions are sorted primarily by priority,
;; secondarily by alphabet.

;; `use-package' :key sort order
;; if to load or not (:if :after)
;; when to load what (:demand :mode :commands :hook)
;; what to keys to bind (:bind :prefix :map)
;; what variables to set (:custom-face :custom)
;; what functions to run when (:init :config)

;;; Code:

;; Internal features and hooks

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
 '(editorconfig-mode t)
 '(eglot-autoshutdown t)
 '(eglot-code-action-indications '(mode-line))
 '(eglot-code-action-indicator "*")
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-help-at-pt t)
 '(electric-pair-mode t)
 '(elfeed-search-filter "@6months")
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
   '((underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified) (fg-heading-0 fg-main) (fg-heading-1 fg-main) (fg-heading-2 fg-main) (fg-heading-3 fg-main) (fg-heading-4 fg-main) (fg-heading-5 fg-main) (fg-heading-6 fg-main) (fg-heading-7 fg-main) (fg-heading-8 fg-main)))
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
   '(agent-shell anglish apheleia betweenle clojure-mode csv-mode dwim-shell-command elfeed elfeed-org elfeed-webkit elfmt exec-path-from-shell ghostel google-translate hackernews lorem-ipsum markdown-indent-mode nerd-icons-multimodal obsidian-cli osx-dictionary spacious-padding swift-mode typo typst-ts-mode writegood-mode))
 '(package-vc-allow-build-commands t)
 '(package-vc-register-as-project nil)
 '(package-vc-selected-packages
   '((betweenle :vc-backend Git :url "https://github.com/vikram-mandyam/betweenle.el") (nerd-icons-multimodal :vc-backend Git :url "https://github.com/abougouffa/nerd-icons-multimodal") (obsidian-cli :url "git@github.com:leaferiksen/obsidian-cli.el.git") (elfmt :url "https://github.com/riscy/elfmt") (anglish :url "git@github.com:leaferiksen/anglish.el.git")))
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
 '(spacious-padding-mode t)
 '(speedbar-directory-unshown-regexp "^\\(\\.\\.?$\\|\\.DS_Store$\\|\\.localized$\\)")
 '(speedbar-prefer-window t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(speedbar-window-default-width 40)
 '(tool-bar-mode nil)
 '(treesit-auto-install-grammar 'always)
 '(treesit-enabled-modes t)
 '(use-package-vc-prefer-newest t)
 '(use-short-answers t)
 '(user-full-name "Leaf Eriksen")
 '(user-mail-address "leaferiksen@gmail.com")
 '(vc-allow-rewriting-published-history t)
 '(vc-auto-revert-mode t)
 '(vc-dir-auto-hide-up-to-date 'revert)
 '(visual-fill-column-center-text t)
 '(visual-fill-column-width 90)
 '(which-key-mode t)
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

 ;; '(bold ((t (:weight bold :family "Maple Mono CN"))))
 ;; '(italic ((t (:slant italic :family "Maple Mono CN"))))
 ;; '(tabulated-list-fake-header ((t (:overline t :underline t :weight bold :family "Maple Mono CN"))))

(setenv "GIT_EDITOR" "emacsclient")

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

(defun speedbar-refresh-on-non-file-buffers (&optional _)
  "Refresh Speedbar when switching to a non-file buffer."
  (when-let* (((not (active-minibuffer-window)))
	      ((not (minibufferp)))
	      ((not buffer-file-name))
	      ((not (string-prefix-p " " (buffer-name))))
	      ((not (derived-mode-p 'speedbar-mode)))
	      (is-open
	       (or
		(and (boundp 'speedbar-window) (window-live-p speedbar-window))
		(and (boundp 'speedbar-frame) (frame-live-p speedbar-frame)))))
    (let ((inhibit-message t)) (speedbar-refresh))))

(defun yt-dlp-download ()
  "Download the URL in the clipboard with yt-dlp."
  (interactive)
  (let* ((url (or (current-kill 0) (user-error "Nothing in clipboard")))
	 (video (y-or-n-p "Video? "))
	 (flags
	  (concat
	   (if video (and (y-or-n-p "Subs? ") "--write-subs") "-x")
	   (and video (y-or-n-p "Backwards-compatible (h264)? ") " -S vcodec:h264"))))
    (async-shell-command (format "yt-dlp %s %s" flags (shell-quote-argument url)))))

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

(add-to-list 'editorconfig-indentation-alist '(js-json-mode js-indent-level))

(add-to-list 'imagemagick-enabled-types 'JXL)

(define-auto-insert "\\.html\\'" "insert.html")

(define-auto-insert "\\.js\\'" "insert.js")

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

(use-package emacs :hook
  ((after-init . almost-maximize-frame)
   (emacs-startup . server-start)
   (emacs-startup . speedbar)
   (window-buffer-change-functions . speedbar-refresh-on-non-file-buffers))
  :bind (("C-c s" . speedbar)
	 ("C-c y" . yt-dlp-download)
	 (:map project-prefix-map ("s" . ghostel-project) ("n" . project-npm-run))
	 (:map completion-preview-active-mode ("M-]" . completion-preview-next-candidate) ("M-[" . completion-preview-prev-candidate))))

(when (eq window-system 'ns)
  ;; Nerd Font Core Icons: Unicode Plane 0 (BMP)
  (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font")
  ;; Nerd Fonts Material Design Icons: Unicode Plane 15 (PUA-A)
  (set-fontset-font t '(#xF0001 . #xF1AF0) "Symbols Nerd Font")
  ;; SF Symbols: Unicode Plane 16 (PUA-B)
  (set-fontset-font t '(#x100000 . #x10FFFD) "SF Pro Display")
  ;; Transpose unwanted s- bindings to project, bookmark, and treesit navigation
  (keymap-set key-translation-map "s-g" "M-g")
  (keymap-set key-translation-map "s-o" "C-x p")
  (keymap-set key-translation-map "s-r" "C-x r")
  (dolist (key '("a" "b" "d" "e" "f" "k" "l" "n" "p" "t" "u" "y" "<backspace>"))
    (keymap-set key-translation-map (concat "s-" key) (concat "C-M-" key)))
  (defun dired-install-dmg ()
    "Mount a .dmg file at point, copy its .app to ~/Applications/, then eject and optionally delete .dmg."
    (interactive)
    (if-let* ((dmg (dired-get-filename))
	      (mount-output (shell-command-to-string (format "yes | hdiutil attach -nobrowse %s" (shell-quote-argument dmg))))
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
  (defun auto-theme (appearance)
    "Load theme matching system APPEARANCE."
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (if (eq appearance 'dark) 'modus-vivendi-tinted 'modus-operandi-tinted) t))
  ;; https://danielde.dev/blog/emacs-for-swift-development
  (defun xcode--do (&rest verbs)
    (dolist (v verbs)
      (ns-do-applescript (format "tell application \"Xcode\" to if (count of workspace documents) > 0 then %s (active workspace document)" v))))
  (defun xcode-build () "Build the active workspace." (interactive) (xcode--do "build"))
  (defun xcode-run () "Stop and run the active workspace." (interactive) (xcode--do "stop" "run"))
  (defun xcode-test () "Stop and test the active workspace." (interactive) (xcode--do "stop" "test"))

  (use-package term/ns-win
    :hook (ns-system-appearance-change-functions . auto-theme)
    :bind ;; modernize undo and remove s-Z to s-z translation map
    ("s-z" . undo-only)
    ("s-Z" . nil)
    ("s-Z" . undo-redo)
    ("s-w" . kill-current-buffer)
    ("C-M-y" . yank-pop)
    ;; enable standard macOS emoji binding
    ("H-e" . ns-do-show-character-palette)
    ("H-f" . toggle-frame-fullscreen))

  (use-package osx-dictionary :bind ("C-c d" . osx-dictionary-search-word-at-point))

  (use-package swift-mode
    ;; Swift ts-modes are reliant on unfinished tree sitters
    :mode "\\.swift\\'"
    :hook (swift-mode . eglot-ensure)
    :bind (:prefix "C-c x" :prefix-map xcode ("b" . xcode-build) ("r" . xcode-run) ("t" . xcode-test))
    :config (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))))

  (use-package exec-path-from-shell :config (exec-path-from-shell-initialize)))

(use-package dired
  ;; Requires ls-lisp for directory sorting
  :hook ((dired-mode . dired-omit-mode)
	 (dired-mode . dired-hide-details-mode))
  :config (require 'ls-lisp))

(use-package eglot
  ;; Flymake is called by eglot automatically
  ;; No :demand: loading eglot during init calls `char-displayable-p' before the
  ;; frame's fonts are ready, which crashes Emacs 31 (SIGBUS in font_style_to_value)
  ;; about half the time on macOS.
  :hook ((html-mode css-ts-mode js-ts-mode markdown-ts-mode)
	 . eglot-ensure)
  :bind (:prefix "C-c a" :prefix-map eglot-actions
		 ("r" . eglot-rename)
		 ("a" . eglot-code-actions)
		 ("o" . eglot-code-action-organize-imports)
		 ("d" . eldoc)
		 ("f" . eglot-format))
  (:map eglot-mode-map ("H-<mouse-1>" . eglot-code-actions-at-mouse)))

(use-package flymake
  :hook (emacs-lisp-mode . flymake-avoid-scratch)
  :bind (:map flymake-mode-map ("M-n" . flymake-goto-next-error) ("M-p" . flymake-goto-prev-error))
  :config (defun flymake-avoid-scratch () (when (buffer-file-name) (flymake-mode 1))))

(use-package html-mode
  ;; mhtml-mode causes issues with apheleia
  :mode ("\\.html\\'" . html-mode)
  :hook (html-mode . visual-wrap-prefix-mode))

(defun markdown-h1-title ()
  "Insert an atx level 1 heading with the name of the file."
  (interactive)
  (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
(defun markdown-h2-today () "Insert a level 2 heading with today's date in iso format." (interactive) (insert "## " (format-time-string "%Y-%m-%d") "\n"))
(defun markdown-mla-frontmatter () "Insert frontmatter for an MLA heading." (interactive) (insert "---\nprofessor: \nclass: \nword-count: true\n---\n"))
(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :hook ((markdown-ts-mode . variable-pitch-mode)
	 (markdown-ts-mode . markdown-indent-mode)
	 (markdown-ts-mode . obsidian-cli-mode)
	 (markdown-ts-mode . typo-mode))
  :bind ((:map markdown-ts-mode-map ("<tab>" . markdown-ts-demote) ("<backtab>" . markdown-ts-promote))
	 (:prefix "C-c m" :prefix-map markdown-actions ("1" . markdown-h1-title) ("2" . markdown-h2-today) ("f" . markdown-mla-frontmatter)))
  :config ;; Match modus headings
  (dolist (n (number-sequence 1 6))
    (set-face-attribute
     (intern (format "markdown-ts-heading-%d" n))
     nil :inherit
     (intern (format "modus-themes-heading-%d" n)))) ;; Fix extensionless wikilinks
  (advice-add 'markdown-ts--make-link-button :around #'markdown-ts-make-link-button-advice)
  (defun markdown-ts-make-link-button-advice (orig-fn beg end url)
    (funcall orig-fn beg end (if (string-match-p "\\`#\\|\\`[a-z]+:\\|\\.[a-zA-Z]+" url) url (concat url ".md"))))
  ;; https://writewithharper.com/docs/integrations/emacs#Optional-Configuration
  (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(markdown-ts-mode . ("harper-ls" "--stdio")))))

(use-package prog-mode
  ;; does not cover languages that inherit from sgml
  :hook (prog-mode . visual-wrap-prefix-mode))

(use-package agent-shell :hook
  (agent-shell-mode . variable-pitch-mode)
  :bind ("C-c c" . agent-shell-new-temp-shell)
  (:map project-prefix-map ("a" . agent-shell)))

;; Sidestep elfmt's `erase-buffer', avoid touching custom-set-*, and make it compatible with buffer-local `fill-column'
(require 'elfmt)
(require 'apheleia)
(defconst apheleia-elfmt-skip-forms-re (rx bol "(custom-set-" (or "variables" "faces") symbol-end) "Top-level forms elfmt should leave untouched.
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

(use-package csv-mode :hook (csv-mode . csv-align-mode))

(use-package dwim-shell-command
  :bind (("s-i" . dwim-file-mediainfo)
	 ([remap shell-command] . dwim-shell-command)
	 ("C-c p" . dwim-file-to-pdf)
	 (:map dired-mode-map
	       ([remap dired-do-async-shell-command] . dwim-shell-command)
	       ([remap dired-do-shell-command] . dwim-shell-command)
	       ([remap dired-smart-shell-command] . dwim-shell-command)
	       ("e" . dwim-shell-commands-macos-open-with)
	       ("i" . dwim-file-mediainfo)
	       ("x" . dwim-export-to)))
  :config ((with-eval-after-load 'dwim-shell-commands (add-to-list 'dwim-shell-commands-git-clone-dirs "~/Git"))
	   (defun dwim-file-mediainfo () "Run mediainfo on the current buffer's file or marked dired files." (interactive) (dwim-shell-command-on-marked-files "MediaInfo" "mediainfo '<<f>>'" :utils "mediainfo"))
	   (defun dwim-file-to-pdf (&optional mla)
	     "Convert file to PDF via pandoc and typst; with prefix arg, use the MLA template."
	     (interactive "P")
	     (dwim-shell-command-on-marked-files "Converting to pdf" (format "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=%s" (expand-file-name (if mla "mla-template.typ" "resume.typ") "~/.config/typst/"))))))

(use-package elfeed
  :bind (("C-c f" . elfeed)
	 :map elfeed-show-mode-map
	 ("w" . elfeed-webkit-toggle))
  :config (elfeed-org))

(use-package google-translate :bind ("C-c t" . google-translate-smooth-translate) ("C-c T" . google-translate-at-point))

(use-package hackernews :defer t :bind ("C-c h" . hackernews))

(use-package obsidian-cli
  :bind (:prefix "C-c o" :prefix-map obsidian-cli-actions
		 ("s" . obsidian-cli-search-notes)
		 ("d" . obsidian-cli-open-daily-note)
		 ("z" . obsidian-cli-zip-vault)
		 ("b" . obsidian-cli-jump-to-backlink)))

(use-package typst-ts-mode :mode "\\.typ\\'" :config (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst")))

(use-package writegood-mode :bind ("C-c g" . writegood-mode))

(provide 'init)
;;; init.el ends here

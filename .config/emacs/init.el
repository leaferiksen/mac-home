;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-

;; Author: Leaf Eriksen

;;; Commentary:

;; Top level function sort order
;; first by load time, second by alphabet

;; `use-package' :key sort order
;; what to install (:ensure :vc)
;; when to load what (:demand :mode :commands :hook)
;; if to load or not (:if :after)
;; what to keys to bind (:bind :prefix :map)
;; what variables to set (:custom-face :custom)
;; what functions to run when (:init :config)

;;; Code:

;; Internal features and hooks

(require 'package)

(use-package emacs
  :hook
  (emacs-startup . server-start)
  (emacs-startup . remap-all-ts-modes)
  :custom
  (auto-insert-directory "~/.config/emacs/templates/")
  (auto-insert-query nil)
  (auto-save-default nil)
  (backward-delete-char-untabify-method nil)
  (column-number-mode t)
  (completion-auto-help nil)
  (completion-ignore-case t)
  (completions-sort 'historical)
  (cursor-type 'bar)
  (custom-file (make-temp-file "~/.cache/emacs/custom"))
  (delete-selection-mode t)
  (disabled-command-function nil)
  (eldoc-echo-area-use-multiline-p t)
  (electric-pair-mode t)
  (find-file-visit-truename t)
  (gc-cons-threshold 100000000)
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
  (shr-fill-text nil)
  (shr-inhibit-images t)
  (use-dialog-box nil)
  (use-package-vc-prefer-newest t)
  (user-full-name "Leaf Eriksen")
  (user-mail-address "leaferiksen@gmail.com")
  (which-key-mode t)
  (word-wrap-by-category t)
  :config
  (setenv "GIT_EDITOR" "emacsclient")
  (defun remap-all-ts-modes ()
    "Remap all available tree-sitter modes to their standard counterparts."
    (interactive)
    (dolist (ts-mode (apropos-internal "-ts-mode$" #'commandp))
      (when-let ((old-mode (intern-soft (concat (string-remove-suffix "-ts-mode" (symbol-name ts-mode)) "-mode")))
                 ((fboundp old-mode)))
        (add-to-list 'major-mode-remap-alist (cons old-mode ts-mode)))))
  (defun async-shell-command-no-window (command)
    (interactive)
    (let ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))))
      (async-shell-command command)))
  (defun unfill ()
    "Unfill the current region if active, or the current paragraph."
    (interactive)
    (let ((fill-column (point-max)))
      (if (use-region-p)
          (fill-region (region-beginning) (region-end) nil)
        (fill-paragraph nil))))
  (defun vc-git-amend ()
    (interactive)
    (vc-checkin nil 'git)
    (vc-git-log-edit-toggle-amend))
  (add-to-list 'imagemagick-enabled-types 'JXL)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Enable or disable global minor modes
  (auto-insert-mode 1)
  (define-auto-insert "\.html" "insert.html")
  (define-auto-insert "\.js" "insert.js")
  (auto-save-visited-mode 1)
  (context-menu-mode 1)
  (delete-selection-mode 1)
  (editorconfig-mode 1)
  (fido-vertical-mode 1)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)
  (repeat-mode 1))

(use-package term/ns-win
  :if (eq window-system 'ns)
  :bind
  ;; modernize undo and remove s-Z to s-z translation map
  ("s-z" . undo-only)
  ("s-Z" . nil)
  ("s-Z" . undo-redo)
  ("s-w" . kill-current-buffer)
  ("C-M-y" . yank-pop)
  ;; enable standard macOS emoji binding
  ("H-e" . ns-do-show-character-palette)
  ;; remove scroll zoom (highly incompatible with macos native inertia)
  ("C-<wheel-up>" . mwheel-scroll)
  ("C-<wheel-down>" . mwheel-scroll)
  ("C-M-<wheel-up>" . mwheel-scroll)
  ("C-M-<wheel-down>" . mwheel-scroll)
  :custom
  (delete-by-moving-to-trash t)
  (mac-function-modifier 'hyper)
  (mac-option-modifier 'none)
  :config (set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display")
  ;; Transpose unwanted s- bindings to project, bookmark, and treesit navigation
  (define-key key-translation-map (kbd "s-g") (kbd "M-g"))
  (define-key key-translation-map (kbd "s-o") (kbd "C-x p"))
  (define-key key-translation-map (kbd "s-r") (kbd "C-x r"))
  (dolist (key '("a" "b" "d" "e" "f" "k" "l" "n" "p" "t" "u" "y" "<backspace>"))
    (define-key key-translation-map (kbd (concat "s-" key)) (kbd (concat "C-M-" key)))))

(use-package window
  :hook (emacs-startup . almost-maximize-frame)
  :bind
  ;; focus follows splits
  ("C-x 2" . split-and-follow-horizontally)
  ("C-x 3" . split-and-follow-vertically)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 3)
  (frame-resize-pixelwise t)
  :init
  (defun almost-maximize-frame ()
    "Borderless maximise with margins for tiling"
    (interactive)
    (add-to-list 'default-frame-alist '(undecorated-round . t))
    (set-frame-width (selected-frame) (- (display-pixel-width) 85) nil t))
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
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(use-package modus-themes
  :hook (ns-system-appearance-change-functions . auto-theme)
  :custom-face
  (default ((t (:family "Maple Mono NF CN" :height 140))))
  (fixed-pitch ((t (:inherit default))))
  (variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height 180))))
  :custom
  (modus-themes-common-palette-overrides '((underline-link unspecified) (underline-link-visited unspecified) (underline-link-symbolic unspecified)))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  :init
  (defun auto-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t)))))

(use-package completion-preview
  :hook (prog-mode html-mode)
  :bind (:map completion-preview-active-mode ("M-]" . completion-preview-next-candidate) ("M-[" . completion-preview-prev-candidate)))

(use-package dired
  :after ls-lisp
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map ("d" . dired-install-dmg))
  :custom
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-omit-verbose nil)
  (dired-recursive-copies 'always)
  (dired-omit-files "\\`[.][.]?\\'\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
  :config
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
      (message "Installation failed: could not mount DMG or find .app bundle"))))

(use-package eglot
  :demand
  :hook
  (html-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :bind
  (:prefix "C-c e" :prefix-map eglot-actions ("r" . eglot-rename) ("a" . eglot-code-actions) ("o" . eglot-code-action-organize-imports) ("d" . eldoc) ("f" . eglot-format))
  (:map eglot-mode-map ("H-<mouse-1>" . eglot-code-actions-at-mouse))
  :custom
  (eglot-code-action-indicator "*")
  (eglot-code-action-indications '(mode-line))
  (eglot-autoshutdown t))

(use-package flymake
  :hook
  (eglot-managed-mode-hook)
  (emacs-lisp-mode . flymake-avoid-scratch)
  :bind (:map flymake-mode-map ("M-n" . flymake-goto-next-error) ("M-p" . flymake-goto-prev-error))
  :config
  (defun flymake-avoid-scratch ()
    (when (buffer-file-name)
      (flymake-mode 1))))

(use-package html-mode
  ;; mhtml-mode causes issues with apheleia
  :mode ("\\.html\\'" . html-mode))

(use-package ls-lisp
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-localized-time-format t))

(use-package open-init
  :bind ([remap customize] . open-init)
  :init
  (defun open-init ()
    (interactive)
    (find-file "~/.config/emacs/init.el")))

(use-package project
  :bind (:map project-prefix-map ("s" . project-gterm) ("S" . project-npx-serve) ("t" . project-tailwindcss))
  :custom
  (project-mode-line t)
  (project-vc-extra-root-markers '("project"))
  :config
  (defun project-gterm ()
    "Open gterm in project's root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (gterm)))
  (defun project-run (label msg &rest args)
    "Run ARGS as a process LABEL in project root, showing MSG."
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (buf (format "*%s:%s*" label (project-name project))))
      (when (get-buffer buf)
        (kill-buffer buf))
      (apply #'start-process label buf args)
      (when msg
        (message msg (project-name project)))))
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
    (if-let ((current-clip (gui-get-selection 'CLIPBOARD 'STRING))
             ((not (string-empty-p current-clip))))
        (progn
          (split-and-follow-horizontally)
          (xwidget-webkit-browse-url current-clip)
          (message "Clipboard update detected! Opened %s in Xwidgets" current-clip))
      (run-at-time "0.5 sec" nil #'watch-clipboard-xwidget-webkit-browse-url))))

(use-package visual-wrap-prefix-mode
  :hook (prog-mode html-mode))

(use-package xwidget
  :bind (:map xwidget-webkit-mode-map ("u" . xwidget-webkit-browse-url)))

(use-package yt-dlp
  :bind ("C-c y" . yt-dlp-download)
  :init
  (defun yt-dlp-download ()
    (interactive)
    (let* ((v (y-or-n-p "Video? "))
           (s (and v (y-or-n-p "Subs? ")))
           (u (read-string "URL: "))
           (f (or (and s "--write-subs") (and v "") "-x")))
      (unless (string-empty-p u)
        (async-shell-command (format "yt-dlp %s %s" f (shell-quote-argument u)))))))

;;; External packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package agent-shell
  :ensure t
  :hook (agent-shell-mode . completion-preview-mode)
  :bind ("C-c c" . agent-shell-new-temp-shell)
  :custom (agent-shell-preferred-agent-config '(auto . opencode))
  :init
  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "a") #'agent-shell)))

(use-package anglish
  :ensure t
  :vc (:url "git@github.com:leaferiksen/anglish.el.git"))

(use-package apheleia
  :ensure t
  :hook (emacs-lisp-mode . (lambda () (apheleia-mode -1)))
  :config (apheleia-global-mode +1))

(use-package clojure-mode
  :ensure t)

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-align-padding 2)
  (csv-align-max-width 72))

(use-package devil
  :ensure t
  :demand t
  ;; https://www.reddit.com/r/emacs/comments/1jgnw8g/devil_mode_and_whichkey
  :vc (:url "https://github.com/fbrosda/devil" :branch "dev" :rev :newest)
  :custom
  (devil-exit-key ".")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e") ("%k m n" "%k m p") ("%k m b" "%k m f" "%k m a" "%k m e") ("%k m m f" "%k m m b" "%k m m a" "%k m m e" "%k m m n" "%k m m p" "%k m m u" "%k m m d")))
  :bind ([remap describe-key] . devil-describe-key)
  :config (global-devil-mode))

(use-package dwim-shell-command
  :ensure t
  :demand
  :bind
  ("s-i" . dwim-file-mediainfo)
  ([remap shell-command] . dwim-shell-command)
  (:prefix "C-c p" :prefix-map dwim-print ("m" . dwim-file-to-mla-pdf) ("r" . dwim-file-to-resume-pdf) ("p" . dwim-md-to-pptx))
  (:map dired-mode-map ([remap dired-do-async-shell-command] . dwim-shell-command) ([remap dired-do-shell-command] . dwim-shell-command) ([remap dired-smart-shell-command] . dwim-shell-command) ("e" . dwim-shell-commands-macos-open-with) ("i" . dwim-file-mediainfo) ("x" . dwim-export-to))
  :config
  (with-eval-after-load 'dwim-shell-commands
    (add-to-list 'dwim-shell-commands-git-clone-dirs "~/Git"))
  (defun dwim-file-mediainfo ()
    "Run mediainfo on the current buffer's file or marked dired files."
    (interactive)
    (dwim-shell-command-on-marked-files "MediaInfo" "mediainfo '<<f>>'" :utils "mediainfo"))
  (defun dwim-file-to-resume-pdf ()
    "Convert file to generic pdf via pandoc."
    (interactive)
    (dwim-shell-command-on-marked-files "Converting to generic pdf" "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=/Users/leaf/.config/typst/resume.typ"))
  (defun dwim-file-to-mla-pdf ()
    "Convert file to MLA pdf via pandoc and typst."
    ;; fonttools varLib.mutator '/Users/leaf/Library/Fonts/AtkinsonHyperlegibleNext[wght].ttf' wght=400
    ;; pandoc --print-default-template=typst
    (interactive)
    (dwim-shell-command-on-marked-files "Converting to MLA pdf" "pandoc '<<f>>' -o '<<fne>>.pdf' --pdf-engine=typst --template=/Users/leaf/.config/typst/mla-template.typ"))
  (defun dwim-md-to-pptx ()
    "Convert md files to pptx."
    (interactive)
    (if-let ((files (dwim-shell-command--files))
             ((seq-every-p (apply-partially #'string-suffix-p ".md") files)))
        (dwim-shell-command-on-marked-files "Converting md to pptx" "npx @marp-team/marp-cli@latest '<<f>>' --pptx")
      (user-error "Selection contains non-markdown files!"))))

(use-package elfeed
  :ensure t
  :bind ("C-c f" . elfeed)
  :init (run-at-time nil "8 hours" #'elfeed-update))

(use-package elfeed-org
  :ensure t
  :init (elfeed-org))

(use-package elfeed-webkit
  :ensure t
  :commands (elfeed-webkit-enable)
  :hook (elfeed-show-mode . elfeed-webkit-enable)
  :bind (:map elfeed-show-mode-map ("w" . elfeed-webkit-toggle)))

(use-package elisp-autofmt
  :ensure t
  :vc (:url "https://codeberg.org/ideasman42/emacs-elisp-autofmt")
  :demand
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :bind (:prefix "C-c e" :prefix-map elisp-autofmt ("b" . elisp-autofmt-buffer) ("r" . elisp-autofmt-region-dwim)))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(ns x))
  :config
  (exec-path-from-shell-initialize)
  (setenv "CC" nil))

(use-package ghostel
  :ensure t
  :bind ("C-c s" . ghostel))

(use-package google-translate
  :ensure t
  :bind
  ("C-c t" . google-translate-smooth-translate)
  ("C-c T" . google-translate-at-point)
  :init
  (setopt
   google-translate-output-destination '(echo-area)
   google-translate-show-phonetic t
   google-translate-translation-directions-alist '(("ja" . "en") ("en" . "ja"))))

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-word-at-point))

(use-package lorem-ipsum
  :ensure t)

(use-package markdown-indent-mode
  :ensure t
  :hook (md-ts-mode))

(use-package md-ts-mode
  :ensure t
  :mode ("\\.md\\'" . md-ts-mode)
  :hook (md-ts-mode . eglot-ensure)
  :bind
  (:map md-ts-mode-map ("s-<return>" . markdown-follow-any-link))
  (:prefix "C-c m" :prefix-map markdown-actions ("1" . markdown-h1-title) ("2" . markdown-h2-today) ("m" . markdown-more-emphasis) ("l" . markdown-less-emphasis))
  :custom
  ;; https://writewithharper.com/docs/integrations/emacs#Optional-Configuration
  (eglot-workspace-configuration '(:harper-ls (:dialect "American" :linters (:LongSentences :json-false :AvoidCurses :json-false))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(md-ts-mode . ("harper-ls" "--stdio")))
    (add-hook
     'eglot-managed-mode-hook
     (lambda ()
       (when anglish-mode
         (add-hook 'flymake-diagnostic-functions #'anglish--check-buffer nil t)
         (flymake-start)))))
  (defun markdown-h1-title ()
    "Insert an atx level 1 heading with the name of the file."
    (interactive)
    (insert "# " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"))
  (defun markdown-h2-today ()
    "Insert an atx level 2 heading with today's date in iso format."
    (interactive)
    (insert "## " (format-time-string "%Y-%m-%d") "\n"))
  (defun markdown-follow-any-link ()
    (interactive)
    (cond
     ((thing-at-point-looking-at "\\[\\[\\([^]]+\\)\\]\\]")
      (when-let ((path (match-string 1)))
        (find-file
         (if (file-name-extension path)
             path
           (concat path ".md")))))
     ((thing-at-point-looking-at "\\[\\([^]]+\\)\\](\\([^)]+\\))")
      (browse-url (match-string 2)))
     (t
      (message "No link found at point."))))
  (defun markdown--bounds ()
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (bounds-of-thing-at-point 'word)))
  (defun markdown-more-emphasis ()
    (interactive)
    (when-let* ((bounds (markdown--bounds))
                (beg (car bounds))
                (end (cdr bounds)))
      (save-excursion
        (goto-char end)
        (insert "*")
        (goto-char beg)
        (insert "*"))))
  (defun markdown-less-emphasis ()
    (interactive)
    (when-let* ((bounds (markdown--bounds))
                (beg (car bounds))
                (end (cdr bounds)))
      (save-excursion
        (when (and (equal "*" (buffer-substring-no-properties (- beg 1) beg)) (equal "*" (buffer-substring-no-properties end (+ end 1))))
          (delete-region end (+ end 1))
          (delete-region (- beg 1) beg))))))

(use-package mines
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook dired-mode)

(use-package obsidian-cli
  :ensure t
  :vc (:url "git@github.com:leaferiksen/obsidian-cli.el.git")
  :hook (markdown-ts-mode md-ts-mode)
  :bind
  ("C-c o" . obsidian-cli-open-note)
  ("C-c j" . obsidian-cli-open-daily-note)
  (:map obsidian-cli-mode-map ("C-c C-b" . obsidian-cli-jump-to-backlink))
  :custom
  (obsidian-cli-note-extensions '("md" "tsv"))
  (obsidian-cli-rename-on-save t))

(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode))

(use-package swift-mode
  :ensure t
  :if (memq window-system '(ns))
  :mode "\\.swift\\'"
  :hook (swift-mode . eglot-ensure)
  :bind (:prefix "C-c x" :prefix-map xcode ("b" . xcode-build) ("r" . xcode-run) ("t" . xcode-test))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(swift-ts-mode . ("xcrun" "sourcekit-lsp"))))
  ;; https://danielde.dev/blog/emacs-for-swift-development
  (defun xcode-build ()
    "Build the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript
     "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        build targetProject
      end if
    end tell"))
  (defun xcode-run ()
    "Stop and run the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript
     "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        stop targetProject
        run targetProject
      end if
    end tell"))
  (defun xcode-test ()
    "Stop and test the active Xcode workspace cleanly via native API."
    (interactive)
    (ns-do-applescript
     "tell application \"Xcode\"
      if (count of workspace documents) > 0 then
        set targetProject to active workspace document
        stop targetProject
        test targetProject
      end if
    end tell")))

(use-package typo
  :ensure t
  :hook text-mode)

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode")
  :mode "\\.typ\\'"
  :config (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst")))

(use-package visual-fill-column
  :ensure t
  :hook
  (md-ts-mode org-mode)
  (visual-fill-column-mode . (lambda () (face-remap-add-relative 'default :height 180)))
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 90))

(use-package writegood-mode
  :ensure t
  :vc (:url "https://github.com/bnbeckwith/writegood-mode")
  :bind ("C-c g" . writegood-mode))

(provide 'init)
;;; init.el ends here

;; Local variables:
;; fill-column: 1000
;; no-byte-compile: t
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; end:

;;; init.el --- Disabled sections of Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
;;;;;;;;;;;;;;;;
;; Early-init ;;
;;;;;;;;;;;;;;;;
(emacs-startup . server-start)

(setq package-vc-allow-build-commands t)
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize with no frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(define-key key-translation-map (kbd "C-˙") (kbd "M-DEL"))
(initial-buffer-choice "~/Documents/")
(trash-directory "~/.Trash")
(add-to-list 'exec-path "/Users/leaf/.docker/bin")
(add-to-list 'exec-path "/opt/homebrew/opt/python@3.14/libexec/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(set-face-attribute 'hl-line nil :background "controlAccentColor")
(set-face-attribute 'hl-line nil :background "controlAccentColor")

(defun get-apw-password (domain)
  "Fetch the password for DOMAIN using the apw tool."
  ;; Usage: (get-apw-password "example.com")
  (let* ((json-str (shell-command-to-string (format "apw pw get %s" domain)))
         (data (json-parse-string json-str :object-type 'alist))
         (results (alist-get 'results data)))
    (when (> (length results) 0)
      (alist-get 'password (elt results 0)))))

:bind
( :map dired-mode-map
  ("f" . dired-finder-path))
:config
(defun dired-finder-path ()
  "Open Dired in the frontmost Finder window path, if available."
  (interactive)
  (let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
    (if path (dired (string-trim path))
      (message "No Finder window found."))))

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

(use-package elfeed-webkit
  :ensure t
  ;; :demand
  :config
  (elfeed-webkit-enable)
  :bind
  ( :map elfeed-show-mode-map
    ("%" . elfeed-webkit-toggle)))

(use-package music-control
  :defer t
  :load-path "elpa/music-control/"
  :config
  (music-control-mode 1))

(use-package sidetabs
  :vc (:url "https://gist.github.com/rougier/23f723b039873cd5c2e9eb6862dbc31e"
	    :rev :newest)
  :config
  (sidetabs-mode 1))

(use-package treesit
  :config
  (defun treesit-bulk-install ()
    "Install everything currently in treesit-language-source-alist."
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
  ;; Source: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  ;; bash/html/toml/yaml/md-ts-modes all handle their own sources
  (setq treesit-language-source-alist
	'((cmake "https://github.com/uyha/tree-sitter-cmake")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))

(use-package treesit-langs
  :ensure t
  :vc ( :url "https://github.com/kiennq/treesit-langs"))

(use-package markdown-ts-mode
  :mode
  ("\\.md\\'" . markdown-ts-mode)
  :hook
  (markdown-mode . (lambda () (font-lock-add-keywords nil '(("\\[\\[\\([^]]+\\)\\]\\]" 0 'link t)))))
  (markdown-mode . orgtbl-mode)
  :config
  (require 'org-table)
  (defun markdown-table-fix (&rest _args)
    (when (and (buffer-file-name) (string-match-p "\\.md$" (buffer-file-name)))
      (save-excursion
	(let ((end (org-table-end)))
          (goto-char (org-table-begin))
          (while (search-forward "+" end t)
            (replace-match "|"))))))
  (advice-add 'org-table-align :after #'markdown-table-fix))

(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :hook
  ;; (markdown-mode . variable-pitch-mode)
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
  :bind
  ("C-c d" . daily-note)
  ( :map markdown-mode-map
    ("C-c SPC 1" . h1-title)
    ("C-c SPC 2" . h2-today)
    ("C-c SPC p" . export-selection-to-mla-pdf)))

(use-package swift-development
  :ensure t :defer t
  :vc
  ( :url "https://github.com/konrad1977/swift-development")
  :config
  ;; (require 'swift-lsp)
  ;; (add-to-list 'eglot-server-programs '(swift-ts-mode . swift-lsp-eglot-server-contact))
  ;; Load the main package
  (require 'swift-development)
  (require 'xcode-project)
  (require 'xcode-build-config)
  ;; Optional modules
  ;; (require 'ios-simulator)
  ;; (require 'ios-device)
  ;; (require 'swift-refactor)
  ;; (require 'localizeable-mode)
  )

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

(use-package vterm
  :bind
  (:map vterm-mode-map
	("C-q" . vterm-send-next-key)))
(use-package epg-config
  :custom (epg-pinentry-mode 'loopback))
(use-package auth-source
  :custom (auth-sources "~/.authinfo.gpg"))
(setopt modus-themes-common-palette-overrides '((fringe unspecified) (bg-tab-bar bg-main) (bg-tab-current bg-active) (bg-tab-other bg-dim) (bg-line-number-inactive unspecified) (bg-line-number-active unspecified) (border-mode-line-active unspecified) (border-mode-line-inactive unspecified)))
(use-package mastodon
  :ensure t :defer t
  :hook ((mastodon-mode . visual-fill-column-mode)
	 (mastodon-toot-mode . visual-fill-column-mode))
  :custom ((mastodon-instance-url "https://mastodon.social")
	   (mastodon-active-user "leaferiksen")
	   (mastodon-auth-use-auth-source nil)
	   ;; (mastodon-tl--display-media-p nil)
	   (mastodon-tl--highlight-current-toot t)))
(use-package devil
  :config
  (global-devil-mode)
  (add-to-list 'devil-repeatable-keys `("%k v"))
  (add-to-list 'devil-repeatable-keys `("%k m v"))
  (add-to-list 'devil-repeatable-keys `("%k m d"))
  (add-to-list 'devil-repeatable-keys `("%k m m p" "%k m m n" "%k m m b" "%k m m f" "%k m m a" "%k m m e" "%k m m u" "%k m m d" "%k m m t")))
(use-package devil
  :ensure t :vc (:url "https://github.com/susam/devil")
  :config
  (global-devil-mode)
  (define-key devil-mode-map (kbd ".") #'devil)
  (add-to-list 'devil-special-keys `(". ." . ,(devil-key-executor ".")))
  (add-to-list 'devil-special-keys `(". SPC" . ,(devil-key-executor ". SPC")))
  (add-to-list 'devil-special-keys `(". RET" . ,(devil-key-executor ". RET")))
  (add-to-list 'devil-special-keys `(". <return>" . ,(devil-key-executor ". <return>")))
  :custom ((devil-translations '((", z" . "C-") (". z" . "M-") (", ," . ",") (". ." . ".") ("," . "C-") ("." . "M-")))
	   (devil-repeatable-keys '(("%k d") ("%k k") (". ^") (", v") (". v") (", x o")
				    (". b" ". f" ". a" ". e") (", p" ", n" ", b" ", f" ", a" ", e")
				    (", . p" ", . n" ", . b" ", . f" ", . a" ", . e" ", . u" ", . d" ", . t")
				    (". , p" ". , n" ". , b" ". , f" ". , a" ". , e" ". , u" ". , d" ". , t")))))
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width t))
(use-package tab-line
  :custom ((global-tab-line-mode t)
	   (tab-line-new-button-show nil)
	   (tab-line-close-button-show nil))
  :custom-face
  (tab-bar ((t (:inherit mode-line))))
  :bind (("C-<tab>" . tab-line-switch-to-next-tab)
	 ("C-M-<tab>" . tab-line-switch-to-prev-tab)))
(use-package files
  :custom (insert-directory-program "gls"))
(defun arc-open-parent-folder-and-quit () "Open the parent folder of the current arc-mode buffer and quit the arc-mode window."
       (interactive)
       (let ((parent-dir (expand-file-name default-directory)))
	 (quit-window)
	 (dired parent-dir)))
(use-package fixed-pitch
  :custom (fixed-pitch-dont-change-cursor t)
  :hook ((archive-mode diff-mode elfeed-search-mode html-mode prog-mode vc-dir-mode) . fixed-pitch-mode))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
		    (font-spec :family "Hiragino Mincho ProN")))
;;;;;;;;;;;;;;;
;; Mode-line ;;
;;;;;;;;;;;;;;;
(mode-line ((t (:inherit 'variable-pitch))))
(defvar my/font "New York"
  "Main font")
(defvar my/font-ja "Hiragino Mincho ProN"
  "Japanese font")
(defun my/use-font (&optional frame)
  (when frame
    (select-frame frame))
  (set-face-attribute 'variable-pitch nil :font my/font)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family my/font-ja))))
(my/use-font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal Interface Emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide the menu bar when it can't integrate with the global bar
(when (not (display-graphic-p))
  (menu-bar-mode -1))
(setq frame-background-mode 'dark)
;; (debug-on-variable-change 'frame-background-mode)
(mapc 'frame-set-background-mode (frame-list))
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))
;; Equivalent bindings
("C-u" . previous-line) ("C-e" . next-line) ("C-n" . backward-char) ;; ("C-i" . forward-char)
("C-p" . nil) ("C-b" . nil) ("C-f" . nil) ;; ("C-n" . nil)
("C-l" . beginning-of-visual-line) ("C-y" . end-of-visual-line)
("C-a" . nil) ; ("C-e" . nil)
("C-j" . scroll-up-command) ;; ("C-m" . scroll-down-command)
("C-v" . nil) ("M-v" . nil)
("M-n" . backward-word) ("M-i" . forward-word)
("M-b" . nil) ("M-f" . nil)
("C-'" . delete-forward-char) ("C-o" . delete-backward-char) ("M-'" . kill-word) ("M-o" . backward-kill-word)
(massmapper :url "https://github.com/meedstrom/massmapper")
(use-package massmapper
  :config (massmapper-conserve-ret-and-tab)
  :custom (massmapper-Cm-Ci-override '(("C-i" . forward-char)
				       ("C-m" . scroll-down-command))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(split-height-threshold 0)
(split-width-threshold nil)
(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 1465 845 t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "M-<home>" 'completion-preview-prev-candidate)
(keymap-global-set "M-<end>" 'completion-preview-next-candidate)
("C-p" . nil) ("C-n" . nil) ("C-f" . nil) ("M-f" . nil) ("C-b" . nil) ("M-b" . nil) ("C-M-p" . nil) ("C-M-n" . nil) ("C-M-f" . nil) ("C-M-b" . nil) ("C-d" . nil) ("M-d" . nil) ("C-w" . nil) ("M-w" . nil) ("C-v" . nil) ("M-v" . nil) ("C-M-v" . nil) ("C-M-S-v" . nil)
([escape] . keyboard-quit) (:map esc-map ([escape] . keyboard-quit)) (:map ctl-x-map ([escape] . keyboard-quit)) (:map help-map ([escape] . keyboard-quit)) (:map goto-map ([escape] . keyboard-quit)) (:map minibuffer-mode-map ([escape] . minibuffer-keyboard-quit)) (:map devil-mode-map ([escape] . keyboard-quit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-<up>" 'beginning-of-buffer)
(keymap-global-set "C-<down>" 'end-of-buffer)
(keymap-global-set "C-<left>" 'move-beginning-of-line)
(keymap-global-set "C-<right>" 'move-end-of-line)
;; macOS keybinds
(keymap-global-set "C-z" 'undo-fu-only-undo)
(keymap-global-set "C-S-z" 'undo-fu-only-redo)
(keymap-global-set "C-," 'customize)
(keymap-global-set "C-w" 'kill-current-buffer)
(keymap-global-set "C-q" 'kill-emacs)
(keymap-global-set "C-o" 'find-file)
(keymap-global-set "C-a" 'mark-whole-buffer)
(keymap-global-set "C-s" 'save-buffer)
(keymap-global-set "C-S-s" 'write-file)
(keymap-global-set "C-f" 'isearch-forward)
(keymap-set isearch-mode-map "C-f" 'isearch-forward)
(keymap-global-set "C-S-f" 'isearch-backward)
(keymap-global-set "C-M-f" 'isearch-forward-regexp)
(keymap-global-set "C-M-S-f" 'isearch-backward-regexp)
(keymap-global-set "C-<up>" 'completion-preview-prev-candidate)
(keymap-global-set "C-<down>" 'completion-preview-next-candidate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swift-mode
  :ensure t :defer t
  :interpreter "swift"
  :hook (swift-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp"))))
(use-package go-translate
  :custom
  (gt-langs
   '(en jp))
  (gt-default-translator
   (gt-translator :engines
		  (gt-google-engine))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation and Selection mode
;; https://github.com/mrkkrp/modalka
(use-package modalka
  :init
  (setq-default
   modalka-cursor-type 'box)
  (modalka-global-mode 1)
  (add-to-list 'modalka-excluded-modes 'dired-mode)
  (add-to-list 'modalka-excluded-modes 'vc-git-log-edit-mode)
  (add-to-list 'modalka-excluded-modes 'term-mode)
  (add-to-list 'modalka-excluded-modes 'eshell-mode)
  (add-to-list 'modalka-excluded-modes 'elfeed-show-mode)
  :config
  (define-key modalka-mode-map (kbd "SPC") 'set-mark-command)
  ;; Symbols
  (modalka-define-kbd "`" "nil")
  (modalka-define-kbd "~" "nil")
  (modalka-define-kbd "!" "M-!")
  (modalka-define-kbd "@" "nil")
  (modalka-define-kbd "#" "nil")
  (modalka-define-kbd "%" "M-%")
  (modalka-define-kbd "$" "M-$")
  (modalka-define-kbd "^" "nil")
  (modalka-define-kbd "&" "M-&")
  (modalka-define-kbd "*" "nil")
  (modalka-define-kbd "(" "nil")
  (modalka-define-kbd ")" "nil")
  (modalka-define-kbd "-" "nil")
  (modalka-define-kbd "_" "nil")
  (modalka-define-kbd "=" "nil")
  (modalka-define-kbd "+" "nil")
  (modalka-define-kbd ";" "M-;")
  (modalka-define-kbd ":" "M-:")
  (modalka-define-kbd "[" "nil")
  (modalka-define-kbd "]" "nil")
  (modalka-define-kbd "{" "nil")
  (modalka-define-kbd "}" "nil")
  (modalka-define-kbd "\\" "nil")
  (modalka-define-kbd "|" "M-|")
  (modalka-define-kbd "," "nil")
  (modalka-define-kbd "." "nil")
  (modalka-define-kbd "/" "nil")
  (modalka-define-kbd "?" "nil")
  (modalka-define-kbd "<" "nil")
  (modalka-define-kbd ">" "nil")
  ;; Numbers
  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")
  ;; Letters
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (define-key modalka-mode-map "c"
	      `(lambda () "Simulates the `C-c' key-press" (interactive)
		 (setq prefix-arg current-prefix-arg)
		 (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c"))))) ; C-c prefix
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (define-key modalka-mode-map "g" goto-map)
  (define-key modalka-mode-map "h" help-map)
  (modalka-define-kbd "i" "C-i")
  (modalka-define-kbd "j" "C-j")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "l" "C-l")
  (modalka-define-kbd "m" "C-m")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "o" "C-o")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "q" "C-q")
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "t" "C-t")
  (modalka-define-kbd "u" "C-u")
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "w" "C-w")
  (define-key modalka-mode-map "x" ctl-x-map)
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "M-z")
  (modalka-define-kbd "A" "M-a")
  (modalka-define-kbd "B" "C-M-b")
  (modalka-define-kbd "C" "M-c")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "E" "M-e")
  (modalka-define-kbd "F" "C-M-f")
  (define-key modalka-mode-map "G" goto-map)
  (modalka-define-kbd "H" "M-h")
  (modalka-define-kbd "I" "M-i")
  (modalka-define-kbd "J" "M-j")
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "L" "M-l")
  (modalka-define-kbd "M" "M-m")
  (modalka-define-kbd "N" "C-M-n")
  (modalka-define-kbd "O" "M-o")
  (modalka-define-kbd "P" "C-M-p")
  (modalka-define-kbd "Q" "M-q")
  (modalka-define-kbd "R" "M-r")
  (modalka-define-kbd "S" "M-s")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "U" "M-u")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "W" "M-w")
  (define-key modalka-mode-map "X" 'execute-extended-command)
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "Z" "M-z")
  :bind
  (("<f13>" . modalka-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disabled.el ends here

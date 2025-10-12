;;; init.el --- Disabled sections of Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
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
'(package-vc-selected-packages
  '((copilot :vc-backend Git :url "https://github.com/copilot-emacs/copilot.el")))
'(package-selected-packages
  '(copilot))
(use-package copilot
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mastodon
  :bind (:map mastodon-mode-map (("u" . 'mastodon-tl-goto-prev-item)
								 ("e" . 'mastodon-tl-goto-next-item)
								 ("n" . nil)
								 ("p" . nil)))
  :hook ((mastodon-mode . visual-fill-column-mode)
		 (mastodon-toot-mode . visual-fill-column-mode))
  :custom ((mastodon-instance-url "https://mastodon.social")
		   (mastodon-active-user "leaferiksen")
		   (mastodon-auth-use-auth-source nil)
		   (mastodon-tl--display-media-p nil)
		   (mastodon-tl--highlight-current-toot t)
		   (mastodon-auth-use-auth-source t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-table
  :config (advice-add 'org-table-align :after 'markdown-org-table-align-advice))
(defun markdown-org-table-align-advice ()
  "Replace \"+\" sign with \"|\" in tables."
  (when (member major-mode '(markdown-mode gfm-mode))
    (save-excursion
	  (save-restriction
        (narrow-to-region (org-table-begin) (org-table-end))
        (goto-char (point-min))
        (while (search-forward "-+-" nil t)
		  (replace-match "-|-"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; (add-hook 'after-init-hook #'global-prettier-mode)
(use-package prettier
  :hook after-init
  :bind
  ("s-p" . 'prettier-prettify)
  ("s-S-p" . 'prettier-prettify-region))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swift
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
			   '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :hook (swift-mode) . 'eglot-ensure)
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/licht1stein/obsidian.el
(use-package obsidian
  :hook markdown-mode
  :config
  (global-obsidian-mode t)
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind
  (:map obsidian-mode-map
		("s-<return>" . obsidian-follow-link-at-point)
		("s-S-<return>" . obsidian-backlink-jump)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab bar
(use-package tab-line
  :bind
  ("M-S-<tab>" . tab-line-switch-to-prev-tab)
  ("M-<tab>" . tab-line-switch-to-next-tab))
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
(require '~/.emacs.d/elpa/terminal-here/terminal-here.el)
(setq terminal-here-mac-terminal-command '("open" "-n" "-a" "Ghostty" "--args" "--working-directory="))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/lorniu/go-translate
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

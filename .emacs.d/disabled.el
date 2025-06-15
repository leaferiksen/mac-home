;;; init.el --- Disabled sections of Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(global-set-key (kbd "C-q") 'kill-emacs)
(global-set-key (kbd "C-c q") 'quoted-insert)
(global-set-key (kbd "C-,") 'customize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab bar
(use-package tab-line
  :bind
  ("M-S-<tab>" . tab-line-switch-to-prev-tab)
  ("M-<tab>" . tab-line-switch-to-next-tab))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://codeberg.org/martianh/mastodon.el
(use-package mastodon
  :custom
  (mastodon-instance-url "https://mastodon.social")
  (mastodon-active-user "leaferiksen"))
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

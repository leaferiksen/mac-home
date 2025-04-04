;;; init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Initialization file for Emacs
;;; Code:

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;;Transform yes-or-no questions into y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)
(run-at-time 5 600 'elfeed-update)
(use-package dired
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . nerd-icons-dired-mode)
  :bind (:map dired-mode-map
			  ("<mouse-2>" . dired-mouse-find-file)))
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :hook
  (elfeed-show-mode . variable-pitch-mode)
  (elfeed-show-mode . visual-fill-column-mode)
  (elfeed-show-mode . (lambda ()
						(setq-local line-spacing 12)
						(setq-local fill-column 90)
						(setq-local shr-width 85)))
  :bind
  ("C-c e" . elfeed)
  (:map elfeed-show-mode-map
		("<mouse-1>" . elfeed-show-next)
		("<mouse-3>" . elfeed-show-prev)))
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  '(lsp-completion-provider :none)
  '(lsp-copilot-enabled t)
  '(lsp-enable-snippet nil)
  :hook
  (html-ts-mode . lsp)
  (css-ts-mode . lsp)
  (js-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (tsx-ts-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)
;; https://github.com/merrickluo/lsp-tailwindcss
(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-skip-config-check t)
  (setq lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server")
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save))
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown
  :config
  (markdown-header-scaling t)
  :hook
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . (lambda ()
					 (setq-local fill-column 90)
					 (setq-local line-spacing 12))))
;; https://github.com/licht1stein/obsidian.el
(use-package obsidian
  :hook markdown-mode
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  (markdown-enable-wiki-links t)
  :bind
  ("C-c l" . obsidian-insert-wikilink)
  ("C-c o" . obsidian-follow-link-at-point)
  ("C-c p" . obsidian-jump)
  ("C-c b" . obsidian-backlink-jump))
;; Fix the trackpad
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
;; Mac standards
(global-set-key (kbd "s-w") (kbd "s-k"))
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
;; Make Escape actually escape
(define-key esc-map [escape] 'keyboard-quit)
(define-key ctl-x-map [escape] 'keyboard-quit)
(define-key help-map [escape] 'keyboard-quit)
(define-key goto-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; Navigation and Selection mode with https://github.com/mrkkrp/modalka
(use-package modalka
  :init
  (setq-default
   modalka-cursor-type 'box)
  (modalka-global-mode 1)
  (add-to-list 'modalka-excluded-modes 'elfeed-search-mode)
  (add-to-list 'modalka-excluded-modes 'elfeed-show-mode)
  (add-to-list 'modalka-excluded-modes 'vc-git-log-edit-mode)
  :config
  (define-key modalka-mode-map "`" 'execute-extended-command)
  (define-key modalka-mode-map (kbd "SPC") 'set-mark-command)
  (modalka-define-kbd ":" "M-;")
  (modalka-define-kbd ";" "C-;")
  (modalka-define-kbd "&" "M-&")
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
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (define-key modalka-mode-map "c" mode-specific-map)
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
  (modalka-define-kbd "q" "M-q")
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
  ;; G (bound to g)
  (modalka-define-kbd "H" "M-h")
  (modalka-define-kbd "I" "M-i")
  (modalka-define-kbd "J" "M-j")
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "L" "M-l")
  (modalka-define-kbd "M" "M-m")
  (modalka-define-kbd "N" "C-M-n")
  (modalka-define-kbd "O" "M-o")
  (modalka-define-kbd "P" "C-M-p")
  ;; Q (bound to q)
  (modalka-define-kbd "R" "M-r")
  (modalka-define-kbd "S" "M-s")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "U" "M-u")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "W" "M-w")
  ;; X (bound to `)
  (modalka-define-kbd "Y" "M-y")
  ;; Z (bound to z)
  :bind
  (("<f13>" . modalka-mode)))
;; Various functions
(defun wrap-urls-with-parentheses (start end)
  "Wrap quoted URLs with parentheses from START to END."
  (interactive "r")
  (save-excursion
	(goto-char start)
	(while
		(re-search-forward "\"\\(https?://[^\"]+\\)\"" end t)
	  (replace-match "(\"\\1\")"))))
;; GUI Settings ⌘,
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(backward-delete-char-untabify-method nil)
 '(cursor-type 'bar)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first -go")
 '(dired-omit-files
   "\\`[.]?#\\|\\.DS_Store\\|\\`\\._\\|\\.CFUserTextEncoding\\|\\.Trash")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '(("https://xkcd.com/rss.xml" comics) ("https://www.smbc-comics.com/comic/rss" comics) ("https://www.questionablecontent.net/QCRSS.xml" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://aftermath.site/feed" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://www.gameinformer.com/rss.xml" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://redstrate.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://cdn.jwz.org/blog/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://www.girlonthenet.com/feed/" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://lwlies.com/feed/" journals) ("https://howtodothingswithmemes.substack.com/feed" journals) ("https://basicappleguy.com/basicappleblog?format=rss" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
 '(elfeed-search-filter "@1-month-ago +unread")
 '(fill-column 9999)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-flycheck-mode t)
 '(global-prettify-symbols-mode t)
 '(global-visual-line-mode t)
 '(initial-buffer-choice t)
 '(make-backup-files nil)
 '(minions-mode t)
 '(minions-prominent-modes '(flymake-mode lsp-mode))
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(aggressive-indent elfeed elfeed-protocol flexoki-themes lsp-mode lsp-tailwindcss minions modalka nerd-icons-dired obsidian treesit-auto undo-fu visual-fill-column))
 '(package-vc-selected-packages
   '((lsp-tailwindcss :url "https://github.com/merrickluo/lsp-tailwindcss" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(flymake-mode display-line-numbers-mode completion-preview-mode aggressive-indent-mode))
 '(project-mode-line t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(tab-width 4)
 '(trash-directory "~/.Trash")
 '(use-package-always-ensure t)
 '(visual-fill-column-center-text t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :foundry "nil" :slant normal :weight regular :height 200 :width normal)))))
;; Install selected packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install-selected-packages)
(package-autoremove)
;;; early-init.el ends here

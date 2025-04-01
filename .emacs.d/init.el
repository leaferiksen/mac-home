;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Fix the trackpad
(global-set-key
 (kbd "<pinch>")
 'ignore)
(global-set-key
 (kbd "<C-wheel-up>")
 'ignore)
(global-set-key
 (kbd "<C-wheel-down>")
 'ignore)
;;Transform yes-or-no questions into y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "dired"
  '(progn
	 (define-key dired-mode-map
				 (kbd "<mouse-1>")
				 (kbd "<return>"))))
(add-hook 'dired-mode-hook
		  (lambda
			()
			(dired-omit-mode)
			(setq-local mouse-1-click-follows-link nil)))
(add-hook 'emacs-startup-hook
		  (lambda
			()
			(run-at-time 5 600 'elfeed-update)))
(add-hook 'elfeed-show-mode-hook
		  (lambda
			()
			(setq-local line-spacing 15)
			(setq-local fill-column 90)
			(setq-local shr-width 85)
			(variable-pitch-mode)
			(visual-fill-column-mode)))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (
   (html-ts-mode . lsp)
   (css-ts-mode . lsp)
   (js-ts-mode . lsp)
   (typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-skip-config-check t)
  (setq lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server"))
(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Obsidian MD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'obsidian)
;; Location of obsidian vault
(setopt obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
;; Default location for new notes from `obsidian-capture'
(setopt obsidian-inbox-directory "Inbox")
;; Useful if you're going to be using wiki links
(setopt markdown-enable-wiki-links t)

;; These bindings are only suggestions; it's okay to use other bindings
;; Create note
(define-key obsidian-mode-map (kbd "C-c C-n") 'obsidian-capture)
;; If you prefer you can use `obsidian-insert-wikilink'
(define-key obsidian-mode-map (kbd "C-c C-l") 'obsidian-insert-link)
;; Open file pointed to by link at point
(define-key obsidian-mode-map (kbd "C-c C-o") 'obsidian-follow-link-at-point)
;; Open a note note from vault
(define-key obsidian-mode-map (kbd "C-c C-p") 'obsidian-jump)
;; Follow a backlink for the current file
(define-key obsidian-mode-map (kbd "C-c C-b") 'obsidian-backlink-jump)

;; Activate obsidian mode and backlinks mode
(global-obsidian-mode t)
;; (obsidian-backlinks-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-in-ghostty ()
  (interactive)
  (let ((path nil))
    (when (derived-mode-p 'dired-mode)
	  (require 'dired)
	  (setq path (dired-get-file-for-visit)))
    (unless path
	  (setq path (buffer-file-name)))
    (when path
	  (shell-command
	   (concat "open -a Ghostty --args --working-directory=" (file-name-directory path))))))
;; Wrap quoted URLs with parentheses within the selected region
(defun wrap-urls-with-parentheses
	(start end)
  (interactive "r")
  (save-excursion
	(goto-char start)
	(while
		(re-search-forward "\"\\(https?://[^\"]+\\)\"" end t)
	  (replace-match "(\"\\1\")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Settings ⌘,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(global-hl-line-mode t)
 '(global-prettify-symbols-mode t)
 '(initial-buffer-choice "~/")
 '(lsp-completion-provider :none)
 '(lsp-copilot-enabled t)
 '(lsp-enable-snippet nil)
 '(make-backup-files nil)
 '(minions-mode t)
 '(minions-prominent-modes '(flymake-mode lsp-mode))
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(aggressive-indent elfeed flexoki-themes lsp-mode lsp-tailwindcss minions modalka obsidian treesit-auto undo-fu visual-fill-column))
 '(package-vc-selected-packages
   '((lsp-tailwindcss :url "https://github.com/merrickluo/lsp-tailwindcss" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(flymake-mode display-line-numbers-mode completion-preview-mode visual-line-mode aggressive-indent-mode modalka-mode))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install selected packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install-selected-packages)
(package-autoremove)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ⌘ Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key
 (kbd "s-1")
 (kbd "C-x 1"))
;; kill other windows (keep 1)
(global-set-key
 (kbd "s-2")
 (kbd "C-x 2"))
;; split horizontally
(global-set-key
 (kbd "s-3")
 (kbd "C-x 3"))
;; split vertically
(global-set-key
 (kbd "s-w")
 (kbd "s-k"))
;; close current window
(global-set-key
 (kbd "s-z")
 'undo-fu-only-undo)
(global-set-key
 (kbd "s-Z")
 'undo-fu-only-redo)
(global-set-key
 (kbd "s-r")
 'replace-string)
(global-set-key
 (kbd "s-d")
 'dired)
(global-set-key
 (kbd "s-e")
 'elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation and Selection mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modalka
  :init
  (setq-default
   modalka-cursor-type 'box)
  :config
  (modalka-define-kbd "SPC" "C-SPC")
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
  (modalka-define-kbd "c b" "C-c C-b")
  (modalka-define-kbd "c c" "C-c C-c")
  (modalka-define-kbd "c k" "C-c C-k")
  (modalka-define-kbd "c l" "C-c C-l")
  (modalka-define-kbd "c n" "C-c C-n")
  (modalka-define-kbd "c o" "C-c C-o")
  (modalka-define-kbd "c p" "C-c C-p")
  (modalka-define-kbd "c s" "C-c C-s")
  (modalka-define-kbd "c t" "C-c C-t")
  (modalka-define-kbd "c u" "C-c C-u")
  (modalka-define-kbd "c v" "C-c C-v")
  (modalka-define-kbd "c x" "C-c C-x")
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "h" "M-h")
  (modalka-define-kbd "i" "C-i")
  (modalka-define-kbd "j" "M-j")
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
  (modalka-define-kbd "x 3" "C-x #")
  (modalka-define-kbd "x ;" "C-x C-;")
  (modalka-define-kbd "x e" "C-x C-e")
  (modalka-define-kbd "x o" "C-x C-o")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "M-z")
  (modalka-define-kbd "A" "M-SPC")
  (modalka-define-kbd "B" "M-b")
  (modalka-define-kbd "C" "M-c")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "E" "M-e")
  (modalka-define-kbd "F" "M-f")
  (modalka-define-kbd "G" "C-`")
  (modalka-define-kbd "H" "M-H")
  ;; I (bound elsewhere)
  ;; J (bound elsewhere)
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "L" "M-l")
  (modalka-define-kbd "M" "M-m")
  (modalka-define-kbd "N" "M-n")
  (modalka-define-kbd "O" "M-o")
  (modalka-define-kbd "P" "M-p")
  ;; Q (bound elsewhere)
  (modalka-define-kbd "R" "M-r")
  (modalka-define-kbd "S" "M-S")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "U" "M-u")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "W" "M-w")
  ;; X (not bound)
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "Z" "C-z")
  :bind
  (("<f13>" . modalka-mode))
  :hook
  ((compilation-mode . modalka-mode)
   (conf-toml-mode . modalka-mode)
   (conf-unix-mode . modalka-mode)
   (diff-mode . modalka-mode)
   (help-mode . modalka-mode)))

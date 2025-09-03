;;; init.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:
(add-to-list 'load-path "~/.emacs.d/elpa/nerd-icons.el")
(require 'nerd-icons)
(defalias 'yes-or-no-p 'y-or-n-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various functions
(use-package emacs
  :bind
  ;; Command key bindings
  ("s-1" . execute-extended-command)
  ("s-2" . simulate-ctl-x-map)
  ("s-3" . simulate-mode-specific-map)
  ("s-4" . simulate-help-map)
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  ("s-o" . find-file)
  ("s-w" . kill-current-buffer)
  ("s-b" . bookmark-jump)
  ("s-t" . ghostty)
  ("s-e" . elfeed)
  ("s-y" . yt-dlp)
  ("s-;" . comment-box)
  ;; Colemak extend bindings
  ("M-<up>" . backward-up-list)
  ("M-<down>" . down-list)
  ("M-<left>" . backward-sexp)
  ("M-<right>" . forward-sexp)
  ("<home>" . move-beginning-of-line)
  ("<end>" . move-end-of-line)
  ;; Unmap default navigation bindings and text rescaling
  ("<pinch>" . nil) ("C-<wheel-up>" . nil) ("C-<wheel-down>" . nil) ("M-<wheel-up>" . nil) ("M-<wheel-down>" . nil) ("C-M-<wheel-up>" . nil) ("C-M-<wheel-down>" . nil)
  ([escape] . keyboard-quit)
  (:map esc-map ([escape] . keyboard-quit))
  (:map ctl-x-map ([escape] . keyboard-quit))
  (:map help-map ([escape] . keyboard-quit))
  (:map goto-map ([escape] . keyboard-quit))
  (:map minibuffer-mode-map ([escape] . minibuffer-keyboard-quit))
  :preface
  (defun simulate-ctl-x-map ()
	"Simulates the `C-x' key-press" (interactive)
	(setq unread-command-events (listify-key-sequence (read-kbd-macro "C-x"))))
  (defun simulate-mode-specific-map ()
	"Simulates the `C-c' key-press" (interactive)
	(setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c"))))
  (defun simulate-help-map ()
	"Simulates the `C-h' key-press" (interactive)
	(setq unread-command-events (listify-key-sequence (read-kbd-macro "C-h"))))
  (defun setup ()
	"Install selected packages."
	(interactive)
	(package-refresh-contents)
	(package-install-selected-packages)
	(package-autoremove))
  (defun ghostty ()
	"Open current directory in Ghostty."
	(interactive)
	(shell-command (concat "open -a Ghostty --args --working-directory=" "\""(expand-file-name default-directory)"\"")))
  (defun send-to-self	(message)
	"Send a MESSAGE to myself."
	(interactive "sMessage to send: ")
	(let ((message
		   (or message "")))			; Ensure message isn't nil
      (shell-command (format "osascript -e 'tell application \"Messages\" to send \"%s\" to buddy \"leaferiksen@gmail.com\"'" (shell-quote-argument message)))))
  (defun wrap-urls-with-parentheses (start end)
	"Wrap quoted URLs with parentheses from START to END."
	(interactive "r")
	(save-excursion (goto-char start)
					(while (re-search-forward "\"\\(https?://[^\"]+\\)\"" end t) (replace-match "(\"\\1\")")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(use-package dired
  :init
  (require 'ls-lisp)
  :bind
  (:map dired-mode-map
		("<mouse-1>" . nil)
		("<mouse-2>" . nil)
		("SPC" . 'quicklook)
		("<up>" . 'dired-previous-line)
		("<down>" . 'dired-next-line)
		("<right>" . 'dired-find-file)
		("<left>" . 'dired-up-directory)
		("u" . 'dired-previous-line)
		("e" . 'dired-next-line)
		("i" . 'dired-find-file)
		("n" . 'dired-up-directory)
		("k" . 'dired-unmark)
		("p" . nil)
		("f" . dired-finder-path)
		("v" . nil)
		("o" . 'dired-do-open)
		("a" . 'afinfo))
  :preface
  (defun quicklook ()
	"QuickLook the currently selected file in Dired."
	(interactive)
	(let ((filename (dired-get-file-for-visit))) (shell-command (concat "qlmanage -p \"" filename "\" > /dev/null 2>&1"))))
  (defun dired-finder-path ()
	"Open Dired in the frontmost Finder window path, if available."
	(interactive)
	(let ((path (ns-do-applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")))
      (if path (dired (string-trim path)) (message "No Finder window found."))))
  (defun afinfo ()
	"Get metadata for focused file."
	(interactive)
	(let ((filename (dired-get-file-for-visit))) (async-shell-command (format "afinfo --info '%s'" filename))))
  (defun yt-dlp (url)
	"Download the audio, video, or video with subs from a given URL."
	(interactive "sEnter URL to download: ")
	(let ((choice	(completing-read "Choose download option: " '("video" "video with subtitles" "audio"))))
      (cond
       ((string-equal choice "video") (async-shell-command (format "yt-dlp -S \"ext\" \"%s\"" url)))
       ((string-equal choice "video with subtitles") (async-shell-command (format "yt-dlp -S \"ext\" --write-subs \"%s\"" url)))
       ((string-equal choice "audio") (async-shell-command (format "yt-dlp -S \"ext\" -x --embed-thumbnail \"%s\"" url)))))))
(use-package vc-dir
  :bind
  (:map vc-dir-mode-map
		("u" . 'vc-dir-previous-line)
		("e" . 'vc-dir-next-line)
		("i" . 'vc-dir-find-file)
		("n" . nil)
		("p" . nil)
		("k" . 'vc-dir-unmark))
  :preface
  (defun vc-amend ()
	"Amend the previous commit title."
	(interactive)
	(vc-checkin nil 'git)
	(vc-git-log-edit-toggle-amend)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-mode
;; https://jblevins.org/projects/markdown-mode/
;; (require 'markdown-mode)
(use-package markdown
  :init (require 'markdown-mode)
  :custom
  (markdown-enable-wiki-links t)
  (markdown-hide-urls t)
  :hook
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . jinx-mode)
  (markdown-mode .
				 (lambda ()
				   (setq-local line-spacing 12)
				   (face-remap-add-relative 'default :family "Old Timey Code" :height 180)))
  :custom-face
  (markdown-code-face ((t (:family "Maple Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
  :bind
  (:map markdown-mode-map
		("s-i" . markdown-insert-italic))
  :preface
  (defun insert-date ()
	"Insert today's date in iso format."
	(interactive)
	(insert (format-time-string "%Y-%m-%d"))))
(use-package obsidian
  :preface (require 'obsidian)
  :config
  (global-obsidian-mode t)
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes")
  :bind
  ("s-d" . obsidian-daily-note)
  (:map obsidian-mode-map
		([remap markdown-follow-thing-at-point] . obsidian-follow-link-at-point)
		("s-S-j" . obsidian-backlink-jump)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog-mode
;; https://github.com/renzmann/treesit-auto
(use-package flymake
  :bind
  (:map flymake-diagnostics-buffer-mode-map
		("u" . 'previous-line)
		("e" . 'next-line)
		("n" . nil)
		("p" . nil)))
(use-package html-ts-mode
  :preface
  (defun http-server ()
	"Start a local server at ./index.html, avoiding port conflicts."
	(interactive)
	(unless (boundp 'http-port-offset)
      (setq http-port-offset 0))
	(setq http-port (- 9999 http-port-offset))
	(setq http-port-offset (1+ http-port-offset))
	(let ((filename (concat "http-server@" (prin1-to-string http-port) "<" (file-name-nondirectory (directory-file-name (file-name-directory default-directory))) ">")))
      (start-process filename filename "npx" "http-server" "-o" "-p" (number-to-string http-port)))))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)) ;; default is 0.2
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
  ((html-mode . lsp)
   (css-mode . lsp)
   (js-mode . lsp)
   (typescript-mode . lsp)
   (tsx-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :preface
  (defun fix-node ()
	"Unlink and relink node binaries."
	(interactive)
	(async-shell-command "/opt/homebrew/bin/brew unlink node && /opt/homebrew/bin/brew link --overwrite node")))
(use-package lsp-tailwindcss
  :after lsp-mode
  :custom
  (lsp-tailwindcss-add-on-mode t)
  (lsp-tailwindcss-skip-config-check t)
  (lsp-tailwindcss-server-path "/opt/homebrew/bin/tailwindcss-language-server"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://depp.brause.cc/nov.el/
(use-package nov
  :init
  (add-to-list 'auto-mode-alist
			   '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode .
			(lambda
			  ()
			  (setq-local line-spacing 15)
			  (setq-local fill-column 90)
			  (face-remap-add-relative 'variable-pitch :family "kermit" :height 240)))
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  :custom
  (nov-text-width t)
  (visual-fill-column-center-text t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :hook
  (elfeed-show-mode . variable-pitch-mode)
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . visual-fill-column-mode)
  (elfeed-show-mode .
					(lambda ()
					  (setq-local line-spacing 12)))
  :bind
  (:map elfeed-search-mode-map
		("u" . previous-line)
		("e" . next-line)
		("n" . elfeed-search-fetch)
		("i" . elfeed-search-show-entry))
  (:map elfeed-show-mode-map
		("<return>" . elfeed-show-next)
		("S-<return>" . elfeed-show-prev)
		("u" . elfeed-show-prev)
		("e" . elfeed-show-next)
		("n" . elfeed-kill-buffer)
		("i" . elfeed-show-visit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper
(use-package minesweeper
  :hook
  (minesweeper-mode .
					(lambda ()
					  (face-remap-add-relative 'default :height 200))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-global-mode t)
 '(auto-package-update-delete-old-versions t)
 '(backward-delete-char-untabify-method nil)
 '(completion-ignore-case t t)
 '(context-menu-mode t)
 '(cursor-type 'bar)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-create-destination-dirs 'ask)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-Alh")
 '(dired-mode-hook '(nerd-icons-dired-mode dired-omit-mode))
 '(dired-mouse-drag-files t)
 '(dired-omit-files
   "^~\\$[^/]*\\|#.*#\\|\\._\\|\\.DS_Store\\|\\.CFUserTextEncoding\\|\\.DocumentRevisions-V100\\|\\.Spotlight-V100\\|\\.TemporaryItems\\|\\.fseventsd")
 '(dired-recursive-copies 'always)
 '(editorconfig-mode t)
 '(electric-pair-mode t)
 '(elfeed-feeds
   '(("https://buttondown.com/monteiro/rss" design) ("https://www.kosatenmag.com/home?format=rss" anime) ("https://www.smbc-comics.com/comic/rss" comics) ("https://existentialcomics.com/rss.xml" comics) ("https://todon.eu/@PinkWug.rss" comics) ("https://www.davidrevoy.com/feed/en/rss" comics) ("https://www.penny-arcade.com/feed" comics) ("https://www.berkeleymews.com/feed/" comics) ("https://catandgirl.com/feed/" comics) ("https://thesecretknots.com/feed/" comics) ("https://feeds.feedburner.com/nerfnow/full" comics) ("https://modmagazine.net/feed.xml" gaming) ("https://remapradio.com/rss/" gaming) ("https://tomorrowcorporation.com/feed" gaming) ("https://enikofox.com/feed.xml" gaming) ("https://panic.com/blog/feed/" gaming) ("https://www.codeweavers.com/blog/?rss=1" gaming) ("https://drewdevault.com/blog/index.xml" linux) ("https://fireborn.mataroa.blog/rss/" linux) ("https://kde.org/index.xml" linux) ("https://asahilinux.org/blog/index.xml" linux) ("https://coffee-and-dreams.uk/feed.xml" linux) ("https://www.ypsidanger.com/rss/" linux) ("https://rosenzweig.io/feed.xml" linux) ("https://theevilskeleton.gitlab.io/feed.xml" linux) ("https://acidiclight.dev/rss.xml" linux) ("https://blog.xfce.org/feed" linux) ("https://blog.fyralabs.com/rss/" linux) ("https://carlschwan.eu/index.xml" linux) ("https://rabbitictranslator.com/blog/index.xml" linux) ("https://lxqt-project.org/feed.xml" linux) ("https://blogs.kde.org/index.xml" linux) ("https://thelibre.news/rss/" linux) ("https://css-tricks.com/feed/" design) ("https://www.smashingmagazine.com/feed/" design) ("https://rachelandrew.co.uk/feed/" design) ("https://piccalil.li/feed.xml" design) ("http://danluu.com/atom.xml" design) ("https://localghost.dev/feed.xml" design) ("https://www.tinylogger.com/90koil/rss" journals) ("https://anhvn.com/feed.xml" journals) ("https://tnywndr.cafe/index.xml" journals) ("https://annas-archive.li/blog/rss.xml" journals) ("https://daverupert.com/atom.xml" journals) ("https://carsonellis.substack.com/feed" journals) ("https://wokescientist.substack.com/feed" journals) ("https://hypercritical.co/feeds/main" journals) ("https://www.jessesquires.com/feed.xml" journals) ("https://ryanleetaylor.com/rss.xml" journals) ("https://themkat.net/feed.xml" journals) ("https://www.wordsbywes.ink/feed.xml" journals) ("https://blogsystem5.substack.com/feed" journals)))
 '(elfeed-search-filter "@1-month-ago +unread")
 '(fill-column 9999)
 '(frame-resize-pixelwise t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-visual-line-mode t)
 '(html-mode-hook '(lsp))
 '(html-ts-mode-indent-offset 4)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/Documents/")
 '(isearch-lazy-count t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format "   (%s/%s)")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(ns-pop-up-frames nil)
 '(obsidian-directory
   "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes" nil nil "Customized with use-package obsidian")
 '(package-selected-packages
   '(apheleia captain company eglot elfeed elfeed-protocol esxml exec-path-from-shell flexoki-themes flymake-eslint jinx lorem-ipsum lsp-mode lsp-tailwindcss massmapper minesweeper minions nerd-icons-dired nov obsidian snow spacious-padding swift-mode treesit-auto undo-fu visual-fill-column yasnippet))
 '(package-vc-selected-packages
   '((massmapper :url "https://github.com/meedstrom/massmapper")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook
   '(company-mode prettify-symbols-mode flymake-mode display-line-numbers-mode))
 '(project-mode-line t)
 '(project-vc-extra-root-markers '("project"))
 '(read-buffer-completion-ignore-case t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(shr-fill-text nil)
 '(shr-inhibit-images t)
 '(snow-pile-factor 1)
 '(spacious-padding-mode t)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(trash-directory "~/.Trash")
 '(use-dialog-box nil)
 '(use-package-always-ensure t)
 '(visual-fill-column-center-text t)
 '(visual-fill-column-width 85)
 '(warning-minimum-level :error)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Maple Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(markdown-italic-face ((t (:foreground "#100f0f" :slant italic))))
 '(outline-1 ((t (:height 1.4 :inherit 'default :foreground "#AF3029" :weight semi-bold))))
 '(outline-2 ((t (:height 1.3 :weight semi-bold :foreground "#BC5215" :inherit 'default))))
 '(outline-3 ((t (:height 1.2 :weight semi-bold :foreground "#AD8301" :inherit 'default))))
 '(outline-4 ((t (:height 1.1 :weight semi-bold :foreground "#66800B" :inherit 'default))))
 '(outline-5 ((t (:height 1.0 :weight semi-bold :foreground "#24837B" :inherit 'default))))
 '(outline-6 ((t (:height 1.0 :weight semi-bold :foreground "#205EA6" :inherit 'default))))
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :foundry "nil" :slant normal :weight regular :height 200 :width normal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme, taking current system APPEARANCE into consideration
;; https://codeberg.org/crmsnbleyd/flexoki-emacs-theme
(use-package flexoki-themes
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments t))
;; https://github.com/d12frosted/homebrew-emacs-plus
(add-hook 'ns-system-appearance-change-functions
		  (lambda (appearance)
			(mapc #'disable-theme custom-enabled-themes)
			(pcase appearance
			  ('light (load-theme 'flexoki-themes-light t)
					  (custom-set-faces
					   '(markdown-italic-face ((t (:foreground "#100f0f" :slant italic))))
					   '(outline-1 ((t (:height 1.4 :inherit 'default :foreground "#AF3029" :weight semi-bold))))
					   '(outline-2 ((t (:height 1.3 :weight semi-bold :foreground "#BC5215" :inherit 'default))))
					   '(outline-3 ((t (:height 1.2 :weight semi-bold :foreground "#AD8301" :inherit 'default))))
					   '(outline-4 ((t (:height 1.1 :weight semi-bold :foreground "#66800B" :inherit 'default))))
					   '(outline-5 ((t (:height 1.0 :weight semi-bold :foreground "#24837B" :inherit 'default))))
					   '(outline-6 ((t (:height 1.0 :weight semi-bold :foreground "#205EA6" :inherit 'default))))))
			  ('dark (load-theme 'flexoki-themes-dark t)
					 (custom-set-faces
					  '(markdown-italic-face ((t (:foreground "#CECDC3" :slant italic))))
					  '(outline-1 ((t (:height 1.4 :inherit 'default :foreground "#D14D41" :weight semi-bold))))
					  '(outline-2 ((t (:height 1.3 :weight semi-bold :foreground "#DA702C" :inherit 'default))))
					  '(outline-3 ((t (:height 1.2 :weight semi-bold :foreground "#D0A215" :inherit 'default))))
					  '(outline-4 ((t (:height 1.1 :weight semi-bold :foreground "#879A39" :inherit 'default))))
					  '(outline-5 ((t (:height 1.0 :weight semi-bold :foreground "#3AA99F" :inherit 'default))))
					  '(outline-6 ((t (:height 1.0 :weight semi-bold :foreground "#4385BE" :inherit 'default)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; https://github.com/meedstrom/massmapper
(use-package massmapper
  :init
  ;; (setq massmapper-homogenizing-winners
  ;; 		'(("C-x C-;" . global-map)
  ;; 		  ("C-c C-s" . markdown-mode-map)))
  (add-hook 'massmapper-keymap-found-hook #'massmapper-homogenize -50)
  :config
  (massmapper-mode 1))
(use-package which-key
  :after massmapper-mode)
;; Hide any key sequence involving more than one chord.  We have no reason to
;; see them after using `massmapper-homogenize'.
(with-eval-after-load 'which-key
  (cl-pushnew '((" .-." . nil) . t) which-key-replacement-alist
			  :test #'equal))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

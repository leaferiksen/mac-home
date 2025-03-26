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
 '(default-frame-alist '((undecorated . t) (fullscreen . maximized)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-agho --group-directories-first")
 '(dired-omit-files "\\`[.]?#\\|\\.DS_Store\\|\\`\\._")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '("https://xkcd.com/rss.xml" "https://www.smbc-comics.com/comic/rss"
     "https://www.questionablecontent.net/QCRSS.xml"
     "https://existentialcomics.com/rss.xml"
     "https://todon.eu/@PinkWug.rss"
     "https://www.davidrevoy.com/feed/en/rss"
     "https://www.penny-arcade.com/feed"
     "https://www.berkeleymews.com/feed/"
     "https://catandgirl.com/feed/" "https://thesecretknots.com/feed/"
     "https://feeds.feedburner.com/nerfnow/full"
     "https://modmagazine.net/feed.xml" "https://aftermath.site/feed"
     "https://remapradio.com/rss/"
     "https://tomorrowcorporation.com/feed"
     "https://enikofox.com/feed.xml" "https://panic.com/blog/feed/"
     "https://www.codeweavers.com/blog/?rss=1"
     "https://www.gameinformer.com/rss.xml"
     "https://drewdevault.com/blog/index.xml"
     "https://kde.org/index.xml"
     "https://asahilinux.org/blog/index.xml"
     "https://coffee-and-dreams.uk/feed.xml"
     "https://www.ypsidanger.com/rss/"
     "https://rosenzweig.io/feed.xml"
     "https://theevilskeleton.gitlab.io/feed.xml"
     "https://acidiclight.dev/rss.xml" "https://blog.xfce.org/feed"
     "https://blog.fyralabs.com/rss/"
     "https://carlschwan.eu/index.xml"
     "https://rabbitictranslator.com/blog/index.xml"
     "https://redstrate.com/blog/index.xml"
     "https://lxqt-project.org/feed.xml"
     "https://blogs.kde.org/index.xml" "https://thelibre.news/rss/"
     "https://obsidian.md/feed.xml"
     "https://blog.newsblur.com/feed.xml"
     "https://frame.work/blog.rss"
     "https://blogsystem5.substack.com/feed"
     "https://mullvad.net/blog/feed/rss/"
     "https://github.com/ai-robots-txt/ai.robots.txt/releases.atom"
     "https://www.wordsbywes.ink/feed.xml"
     "https://basicappleguy.com/basicappleblog?format=rss"
     "https://hypercritical.co/feeds/main"
     "https://www.jessesquires.com/feed.xml"
     "https://ryanleetaylor.com/rss.xml"
     "https://themkat.net/feed.xml" "https://css-tricks.com/feed/"
     "https://www.smashingmagazine.com/feed/"
     "https://rachelandrew.co.uk/feed/"
     "https://cdn.jwz.org/blog/feed/" "https://piccalil.li/feed.xml"
     "http://danluu.com/atom.xml" "https://localghost.dev/feed.xml"
     "https://www.tinylogger.com/90koil/rss"
     "https://anhvn.com/feed.xml" "https://tnywndr.cafe/index.xml"
     "https://www.girlonthenet.com/feed/"
     "https://annas-archive.li/blog/rss.xml"
     "https://daverupert.com/atom.xml"
     "https://carsonellis.substack.com/feed"
     "https://wokescientist.substack.com/feed"
     "https://lwlies.com/feed/"
     "https://howtodothingswithmemes.substack.com/feed"))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-completion-preview-mode t)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(initial-buffer-choice "~/")
 '(make-backup-files nil)
 '(package-selected-packages
   '(elfeed flexoki-themes magit undo-fu web-mode))
 '(pixel-scroll-precision-mode t)
 '(project-mode-line t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(trash-directory "~/.Trash")
 '(use-package-always-ensure t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Red Hat Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)
;; Auto theme
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'flexoki-themes-light t))
    ('dark (load-theme 'flexoki-themes-dark t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
;; Fix the trackpad
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cmd Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal arrow movement
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e")) ;; Select to end of line
(global-set-key (kbd "s-<left>") 'back-to-indentation)
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))  ;; Select to beginning of line
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; split vertically
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; close current window
(global-set-key (kbd "s-0") (kbd "C-0"))
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "s-d") 'dired)
(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "M-s-b") 'bookmark-set)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-e") 'elfeed)
;; Open the folder containing the current file or directory in Ghostty
(global-set-key (kbd "s-t")
                (lambda ()
                  (interactive)
                  (let ((path (if (derived-mode-p 'dired-mode)
                                  (dired-get-file-for-visit)
                                (buffer-file-name))))
                    (when path
                      (shell-command (concat "open -a Ghostty --args --working-directory="
                                             (shell-quote-argument (file-name-directory path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Mode
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2]
    (lambda (event)
      "In Dired, visit the file or directory name you click on."
      (interactive "e")
      (mouse-set-point event)
      (dired-find-file))))
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))
;; Web Mode
(use-package web-mode :mode ("\\.html\\'" . web-mode) :config
  (with-eval-after-load 'web-mode
    (define-key web-mode-map (kbd "C-c b") 'browse-url-of-file)))
(add-hook 'after-save-hook (lambda ()
                              (when (eq major-mode 'web-mode)
                                (shell-command "rustywind --write . > /dev/null 2>&1")
                                (revert-buffer :ignore-auto :noconfirm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install selected packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archived config ideas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package copilot :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main") :config
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "<S-tab>") 'copilot-accept-completion-by-word))
;; (use-package html-autoview-mode :ensure nil :hook (mhtml-mode . html-autoview-mode)) ;;opens too many tabs
;; (global-set-key (kbd "s-Z") 'undo-redo)

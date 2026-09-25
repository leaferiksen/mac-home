;;; disabled.el --- Disabled Emacs Initialization -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Leaf Eriksen

;;; Commentary:

;; This is sorted first by how recently they were in use... kinda?

;;; Code:

(window-buffer-change-functions . speedbar-refresh-on-non-file-buffers)
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

(use-package agent-shell-macext
  :vc (:url "https://github.com/cxa/agent-shell-macext")
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom (agent-shell-macext-file-copy-policy 'auto)
  (agent-shell-macext-notifications t)
  (agent-shell-macext-notify-current-buffer nil))

(cl-defun apheleia-elfmt
    (&key buffer scratch callback &allow-other-keys)
  "Format SCRATCH with `elfmt', then invoke CALLBACK.
Indentation settings are copied from BUFFER so the result matches
what you'd get by typing TAB there."
  (let ((fc (buffer-local-value 'fill-column buffer))
	(tabs (buffer-local-value 'indent-tabs-mode buffer))
	(indent-fn
	 (buffer-local-value 'lisp-indent-function buffer))
	(original (with-current-buffer scratch (buffer-string))))
    (with-current-buffer scratch
      (delay-mode-hooks (emacs-lisp-mode))
      ;; after the major mode, so these aren't clobbered
      (setq-local fill-column fc indent-tabs-mode tabs lisp-indent-function indent-fn)
      (condition-case err
	  (let ((gc-cons-threshold most-positive-fixnum)
		(inhibit-message t)
		(message-log-max nil))
	    (goto-char (point-max))
	    (while (not (bobp))
	      (backward-sexp)
	      (elfmt--sexp)))
	;; elfmt errors on unbalanced parens and old-style backquotes;
	;; roll back so apheleia applies an empty patch instead of garbage
	(error
	 (erase-buffer)
	 (insert original)
	 (message "elfmt: %s" (error-message-string err))))))
  (funcall callback))

(use-package reader
  :ensure t
  :vc (:url "https://codeberg.org/MonadicSheep/emacs-reader" :make "all")
  :config (defun fix-reader
	      ()
	    "Recompile Reader Libraries"
	    (interactive)
	    (let ((default-directory "~/.config/emacs/elpa/reader/"))
	      (shell-command "make clean all"))))

(use-package agent-shell-sidebar
  :ensure t
  :after agent-shell
  :vc (:url "https://github.com/cmacrae/agent-shell-sidebar")
  :custom
  (agent-shell-sidebar-minimum-width 60)
  (agent-shell-sidebar-default-config (agent-shell-opencode-make-agent-config))
  :init
  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "a") #'agent-shell-sidebar-toggle)))

(defun project-npx-serve ()
  "Clear clipboard, npx serve the project's root directory, call clipboard watcher."
  (interactive)
  (gui-set-selection 'CLIPBOARD "")
  (project-run "serve" "Serving %s..." "npx" "serve")
  (watch-clipboard-xwidget-webkit-browse-url))
(defun watch-clipboard-xwidget-webkit-browse-url ()
  "Watch for clipboard data and open in Xwidgets."
  (if-let* ((current-clip (gui-get-selection 'CLIPBOARD 'STRING)) ;; * may break
            ((not (string-empty-p current-clip))))
      (progn
        (split-and-follow-horizontally)
        (xwidget-webkit-browse-url current-clip)
        (message "Clipboard update detected! Opened %s in Xwidgets" current-clip))
    (run-at-time "0.5 sec" nil #'watch-clipboard-xwidget-webkit-browse-url)))

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize with no frame

("M-q" . save-buffers-kill-emacs)
("M-w" . kill-current-buffer)
("M-z" . undo-only)
("M-Z" . undo-redo)
("M-x" . kill-region)
("M-c" . ns-copy-including-secondary)
("M-v" . yank)
("M-o" . execute-extended-command)

(:map markdown-ts-mode-map ("s-<return>" . markdown-follow-any-link))
(defun markdown-follow-any-link ()
  (interactive)
  (cond
   ((thing-at-point-looking-at "\\[\\[\\([^]]+\\)\\]\\]")
    (when-let* ((path (match-string 1))) ;; * might fail
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
:bind (:prefix "C-c m" :prefix-map markdown-actions ("m" . markdown-more-emphasis) ("l" . markdown-less-emphasis))
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
        (delete-region (- beg 1) beg)))))

(advice-add 'completing-read :around
            (lambda (orig prompt &rest args)
	      (apply orig (if (eq this-command 'execute-extended-command)
			      (string-replace "M-x" "M-o" prompt)
                            prompt)
                     args)))

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
(emacs-startup . remap-all-ts-modes)
(defun remap-all-ts-modes ()
  "Remap all available tree-sitter modes to their standard counterparts."
  (interactive)
  (dolist (ts-mode (apropos-internal "-ts-mode$" #'commandp))
    (when-let ((old-mode (intern-soft (concat (string-remove-suffix "-ts-mode" (symbol-name ts-mode)) "-mode")))
               ((fboundp old-mode)))
      (add-to-list 'major-mode-remap-alist (cons old-mode ts-mode)))))

(use-package swift-ts-mode
  :ensure t
  :if (memq window-system '(ns))
  :mode "\\.swift\\'"
  :hook (swift-mode . eglot-ensure)
  :bind (:prefix "C-c x" :prefix-map xcode ("b" . xcode-build) ("r" . xcode-run) ("t" . xcode-test))
  :config
  ;; https://github.com/alex-pinkus/tree-sitter-swift#where-is-your-parserc
  ;; https://github.com/alex-pinkus/tree-sitter-swift/actions/workflows/parser-src.yml
  (add-to-list 'treesit-language-source-alist '(swift "/Users/leaf/.config/emacs/tree-sitter/tree-sitter-swift" nil "."))
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-mode-alist '(swift-ts-mode . swift-format))
    (add-to-list 'apheleia-formatters '(swift-format "xcrun" "swift-format" (buffer-file-name)))))

(use-package auth-source
  :custom (auth-sources "~/.authinfo"))

(use-package epg-config
  :custom (epg-pinentry-mode 'loopback))

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

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
		    (font-spec :family "Hiragino Mincho ProN")))

;; Mode-line
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

;; macOS keybinds
(keymap-global-set "C-<up>" 'beginning-of-buffer)
(keymap-global-set "C-<down>" 'end-of-buffer)
(keymap-global-set "C-<left>" 'move-beginning-of-line)
(keymap-global-set "C-<right>" 'move-end-of-line)

(keymap-global-set "C-w" 'kill-current-buffer)
(keymap-global-set "C-o" 'find-file)
(keymap-global-set "C-a" 'mark-whole-buffer)
(keymap-global-set "C-s" 'save-buffer)
(keymap-global-set "C-S-s" 'write-file)
(keymap-global-set "C-f" 'isearch-forward)
(keymap-set isearch-mode-map "C-f" 'isearch-forward)
(keymap-global-set "C-S-f" 'isearch-backward)
(keymap-global-set "C-M-f" 'isearch-forward-regexp)
(keymap-global-set "C-M-S-f" 'isearch-backward-regexp)

;;; disabled.el ends here

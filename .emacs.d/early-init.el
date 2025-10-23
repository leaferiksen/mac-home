;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
(add-to-list 'default-frame-alist '(undecorated-round . t)) ;; rounded with no title
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; Fix for Native Comp (AOT) linker errors on macOS GUI launch;
(let ((brew-prefix "/opt/homebrew/bin"))
  (when (file-directory-p brew-prefix)
    (setenv "PATH" (concat brew-prefix ":" (getenv "PATH")))
    (add-to-list 'exec-path brew-prefix)))
(setq native-comp-deferred-compilation nil
	  native-comp-jit-compilation nil)
;;; early-init.el ends here

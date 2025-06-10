;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; Maximize with no frame
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Optimize for PaperWM.spoon
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;;; early-init.el ends here

;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
;; title present but transparent
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; rounded with no title (optimize for PaperWM.spoon)
(add-to-list 'default-frame-alist '(undecorated-round . t))
;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; Maximize with no frame
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;;; early-init.el ends here

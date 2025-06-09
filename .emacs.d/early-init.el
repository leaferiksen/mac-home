;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
;; Maximize with no frame
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Optimize for PaperWM.spoon
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;;; early-init.el ends here

;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Maximize with no frame
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Optimize for PaperWM.spoon
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
;;; early-init.el ends here

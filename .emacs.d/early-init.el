;;; early-init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;; Early initialization file for Emacs
;;; Code:
;; Speed up lsp-mode
(setenv "LSP_USE_PLISTS" "true")
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;; title present but transparent
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq custom-file (make-temp-file "~/.cache/emacs/custom"))
;;; early-init.el ends here

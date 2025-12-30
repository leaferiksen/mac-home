;;; keyboard-experiment.el --- Emacs Initialization user config  -*- lexical-binding: t; -*-
;;; Commentary:
;; by Leaf Eriksen
;;; Code:

;; Colemak extend bindings
("s-<up>" . backward-up-list) ("s-<down>" . down-list) ("s-<left>" . backward-sexp) ("s-<right>" . forward-sexp) 
("<home>" . beginning-of-visual-line) ("<end>" . end-of-visual-line) ("M-<delete>" . kill-word)
(use-package dired
  :bind (:map dired-mode-map (("<mouse-1>" . nil)
							  ("<mouse-2>" . nil)
							  ("SPC" . 'quicklook)
							  ("u" . 'dired-previous-line)
							  ("e" . 'dired-next-line)
							  ("i" . 'dired-find-file)
							  ("n" . 'dired-up-directory)
							  ("p" . nil)
							  ("f" . dired-finder-path)
							  ("k" . 'dired-unmark)
							  ("v" . nil)
							  ("o" . 'dired-do-open)
							  ("a" . 'afinfo))))
(use-package vc-dir
  :bind (:map vc-dir-mode-map (("u" . 'vc-dir-previous-line)
							   ("e" . 'vc-dir-next-line)
							   ("i" . 'vc-dir-find-file)
							   ("n" . nil)
							   ("p" . nil)
							   ("f" . nil)
							   ("k" . 'vc-dir-unmark))))
(use-package arc-mode
  :bind (:map archive-mode-map (("u" . 'archive-previous-line)
								("e" . 'archive-next-line)
								("i" . 'archive-extract)
								("n" . 'arc-open-parent-folder-and-quit)
								("p" . nil)
								("f" . nil)
								("k" . 'archive-unflag))))
(use-package diff-mode
  :bind (:map diff-mode-read-only (("u" . 'diff-hunk-prev)
								   ("e" . 'diff-hunk-next)
								   ("U" . 'diff-file-prev)
								   ("E" . 'diff-file-next)
								   ("n" . nil)
								   ("p" . nil))))
(use-package completion-preview
  :bind (:map completion-preview-active-mode-map (("M-<up>" . completion-preview-next-candidate)
												  ("M-<down>" . completion-preview-prev-candidate))))
(use-package flymake-mode
  :bind (:map flymake-diagnostics-buffer-mode-map (("u" . 'previous-line)
												   ("e" . 'next-line)
												   ("n" . nil)
												   ("p" . nil))))
(use-package help-mode
  :bind (:map help-mode-map (("u" . help-goto-previous-page)
							 ("e" . help-goto-next-page)
							 ("n" . nil)
							 ("p" . nil))))
:bind ((:map elfeed-search-mode-map (("u" . previous-line)
									 ("e" . next-line)
									 ("n" . elfeed-search-fetch)
									 ("i" . elfeed-search-show-entry)))
	   (:map elfeed-show-mode-map (("<return>" . elfeed-show-next)
								   ("S-<return>" . elfeed-show-prev)
								   ("u" . elfeed-show-prev)
								   ("e" . elfeed-show-next)
								   ("n" . elfeed-kill-buffer)
								   ("i" . elfeed-show-visit))))
;;; keyboard-experiment.el ends here

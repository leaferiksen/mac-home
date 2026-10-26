;;; macos-text-qol-mode.el --- macOS-style text auto-corrections  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Leaf Eriksen

;; Author: Leaf Eriksen <leaferiksen@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `macos-text-qol-mode' is a buffer-local minor mode that recreates a few
;; macOS text-entry conveniences while you type:
;;
;;   - Typing a second space after a word turns the two spaces into ". ".
;;   - A standalone lowercase "i" becomes "I".
;;   - A lowercase letter at the start of the buffer, after a sentence end,
;;     or at the start of a paragraph is capitalized.
;;
;; Enable it in the buffers you want, for example:
;;
;;   (add-hook 'markdown-ts-mode-hook #'macos-text-qol-mode)

;;; Code:

(defun macos-text-qol--post-self-insert ()
  "Apply macOS-style corrections after a character is self-inserted."
  (when-let* ((char (char-before)))
    (cond
     ;; Double space after a word -> ". "
     ((and-let* (((eq char ?\s))
                 ((eq (char-before (1- (point))) ?\s))
                 (prev (char-before (- (point) 2))))
        (eq (char-syntax prev) ?w))
      (delete-char -2)
      (insert ". "))
     ;; Standalone "i" -> "I"
     ((and-let* (((not (eq (char-syntax char) ?w)))
                 ((> (point) 2)))
        (looking-back "\\bi\\b\\(.\\)" (max (point-min) (- (point) 3))))
      (replace-match "I\\1"))
     ;; Auto-capitalize at buffer start, after sentence end, or after a
     ;; blank line.
     ((and-let* (((string-match-p "[[:lower:]]" (string char))))
        (looking-back
         (concat "\\(?:"
                 "\\`[ \t\n]*"          ; start of buffer
                 "\\|[.!?][ \t\n]+"     ; end of sentence
                 "\\|\n[ \t]*\n[ \t]*"  ; blank line (new paragraph)
                 "\\)"
                 ".")
         (max (point-min) (- (point) 20))))
      (upcase-region (1- (point)) (point))))))

;;;###autoload
(define-minor-mode macos-text-qol-mode
  "Minor mode to enable macOS-style text auto-corrections."
  :lighter " MacQoL"
  (if macos-text-qol-mode
      (add-hook 'post-self-insert-hook
                #'macos-text-qol--post-self-insert nil t)
    (remove-hook 'post-self-insert-hook
                 #'macos-text-qol--post-self-insert t)))

(provide 'macos-text-qol-mode)
;;; macos-text-qol-mode.el ends here


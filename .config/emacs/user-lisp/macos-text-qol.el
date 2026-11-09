;;; macos-text-qol.el --- macOS-style text auto-corrections  -*- lexical-binding: t; -*-

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
;;     at the start of a paragraph, or at the start of a markdown list item
;;     is capitalized.
;;   - Dotted abbreviations such as "U.S.", "e.g." or "a.m." are not treated
;;     as sentence ends, so the next word stays lowercase.
;;
;; Enable it in the buffers you want, for example:
;;
;;   (add-hook 'markdown-ts-mode-hook #'macos-text-qol-mode)

;;; Code:

(defconst macos-text-qol--bullet-regexp
  "^[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[[ xX]\\][ \t]+\\)?"
  "Match the start of a markdown list item, including task checkboxes.")

(defun macos-text-qol--abbreviation-end-p ()
  "Return non-nil if point is right after a dotted abbreviation like U.S."
  (looking-back
   "\\(?:\\`\\|[^[:alnum:].]\\)\\(?:[[:alpha:]]\\.\\)\\{2,\\}"
   (max (point-min) (- (point) 20))))

(defun macos-text-qol--capitalize-position-p (pos)
  "Return non-nil if the character at POS should be capitalized."
  (save-excursion
    (goto-char pos)
    (let ((limit (max (point-min) (- (point) 20))))
      (or
       ;; Start of buffer.
       (save-excursion (skip-chars-backward " \t\n") (bobp))
       ;; Start of a paragraph.
       (looking-back "\n[ \t]*\n[ \t]*" limit)
       ;; Start of a markdown list item.
       (looking-back macos-text-qol--bullet-regexp (line-beginning-position))
       ;; After a sentence end, unless the period belongs to an abbreviation.
       (and (looking-back "[.!?][ \t\n]+" limit)
            (not (save-excursion
                   (goto-char (1+ (match-beginning 0)))
                   (macos-text-qol--abbreviation-end-p))))))))

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
     ;; Auto-capitalize at buffer start, after sentence end, after a
     ;; blank line, or after a markdown bullet.
     ((and-let* (((string-match-p "[[:lower:]]" (string char)))
                 ((macos-text-qol--capitalize-position-p (1- (point)))))
        (upcase-region (1- (point)) (point)))))))

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
;;; macos-text-qol.el ends here

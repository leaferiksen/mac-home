;;; markdown-ts-csv-align.el --- Column-align csv code blocks in markdown-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Leaf Eriksen

;; Author: Leaf Eriksen <leaferiksen@gmail.com>
;; Keywords: text
;; Package-Requires: ((emacs "31.1") (csv-mode "1.27"))

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

;; This package provides `markdown-ts-csv-align-mode', a buffer-local
;; minor mode that uses tree-sitter to find ```csv fenced code blocks
;; and aligns their columns with `csv-align-fields' (soft alignment, via
;; overlays; the buffer text is never modified).  Alignment is refreshed
;; after a short idle delay following each edit.
;;
;; Usage:
;;
;;   (add-hook 'markdown-ts-mode-hook #'markdown-ts-csv-align-mode)
;;
;; Only the language tag "csv" is recognized, and the separator is
;; whatever `csv-mode' would use by default (see `csv-separator-chars').
;;
;; If the buffer uses `variable-pitch-mode', the padding that
;; `csv-align-fields' inserts would be drawn in the proportional font and
;; the columns would not line up.  The padding is therefore given the
;; face `markdown-ts-csv-align-padding', which inherits `fixed-pitch'.

;;; Code:

(require 'csv-mode)
(require 'treesit)

(defgroup markdown-ts-csv-align nil
  "Align csv fenced code blocks in `markdown-ts-mode'."
  :group 'text
  :prefix "markdown-ts-csv-align-")

(defcustom markdown-ts-csv-align-idle-delay 0.3
  "Seconds of idle time after an edit before csv blocks are re-aligned."
  :type 'number)

(defface markdown-ts-csv-align-padding
  '((t :inherit fixed-pitch))
  "Face for the padding inserted between aligned csv fields.
It should use the same font as the code block text, so that the
columns line up when the buffer's default face is remapped, for
example by `variable-pitch-mode'.")

(defconst markdown-ts-csv-align--query
  '((fenced_code_block
     (info_string (language) @lang)
     (code_fence_content) @body
     (:equal @lang "csv")))
  "Tree-sitter query matching the body of csv fenced code blocks.")

(defvar-local markdown-ts-csv-align--timer nil
  "Pending idle timer for re-aligning the current buffer.")

(defun markdown-ts-csv-align--clear ()
  "Delete every csv alignment overlay in the buffer.
This includes stale overlays outside any current block, for example on
a line whose text was just killed."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'csv)
      (delete-overlay ov))))

(defun markdown-ts-csv-align--tidy (ov)
  "Fix up OV if it is a csv alignment overlay.
Give its padding strings `markdown-ts-csv-align-padding' and make it
vanish along with its text."
  (and-let* (((overlay-get ov 'csv)))
    (dolist (prop '(before-string after-string))
      (and-let* ((str (overlay-get ov prop)))
        (overlay-put ov prop
                     (propertize str 'face 'markdown-ts-csv-align-padding))))
    ;; An empty overlay (a null field) would be deleted at once.
    (when (< (overlay-start ov) (overlay-end ov))
      (overlay-put ov 'evaporate t))))

;;;###autoload
(defun markdown-ts-csv-align-buffer ()
  "Align the columns of all csv fenced code blocks in the buffer."
  (interactive)
  (markdown-ts-csv-align--clear)
  (when (treesit-parser-list nil 'markdown)
    (dolist (cap (treesit-query-capture
                  'markdown markdown-ts-csv-align--query))
      (when (eq (car cap) 'body)
        (let ((beg (save-excursion
                     (goto-char (treesit-node-start (cdr cap)))
                     (pos-bol)))
              (end (save-excursion
                     (goto-char (treesit-node-end (cdr cap)))
                     (skip-chars-backward " \t\n")
                     (point))))
          (when (< beg end)
            (csv-align-fields nil beg end)
            (mapc #'markdown-ts-csv-align--tidy (overlays-in beg end))))))))

(defun markdown-ts-csv-align--schedule (&rest _)
  "Schedule a re-alignment after `markdown-ts-csv-align-idle-delay'."
  (when (timerp markdown-ts-csv-align--timer)
    (cancel-timer markdown-ts-csv-align--timer))
  (let ((buf (current-buffer)))
    (setq markdown-ts-csv-align--timer
          (run-with-idle-timer
           markdown-ts-csv-align-idle-delay nil
           (lambda ()
             (when (and (buffer-live-p buf)
                        (buffer-local-value 'markdown-ts-csv-align-mode buf))
               (with-current-buffer buf
                 (markdown-ts-csv-align-buffer))))))))

;;;###autoload
(define-minor-mode markdown-ts-csv-align-mode
  "Keep csv fenced code blocks column-aligned in `markdown-ts-mode'."
  :lighter " CSV⇥"
  (if markdown-ts-csv-align-mode
      (progn
        (markdown-ts-csv-align-buffer)
        (add-hook 'after-change-functions
                  #'markdown-ts-csv-align--schedule nil t))
    (remove-hook 'after-change-functions
                 #'markdown-ts-csv-align--schedule t)
    (when (timerp markdown-ts-csv-align--timer)
      (cancel-timer markdown-ts-csv-align--timer))
    (markdown-ts-csv-align--clear)))

(provide 'markdown-ts-csv-align-mode)
;;; markdown-ts-csv-align.el ends here


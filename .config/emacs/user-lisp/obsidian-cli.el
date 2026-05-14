;;; obsidian-cli.el --- Integrate with Obisidian CLI  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Leaf Eriksen

;; Author: Leaf Eriksen <leaferiksen@gmail.com>
;; Keywords: convenience, tools

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

;; Obsidian must be running for the cli to work, so ensure it starts
;; on login. To automatically hide Obsidian once it launches, you can
;; use the "Javascript Init" plugin to run "electronWindow.minimize()"

;; If you use Obsidian mobile, I recommend the plugin "File Title
;; Updater" to ensure functional consistency with Emacs with the
;; obsidian-cli-update-title feature. Ensure the "Default title
;; source" is "First Heading" and the Sync mode is "Filename +
;; Heading"

;;; Code:

(defcustom obsidian-vault
  (expand-file-name "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes/")
  "Path to the Obsidian vault directory."
  :type 'directory
  :group 'obsidian)

(defun obsidian-cli-update-title ()
  (when-let* ((path (buffer-file-name))
              (_    (string-prefix-p obsidian-vault path))
              (new  (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^# \\(.+\\)$" nil t)
                        (match-string 1))))
              (_    (not (string= (file-name-base path) new))))
    (shell-command (format "obsidian rename file=%s name=%s"
                           (shell-quote-argument (file-name-nondirectory path))
                           (shell-quote-argument new)))
    (set-visited-file-name (concat obsidian-vault new ".md") t t)
    (set-buffer-modified-p nil)))

(defun obsidian-cli-daily-note ()
  "Open today's daily note."
  (interactive)
  (let* ((raw (shell-command-to-string "obsidian daily:path"))
         (path (string-trim raw)))
    (if (string= path "")
        (message "Obsidian: No daily note path returned")
      (find-file (expand-file-name path obsidian-vault)))))

(defun obsidian-cli-jump-to-backlink ()
  (interactive)
  (when-let* ((path  (buffer-file-name))
              (_     (string-prefix-p obsidian-vault path))
              (raw   (shell-command-to-string
                      (format "obsidian backlinks file=%s"
                              (shell-quote-argument (file-name-nondirectory path)))))
              (links (split-string (string-trim raw) "\n" t))
              (pick  (completing-read "Backlink: " links nil t)))
    (find-file (concat obsidian-vault pick ".md"))))

;;;###autoload
(define-minor-mode obsidian-cli-mode
  "Toggle Obsidian CLI integration."
  :init-value nil
  :lighter " Obs"
  (if obsidian-cli-mode
      (add-hook 'after-save-hook #'obsidian-cli-update-title nil t)
    (remove-hook 'after-save-hook #'obsidian-cli-update-title t)))

(provide 'obsidian-cli)
;;; obsidian-cli.el ends here

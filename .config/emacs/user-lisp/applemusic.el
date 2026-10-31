;;; applemusic.el --- cli frontend                   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Leaf Eriksen

;; Author: Leaf Eriksen <leaferiksen@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, tools

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

;; A thin wrapper around the `am' binary from
;; https://github.com/leaferiksen/applemusic-cli (built with
;; `bun build --compile src/index.ts --outfile am && mv am ~/.local/bin/am').
;;
;; Setup:
;;   (require 'applemusic)
;;   (global-set-key (kbd "C-c m") #'applemusic-menu)
;;
;; Design: all AppleScript lives in the CLI.  This file only runs `am',
;; shows its output, and offers menus.  Anything the CLI prints in a
;; format we need to understand is parsed in exactly two places:
;; `applemusic--parse-item' (list output) and
;; `applemusic--current-volume' (volume output).

;;; Code:


(defgroup applemusic nil
  "Control Apple Music through applemusic-cli."
  :group 'multimedia)

(defcustom applemusic-executable (expand-file-name "~/.local/bin/am")
  "Path to the compiled `am' binary."
  :type 'file)

(defcustom applemusic-list-limit 0
  "Maximum number of items to fetch when listing playlists, albums or searching.
0 means no limit."
  :type 'integer)

;;;; Running the CLI

(defun applemusic--run (&rest args)
  "Run the CLI with ARGS and return its trimmed output.
Signal a `user-error' if the binary is missing or exits non-zero."
  (if-let* ((exe (executable-find applemusic-executable)))
      (with-temp-buffer
        (let* ((status (apply #'call-process exe nil t nil args))
               (output (string-trim (buffer-string))))
          (if (eq status 0)
              output
            (user-error "am %s: %s" (string-join args " ") output))))
    (user-error "Can't find `am' at %s (see `applemusic-executable')"
                applemusic-executable)))

(defun applemusic--say (&rest args)
  "Run the CLI with ARGS and echo any output."
  (when-let* ((out (apply #'applemusic--run args))
              ((not (string-empty-p out))))
    (message "%s" out)))

(defmacro applemusic--define-simple (name doc &rest cli-args)
  "Define interactive command NAME with DOC that runs the CLI with CLI-ARGS."
  (declare (indent 2))
  `(defun ,name ()
     ,doc
     (interactive)
     (applemusic--say ,@cli-args)))

;;;; Parsing (the only places that depend on CLI output format)

(defun applemusic--parse-item (line)
  "Turn one LINE of list output into a candidate string.
The CLI prefixes each line with its index (\"13. Name\"); drop that."
  (string-trim
   (replace-regexp-in-string "\\`[ \t]*[0-9]+[.)][ \t]+" "" line)))

(defun applemusic--list (&rest args)
  "Run the CLI with ARGS plus the limit, and return the numbered items.
Only lines of the form \"N. Name\" are kept, so the CLI's header line and
\"Use --offset\" hint are ignored."
  (mapcar #'applemusic--parse-item
          (seq-filter
           (lambda (line) (string-match-p "\\`[ \t]*[0-9]+[.)][ \t]" line))
           (split-string
            (apply #'applemusic--run
                   (append args
                           (list "--limit"
                                 (number-to-string applemusic-list-limit))))
            "\n" t))))

(defun applemusic--current-volume ()
  "Return the current volume as an integer, or nil."
  (and-let* ((out (applemusic--run "vol"))
             ((string-match "[0-9]+" out)))
    (string-to-number (match-string 0 out))))

;;;; Menus

(defun applemusic--menu (title entries)
  "Show a one-key menu named TITLE and run the chosen entry.
ENTRIES is a list of (KEY LABEL COMMAND).  COMMAND is called
interactively if it is a command, otherwise with no arguments."
  (let ((prompt (format "%s: %s  [q] quit "
                        title
                        (mapconcat (lambda (e) (format "[%c] %s" (nth 0 e) (nth 1 e)))
                                   entries "  "))))
    (and-let* ((key (read-char-choice prompt (cons ?q (mapcar #'car entries))))
               ((not (eq key ?q)))
               (action (nth 2 (assq key entries))))
      (if (commandp action)
          (call-interactively action)
        (funcall action)))))

(defun applemusic--pick (prompt items)
  "Ask the user to choose from ITEMS with PROMPT.  Return nil if nothing chosen."
  (and-let* ((items)
             (choice (completing-read prompt items nil t))
             ((not (string-empty-p choice))))
    choice))

;;;; Playback

(applemusic--define-simple applemusic-play   "Start playback."     "play")
(applemusic--define-simple applemusic-pause  "Pause playback."     "pause")
(applemusic--define-simple applemusic-toggle "Toggle play/pause."  "toggle")
(applemusic--define-simple applemusic-next   "Skip to next track." "next")
(applemusic--define-simple applemusic-prev   "Go to previous track." "prev")
(applemusic--define-simple applemusic-now    "Show the current track." "now")

;;;; Volume

(defun applemusic-volume ()
  "Show the current volume, then set a new one with a single key.
Keys 1-9 set the volume to 10-90 percent, and 0 sets it to 100."
  (interactive)
  (let ((prompt (format "Volume is %s.  Set to: [1-9] = 10-90, [0] = 100, [q] quit "
                        (or (applemusic--current-volume) "unknown"))))
    (and-let* ((key (read-char-choice prompt (append "0123456789q" nil)))
               ((not (eq key ?q))))
      (applemusic--say "vol" (number-to-string
                              (if (eq key ?0) 100 (* 10 (- key ?0))))))))

;;;; Shuffle and repeat

(defun applemusic-shuffle (state)
  "Set shuffle to STATE, \"on\" or \"off\"."
  (interactive
   (list (pcase (read-char-choice "Shuffle: [o]n  o[f]f " '(?o ?f))
           (?o "on")
           (?f "off"))))
  (applemusic--say "shuffle" state))

(defun applemusic-repeat (mode)
  "Set repeat to MODE, one of \"off\", \"one\" or \"all\"."
  (interactive
   (list (pcase (read-char-choice "Repeat: [o]ff  o[n]e  [a]ll " '(?o ?n ?a))
           (?o "off")
           (?n "one")
           (?a "all"))))
  (applemusic--say "repeat" mode))

;;;; Library

(defun applemusic-playlist ()
  "Choose a playlist and play it."
  (interactive)
  (if-let* ((choice (applemusic--pick "Playlist: "
                                      (applemusic--list "playlists"))))
      (applemusic--say "playlist" choice)
    (message "No playlist selected")))

(defun applemusic-album ()
  "Choose an album and play it."
  (interactive)
  (if-let* ((choice (applemusic--pick "Album: "
                                      (applemusic--list "albums"))))
      (applemusic--say "album" choice)
    (message "No album selected")))

(defun applemusic-search (query)
  "Search the library for QUERY, choose a result, and play it."
  (interactive "sSearch: ")
  (if-let* (((not (string-empty-p query)))
            (choice (applemusic--pick (format "Songs matching %s: " query)
                                      (applemusic--list "search" query))))
      (applemusic--say "song" choice)
    (message "Nothing selected")))

;;;; Main menu

;;;###autoload
(defun applemusic-menu ()
  "Top-level Apple Music menu."
  (interactive)
  (applemusic--menu "Apple Music"
                    '((?t "toggle"   applemusic-toggle)
                      (?i "info"     applemusic-now)
                      (?l "playlist" applemusic-playlist)
                      (?a "album"    applemusic-album)
                      (?s "search"   applemusic-search)
                      (?v "volume"   applemusic-volume)
                      (?h "shuffle"  applemusic-shuffle)
                      (?r "repeat"   applemusic-repeat))))

(provide 'applemusic)
;;; applemusic.el ends here

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
;; AirPlay needs the `am airplay' command (see the accompanying CLI patch).
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

(defun applemusic--menu-label (key label)
  "Return LABEL with KEY bracketed in place, e.g. \"[t]oggle\" or \"air[p]lay\".
If KEY does not occur in LABEL, fall back to \"[K] LABEL\"."
  (if-let* ((pos (seq-position label key)))
      (concat (substring label 0 pos)
              (format "[%c]" key)
              (substring label (1+ pos)))
    (format "[%c] %s" key label)))

(defun applemusic--menu-help (title entries)
  "Show a help window describing each of ENTRIES, titled TITLE.
Descriptions are the first line of each command's docstring."
  (with-help-window (format "*%s Help*" title)
    (princ (format "%s menu\n\n" title))
    (dolist (e entries)
      (let* ((cmd (nth 2 e))
             (doc (and (symbolp cmd) (documentation cmd t))))
        (princ (format "  %c  %-10s %s\n" (nth 0 e) (nth 1 e)
                       (if doc (car (split-string doc "\n")) "")))))))

(defun applemusic--menu (title entries)
  "Show a one-key menu named TITLE and run the chosen entry.
ENTRIES is a list of (KEY LABEL COMMAND).  COMMAND is called
interactively if it is a command, otherwise with no arguments.
Pressing ? describes each entry; \\[keyboard-quit] leaves the menu."
  (let* ((prompt (format "%s: %s  [?] help "
                         title
                         (mapconcat (lambda (e)
                                      (applemusic--menu-label (nth 0 e) (nth 1 e)))
                                    entries "  ")))
         (keys (cons ?? (mapcar #'car entries)))
         (key (read-char-choice prompt keys)))
    (while (eq key ??)
      (applemusic--menu-help title entries)
      (setq key (read-char-choice prompt keys)))
    (and-let* ((action (nth 2 (assq key entries))))
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
  (let ((prompt (format "Volume is %s.  Set to: [1-9] = 10-90, [0] = 100 "
                        (or (applemusic--current-volume) "unknown"))))
    (let ((key (read-char-choice prompt (append "0123456789" nil))))
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

(defun applemusic-song (query)
  "Search the library for QUERY, choose a song, and play it."
  (interactive "sSearch: ")
  (if-let* (((not (string-empty-p query)))
            (choice (applemusic--pick (format "Songs matching %s: " query)
                                      (applemusic--list "search" query))))
      (applemusic--say "song" choice)
    (message "Nothing selected")))

;;;; AirPlay

(defun applemusic--airplay-table (devices active)
  "Return a completion table over DEVICES, marking those in ACTIVE with a check."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (display-sort-function . identity)
          (annotation-function
           . ,(lambda (c) (if (member c active) "  ✓" ""))))
      (complete-with-action action devices string pred))))

(defun applemusic-airplay (device &optional only)
  "Choose an AirPlay output and toggle it.
Interactively, prompt for DEVICE, marking those currently receiving
audio.  With prefix argument ONLY, play to that device alone instead,
turning all other outputs off."
  (interactive
   (let* ((devices (applemusic--list "airplay"))
          (active  (applemusic--list "airplay" "--active"))
          (choice  (and devices
                        (completing-read
                         (if current-prefix-arg "AirPlay (only): " "AirPlay (toggle): ")
                         (applemusic--airplay-table devices active) nil t))))
     (unless (and choice (not (string-empty-p choice)))
       (user-error "No AirPlay device selected"))
     (list choice current-prefix-arg)))
  (if only
      (applemusic--say "airplay" device "--only")
    (applemusic--say "airplay" device)))

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
                      (?s "song"     applemusic-song)
                      (?v "volume"   applemusic-volume)
                      (?p "airplay"  applemusic-airplay)
                      (?h "shuffle"  applemusic-shuffle)
                      (?r "repeat"   applemusic-repeat))))

(provide 'applemusic)
;;; applemusic.el ends here

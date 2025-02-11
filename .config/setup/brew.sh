#!/bin/sh

brew install --formula eza fastfetch ffmpeg fswatch jackett koekeishiya/formulae/skhd lazygit less mas nano tailwindcss tlrc yt-dlp zellij zsh-autosuggestions zsh-syntax-highlighting

brew install --cask --no-quarantine aldente anki calibre chromium font-atkinson-hyperlegible font-open-dyslexic font-red-hat-mono ghostty iina itch knockknock loop maccy notunes obsidian osu proton-pass qbittorrent sanesidebuttons soulver steam utm webstorm zen-browser zotero

brew services start jackett

###############################################################################

# brew uninstall --formula 
# brew uninstall --cask --zap
# thunderbird@esr libreoffice handbrake vlc diffusionbee lunar topnotch whisky heroic
#!/bin/sh

brew install --formula duti emacs eza ffmpeg jackett koekeishiya/formulae/skhd less mas tailwindcss tlrc yt-dlp zsh-autosuggestions zsh-syntax-highlighting

brew install --cask --no-quarantine anki calibre chromium font-atkinson-hyperlegible-mono font-atkinson-hyperlegible-next font-red-hat-mono ghostty iina itch knockknock loop maccy notunes obsidian osu proton-pass qbittorrent@lt20 sanesidebuttons soulver steam utm zen-browser zotero

brew services start jackett

###############################################################################

# brew uninstall --formula
# brew uninstall --cask --zap
# thunderbird@esr libreoffice handbrake vlc diffusionbee lunar topnotch whisky heroic

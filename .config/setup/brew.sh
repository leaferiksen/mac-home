#!/bin/sh

brew tap d12frosted/emacs-plus

brew install --formula duti emacs-plus@30 eza ffmpeg jackett koekeishiya/formulae/skhd mas tailwindcss tlrc yt-dlp zsh-autosuggestions zsh-syntax-highlighting

ln -s /opt/homebrew/opt/emacs-plus@30/Emacs.app ~/Applications

brew install --cask --no-quarantine anki calibre chromium font-atkinson-hyperlegible-mono font-atkinson-hyperlegible-next font-red-hat-mono ghostty iina itch knockknock loop maccy notunes obsidian osu qbittorrent@lt20 qlvideo soulver steam utm virtualbuddy zen-browser zotero

###############################################################################

# brew services start jackett
# brew uninstall --formula
# brew uninstall --cask --zap
# thunderbird@esr libreoffice handbrake vlc diffusionbee lunar topnotch whisky heroic sanesidebuttons proton-pass

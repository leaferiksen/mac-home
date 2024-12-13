 #!/bin/sh

softwareupdate --install-rosetta --agree-to-license

brew install --formula lazygit less mas eza nano zsh-autosuggestions zsh-syntax-highlighting fswatch yt-dlp ffmpeg koekeishiya/formulae/skhd

skhd --start-service

brew install --cask --no-quarantine font-atkinson-hyperlegible font-open-dyslexic font-red-hat-mono sanesidebuttons aldente maccy notunes iina proton-pass obsidian librewolf thunderbird@esr zotero affinity-designer calibre soulver anki itch steam whisky heroic modrinth utm webstorm knockknock chromium

###############################################################################

# deno tailwindcss
# brew tap zackelia/formulae && brew install bclm
# brew tap FelixKratz/formulae && brew install borders && brew services start borders
# libreoffice handbrake vlc zen-browser porting-kit crossover diffusionbee lunar topnotch

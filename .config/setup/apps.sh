#!/bin/sh

brew tap d12frosted/emacs-plus
brew install emacs-plus@31 --with-xwidgets
cp -r /opt/homebrew/opt/emacs-plus@31/Emacs.app ~/Applications/
brew install --formula aria2 aspell cmake eslint ffmpeg fzf gemini-cli yqrashawn/goku/goku http-server libvterm media-info mupdf node pandoc prettier rustywind tailwindcss-language-server tealdeer typescript typescript-language-server typst vscode-langservers-extracted yt-dlp zsh-autosuggestions zsh-syntax-highlighting
brew install --cask anki font-maple-mono-nf-cn font-symbols-only-nerd-font ghostty hammerspoon homerow iina itch knockknock modrinth notunes osu qlmarkdown qlvideo Sikarugir-App/sikarugir/sikarugir soulver steam syncthing tor-browser virtualbuddy waterfox zotero

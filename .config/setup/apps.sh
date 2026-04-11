#!/bin/sh

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-imagemagick --with-xwidgets
cp -r /opt/homebrew/opt/emacs-plus@31/Emacs.app ~/Applications/
brew install --formula aria2 aspell cmake copilot-cli eslint ffmpeg fzf gemini-cli yqrashawn/goku/goku http-server libvterm media-info mupdf node opencode pandoc pngpaste prettier rustywind tailwindcss-language-server tealdeer typescript typescript-language-server typst vscode-langservers-extracted xcode-build-server yt-dlp zsh-autosuggestions zsh-syntax-highlighting
brew install --cask anki calibre font-atkinson-hyperlegible-next font-atkinson-hyperlegible-mono font-maple-mono-nf-cn font-symbols-only-nerd-font hammerspoon homerow iina knockknock lunar modrinth notunes osu qlmarkdown qlvideo Sikarugir-App/sikarugir/sikarugir soulver syncthing virtualbuddy waterfox zotero
npm install -g npm-check-updates

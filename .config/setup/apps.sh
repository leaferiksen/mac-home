#!/bin/sh

brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app

brew install --formula aspell ffmpeg media-info pandoc paneru pngpaste tealdeer typst yqrashawn/goku/goku yt-dlp zig zsh-autosuggestions zsh-syntax-highlighting 

paneru install
paneru start

# development
brew install --formula copilot-cli eslint gemini-cli opencode prettier tailwindcss-language-server typescript typescript-language-server vscode-langservers-extracted
# amgi
brew install --formula protobuf swift-protobuf xcodegen
# paperWM
brew install --formula busted lua-language-server

brew install --cask anki calibre font-atkinson-hyperlegible-next font-atkinson-hyperlegible-mono font-maple-mono-nf-cn font-symbols-only-nerd-font homerow iina knockknock modrinth osu qlmarkdown qlvideo Sikarugir-App/sikarugir/sikarugir soulver syncthing virtualbuddy waterfox zotero

npm install -g npm-check-updates

#!/bin/sh

brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app

brew install --formula aspell ffmpeg media-info mupdf pandoc pngpaste tealdeer typst yqrashawn/goku/goku xbzig zsh-autosuggestions zsh-syntax-highlighting

# ani-cli
git clone "https://github.com/pystardust/ani-cli.git" && cd ./ani-cli
cp ./ani-cli "$(brew --prefix)"/bin
cd .. && rm -rf ./ani-cli
brew install --formula curl grep aria2 ffmpeg fzf yt-dlp

# development
brew install --formula copilot-cli eslint gemini-cli opencode prettier tailwindcss-language-server typescript typescript-language-server vscode-langservers-extracted
# amgi
brew install --formula protobuf swift-protobuf xcodegen
# paperWM
# brew install --formula busted lua-language-server

brew install --cask anki BarutSRB/tap/omniwm calibre font-atkinson-hyperlegible-next font-atkinson-hyperlegible-mono font-maple-mono-nf-cn font-symbols-only-nerd-font homerow iina knockknock modrinth osu qlmarkdown qlvideo Sikarugir-App/sikarugir/sikarugir soulver syncthing virtualbuddy waterfox zotero

npm install -g npm-check-updates

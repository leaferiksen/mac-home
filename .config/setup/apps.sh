#!/bin/sh

brew install --formula ffmpeg harper karinushka/paneru/paneru media-info mupdf pandoc pngpaste tealdeer typst yqrashawn/goku/goku xbzig zsh-autosuggestions zsh-syntax-highlighting
paneru install && paneru start

# ani-cli
git clone "https://github.com/pystardust/ani-cli.git" && cd ./ani-cli
cp ./ani-cli "$(brew --prefix)"/bin && cd .. && rm -rf ./ani-cli
brew install --formula curl grep aria2 ffmpeg fzf yt-dlp

# web development
brew install --formula copilot-cli eslint gemini-cli opencode prettier tailwindcss-language-server typescript typescript-language-server vscode-langservers-extracted
# amgi
brew install --formula protobuf swift-protobuf xcodegen
# paperWM
# brew install --formula busted lua-language-server

brew install --cask anki calibre d12frosted/emacs-plus/emacs-plus-app font-atkinson-hyperlegible-next font-atkinson-hyperlegible-mono font-maple-mono-nf-cn font-symbols-only-nerd-font homerow iina knockknock modrinth mos@beta osu Sikarugir-App/sikarugir/sikarugir soulver syncthing virtualbuddy waterfox zotero

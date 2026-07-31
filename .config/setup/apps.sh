#!/bin/sh

brew tap d12frosted/emacs-plus
brew trust d12frosted/emacs-plus
brew install emacs-plus-app

brew install --formula atool ffmpeg harper karinushka/paneru/paneru media-info mole mupdf ollama pandoc pngpaste tealdeer typst yqrashawn/goku/goku xbzig zsh-autosuggestions zsh-syntax-highlighting
paneru install && paneru start

# ani-cli
git clone "https://github.com/pystardust/ani-cli.git" && cd ./ani-cli
cp ./ani-cli "$(brew --prefix)"/bin && cd .. && rm -rf ./ani-cli
brew install --formula curl grep aria2 ffmpeg fzf yt-dlp

# web development
brew install --formula anomalyco/tap/opencode eslint prettier tailwindcss-language-server typescript typescript-language-server vscode-langservers-extracted
brew install --cask open-design
# amgi
brew install --formula protobuf swift-protobuf xcodegen
# paperWM
# brew install --formula busted lua-language-server

brew install --cask anki calibre darrylmorley/whatcable/whatcable font-atkinson-hyperlegible-next font-atkinson-hyperlegible-mono font-maple-mono-nf-cn font-symbols-only-nerd-font homerow iina knockknock modrinth mos@beta neodisk osu soulver syncthing transmission virtualbuddy waterfox xcodes-app zotero

# Sikarugir-App/sikarugir/sikarugir

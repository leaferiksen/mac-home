#!/bin/sh

brew tap d12frosted/emacs-plus
brew trust d12frosted/emacs-plus
brew install emacs-plus-app

# general cli
brew install --formula anomalyco/tap/opencode atool cmatrix ffmpeg harper imagemagick media-info mole mupdf ollama pandoc paneru pngpaste tealdeer typst yqrashawn/goku/goku xbzig zsh-autosuggestions zsh-syntax-highlighting
paneru install && paneru start

# general apps
brew install --cask anki calibre darrylmorley/whatcable/whatcable finetune font-atkinson-hyperlegible-next font-maple-mono-cn font-symbols-only-nerd-font homerow iina knockknock modrinth mos@beta neodisk open-design osu soulver superwhisper syncthing transmission virtualbuddy zotero
brew pin font-atkinson-hyperlegible-next

# web development
npm install -g npm-check-updates typescript-language-server tailwindcss-language-server vscode-langservers-extracted

# ani-cli
git clone "https://github.com/pystardust/ani-cli.git" && cd ./ani-cli
cp ./ani-cli "$(brew --prefix)"/bin && cd .. && rm -rf ./ani-cli
brew install --formula curl grep aria2 ffmpeg fzf yt-dlp

# amgi
brew install --formula protobuf swift-protobuf xcodegen

# paperWM
# brew install --formula busted lua-language-server

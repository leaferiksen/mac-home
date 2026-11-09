#!/bin/sh

# general cli
brew install --formula anomalyco/tap/opencode-2 bun cmatrix ffmpeg gh harper imagemagick media-info mole ollama pandoc paneru pngpaste tealdeer typst yqrashawn/goku/goku yt-dlp zsh-autosuggestions zsh-syntax-highlighting
paneru install && paneru start

# general apps
brew install --cask anki calibre d12frosted/emacs-plus/emacs-plus-app finetune font-atkinson-hyperlegible-next font-maple-mono-cn font-symbols-only-nerd-font homerow iina knockknock modrinth mos@beta neodisk open-design osu reminders-menubar soulver superwhisper syncthing transmission virtualbuddy zotero
brew pin font-atkinson-hyperlegible-next

# web development
npm install -g npm-check-updates typescript-language-server tailwindcss-language-server vscode-langservers-extracted

# amgi
brew install --formula protobuf swift-protobuf xcodegen

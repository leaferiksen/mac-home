# zsh fuzzy caps autocomplete and Plugins
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source ${HOME}/Homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source ${HOME}/Homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export VISUAL="nano"
export LESSHISTFILE=-
export SHELL_SESSIONS_DISABLE=1
export PATH=${HOME}/bin:${HOME}/Homebrew/sbin:${HOME}/Homebrew/bin:$PATH
export HOMEBREW_CASK_OPTS="--appdir=~/Applications --no-quarantine"
eval "$(brew shellenv)"
autoload -Uz compinit
compinit

# named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs/"
export sf="${HOME}/Documents/College/"
export sd="/Volumes/Leafs Media"

# My fun tools
alias q='qlmanage -p'
alias fixapp='xattr -dr com.apple.quarantine'
alias upb='brew update && brew upgrade --greedy'
alias upn='ncu -u && npm install'
alias fixnode='brew unlink node && brew link --overwrite node'
alias tw='npx @tailwindcss/cli -i app.css -o dist.css --watch'

# My BSD utils
alias ctar=tar -czvf
alias xtar=tar -xzvf
alias ttar=tar -tzvf
alias diff='diff --color=always'
alias l='ls -a --color=auto'
alias ll='ls -al --color=auto'
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}

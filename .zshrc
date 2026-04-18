# zsh fuzzy caps autocomplete and Plugins
autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export VISUAL="nano"
export LESSHISTFILE=-
export SHELL_SESSIONS_DISABLE=1
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"
export PATH=$HOME/.docker/bin:/opt/homebrew/sbin:/opt/homebrew/bin:$PATH
eval "$(brew shellenv)"
. "$HOME/.cargo/env"

# named directories
export ic="/Users/leaf/Library/Mobile Documents/com~apple~CloudDocs/"
export sf="/Users/leaf/Documents/College/"
export sd="/Volumes/Leafs Media"

# My fun tools
alias q='qlmanage -p'
alias ani-cli='ani-cli -q 1080'
alias tailscale='/Applications/Tailscale.app/Contents/MacOS/Tailscale'
alias fixapp='xattr -dr com.apple.quarantine'
alias upb='brew update && brew upgrade --greedy'
alias upn='ncu -u && npm install'
alias fixnode='brew unlink node && brew link --overwrite node'

# My BSD utils
alias gitamend='git commit --amend --no-edit && git status'
alias myip='ifconfig | grep "inet "'
alias ctar='tar -czvf'
alias xtar='tar -xzvf'
alias ttar='tar -tzvf'
alias diff='diff --color=always'
alias l='ls -a --color=auto'
alias ll='ls -al --color=auto'
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}

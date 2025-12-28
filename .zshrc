# zsh fuzzy caps autocomplete and Plugins
autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source /Users/leaf/Homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /Users/leaf/Homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export VISUAL="nano"
export LESSHISTFILE=-
export SHELL_SESSIONS_DISABLE=1
export PATH=/Users/leaf/Homebrew/sbin:/Users/leaf/Homebrew/bin:/Users/leaf/.local/bin:/Users/leaf/Homebrew/opt/python@3.14/libexec/bin:/Users/leaf/.docker/bin:$PATH
export PYTHONPATH="/Users/leaf/Homebrew/opt/notmuch/lib/python3.14/site-packages/"
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"
eval "$(brew shellenv)"

# named directories
export ic="/Users/leaf/Library/Mobile Documents/com~apple~CloudDocs/"
export sf="/Users/leaf/Documents/College/"
export sd="/Volumes/Leafs Media"

# My fun tools
alias q='qlmanage -p'
alias tailscale='/Applications/Tailscale.app/Contents/MacOS/Tailscale'
alias fixapp='xattr -dr com.apple.quarantine'
alias upb='brew update && brew upgrade --greedy'
alias upn='ncu -u && npm install'
alias fixnode='brew unlink node && brew link --overwrite node'

# My BSD utils
alias ctar='tar -czvf'
alias xtar='tar -xzvf'
alias ttar='tar -tzvf'
alias diff='diff --color=always'
alias l='ls -a --color=auto'
alias ll='ls -al --color=auto'
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

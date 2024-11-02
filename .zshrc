# zsh fuzzy caps autocomplete
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# zsh Plugins
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# brew completion definitions
autoload -Uz compinit
compinit

# environment variables
## brew
export HOMEBREW_CASK_OPTS="--appdir=~/Applications  no-quarantine"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE
## fuck vim
export VISUAL='nano --modernbindings'
export EDITOR="$VISUAL"
## named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs"
export sd="/Volumes/Leafs Media"
export sf="${HOME}/Documents/Fall 24"
# aliases
alias q='qlmanage -p'
alias up='brew update && brew upgrade --no-quarantine'
alias fix='xattr -dr com.apple.quarantine'
alias nano='nano --modernbindings --softwrap --tabsize=4 --tabstospaces'
alias chat='ollama run myllama --nowordwrap'
alias lg='lazygit'

# functions
## ls: A=show hidden files h=unit suffixes o=long format without groups
function l() {
    clear -x
    echo -e "\e[31m$(pwd)\e[0m"
    ls -ho --color "$@" | less --quit-if-one-screen --RAW-CONTROL-CHARS
}
function lh() {
    clear -x
    echo -e "\e[31m$(pwd)\e[0m"
    ls -Aho --color "$@" | less --quit-if-one-screen --RAW-CONTROL-CHARS
}
function d() {
    cd "$@"
    clear -x
    echo -e "\e[31m$(pwd)\e[0m"
    ls -ho --color | less --quit-if-one-screen --RAW-CONTROL-CHARS
}
function dh() {
    cd "$"
    clear -x
    echo -e "\e[31m$(pwd)\e[0m"
    ls -Aho --color | less --quit-if-one-screen --RAW-CONTROL-CHARS
}
function md() {
    mkdir -p "$(pwd)/$@"
    la
}
function mf() {
    touch "$(pwd)/$@"
    la  
}
# Terminal startup
l

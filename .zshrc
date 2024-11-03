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
export HOMEBREW_NO_AUTO_UPDATE=1
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
## fuck vim
export VISUAL='nano --modernbindings'
export EDITOR="$VISUAL"
## named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs"
export sd="/Volumes/Leafs Media"
export sf="${HOME}/Documents/Fall 24"

# aliases
alias q='qlmanage -p'
alias lg='lazygit'
alias up='brew update; brew upgrade --formulae --no-quarantine $(brew list --formulae); brew upgrade --cask --no-quarantine --greedy $(brew list --cask | grep --invert-match --regexp=thunderbird --regexp=font-red-hat-mono)'
alias fix='xattr -dr com.apple.quarantine'
alias nano='nano --modernbindings --softwrap --tabsize=4 --tabstospaces'
alias chat='ollama run myllama --nowordwrap'

# navigation
#  | less --quit-if-one-screen --RAW-CONTROL-CHARS
## ls: A=show hidden files h=unit suffixes o=long format without groups
# alias l='echo -e "\e[31m$(pwd)\e[0m" && eza --all --group-directories-first'
function l() {
    echo -e "\e[31m$(pwd)\e[0m"
    eza --all --long --grid --group-directories-first --no-time --no-permissions --no-user "$@"
}
function d() {cd "$@" && l}
# function dl() {cd "$@" && ll}
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

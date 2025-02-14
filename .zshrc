# zsh fuzzy caps autocomplete and Plugins
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# brew completion definitions
autoload -Uz compinit
compinit

# brew
export HOMEBREW_CASK_OPTS="--appdir=~/Applications  no-quarantine"
export HOMEBREW_NO_AUTO_UPDATE=1
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"

# fuck vim
export EDITOR="emacs"
export VISUAL="emacs"

# named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs"
export sd="/Volumes/Leafs Media"
export sf="${HOME}/Documents/College/Spring 25"
## ssh-add --apple-use-keychain ~/.ssh/id_ed25519 &> /dev/null

# Mac utils
alias q='qlmanage -p'
alias fix='xattr -dr com.apple.quarantine'
alias up='brew update; brew upgrade --no-quarantine'
## copy() {osascript -e "set the clipboard to (POSIX file \"$PWD/$1\")"}
## 'brew update; brew upgrade --formulae --no-quarantine $(brew list --formulae); brew upgrade --cask --no-quarantine --greedy $(brew list --cask | grep --invert-match --regexp=thunderbird --regexp=font-red-hat-mono)'

# Dev shortcuts
alias lg='lazygit'
alias tw='tailwindcss -i app.css -o dist.css'
alias ctar=tar -czvf
alias xtar=tar -xzvf
alias ttar=tar -tzvf

# Better BSD utils
alias diff='diff --color=always'
alias l='echo -e "\e[31m$(pwd)\e[0m"; eza --long --grid --group-directories-first --no-time --no-permissions --no-user'
function la() {{echo -e "\e[31m$(pwd)\e[0m"; eza --color=always --all --long --header --group-directories-first "$@"} | less --header 2 --quit-if-one-screen --RAW-CONTROL-CHARS --SILENT --no-vbell}
function cl() {cd "$@"; l}
function cla() {cd "$@"; la}
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}
## ls: A=show hidden files h=unit suffixes o=long format without groups
## alias nano='nano --modernbindings --softwrap --tabsize=4 --tabstospaces'

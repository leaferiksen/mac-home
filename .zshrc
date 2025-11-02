# zsh fuzzy caps autocomplete and Plugins
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# brew completion definitions
autoload -Uz compinit
compinit

# disable macOS's Zsh session saving
export SHELL_SESSIONS_DISABLE=1

# environment variables
export VISUAL="nano"
export LESSHISTFILE=-
export PATH=/Users/leaf/bin:/opt/homebrew/sbin:/opt/homebrew/bin:$PATH
export HOMEBREW_CASK_OPTS="--appdir=~/Applications --no-quarantine"

# named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs/"
export sd="/Volumes/Leafs Media"
export sf="${HOME}/Documents/College/"

# My fun tools
alias q='qlmanage -p'
alias fix='xattr -dr com.apple.quarantine'
alias emacs='${HOME}/Applications/Emacs.app/Contents/MacOS/Emacs'
alias upb='brew update && brew upgrade --greedy'
alias upn='ncu -u && npm install'
alias tw='npx @tailwindcss/cli -i app.css -o dist.css --watch'
## copy() {osascript -e "set the clipboard to (POSIX file \"$PWD/$1\")"}

# My BSD utils
alias ctar=tar -czvf
alias xtar=tar -xzvf
alias ttar=tar -tzvf
alias diff='diff --color=always'
alias l='echo -e "\e[31m$(pwd)\e[0m" && eza --group-directories-first --icons'
alias la='echo -e "\e[31m$(pwd)\e[0m" && eza --all --group-directories-first --icons'
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}
## eza  --long --grid --no-time --no-permissions --no-user
## ls: A=show hidden files h=unit suffixes o=long format without groups
## less --header 2 --quit-if-one-screen --RAW-CONTROL-CHARS --SILENT --no-vbell

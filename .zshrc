# zsh fuzzy caps autocomplete and Plugins
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# brew completion definitions
autoload -Uz compinit
compinit

# disable macOS's Zsh session saving
export SHELL_SESSIONS_DISABLE=1

# editor 
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -r -a emacs"         # $VISUAL opens in GUI mode
export LESSHISTFILE=-

# brew
export PATH=/Users/leaf/bin:/opt/homebrew/sbin:/opt/homebrew/bin:\ /usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin
export HOMEBREW_CASK_OPTS="--appdir=~/Applications --no-quarantine"

# named directories
export ic="${HOME}/Library/Mobile Documents/com~apple~CloudDocs"
export sd="/Volumes/Leafs Media"
export sf="${HOME}/Documents/College/Spring 25"
## ssh-add --apple-use-keychain ~/.ssh/id_ed25519 &> /dev/null

# My fun tools
alias q='qlmanage -p'
alias fix='xattr -dr com.apple.quarantine'
alias up='brew update && brew upgrade'
alias tw='npx @tailwindcss/cli -i app.css -o dist.css --watch'
## https://eclecticlight.co/2023/10/11/launchservices-problems-in-sonoma-14-0/
## https://lapcatsoftware.com/articles/2023/10/4.html
alias resetLaunchServices='/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -v -apps u'
## copy() {osascript -e "set the clipboard to (POSIX file \"$PWD/$1\")"}
## 'brew update; brew upgrade --formulae --no-quarantine $(brew list --formulae); brew upgrade --cask --no-quarantine --greedy $(brew list --cask | grep --invert-match --regexp=thunderbird --regexp=font-red-hat-mono)'

# My BSD utils
alias ctar=tar -czvf
alias xtar=tar -xzvf
alias ttar=tar -tzvf
alias diff='diff --color=always'
alias l='eza --group-directories-first --icons'
alias la='eza --all --group-directories-first --icons'
alias ll='eza --all --long --header --group-directories-first --git --icons --no-user --no-permissions'
function ml() {mkdir -p "$(pwd)/$@"; l}
function tl() {touch "$(pwd)/$@"; l}
## echo -e "\e[31m$(pwd)\e[0m";
## eza  --long --grid --no-time --no-permissions --no-user
## ls: A=show hidden files h=unit suffixes o=long format without groups
## less --header 2 --quit-if-one-screen --RAW-CONTROL-CHARS --SILENT --no-vbell
## alias nano='nano --modernbindings --softwrap --tabsize=4 --tabstospaces'

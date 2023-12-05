# Set the default umask for new files
umask 022

# turn off case sensitive glob
setopt NO_CASE_GLOB

# automatic CD
setopt AUTO_CD

# Set up system parameters
export PATH=$HOME/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:$PATH
export HOMEBREW_PREFIX="/opt/homebrew";
export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
export HOMEBREW_REPOSITORY="/opt/homebrew";
export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
export PATH=$PATH:/Users/stott/.spicetify
export BAT_THEME="Monokai Extended"
export EDITOR=/Users/stott/bin/emc
export HOMEBREW_BREWFILE=~/.brewfile

# set up history
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
# provide timestamp
setopt EXTENDED_HISTORY
# ensure defaults big
SAVEHIST=5000
HISTSIZE=2000
# share across multiple zsh sessions
setopt SHARE_HISTORY
# append to one history file
setopt APPEND_HISTORY
# save history as commands executed
setopt INC_APPEND_HISTORY
# do not store duplicates
setopt HIST_IGNORE_DUPS
# remove blank lines
setopt HIST_REDUCE_BLANKS
# set so can edit history search results before submitting
setopt HIST_VERIFY
# zsh correction
setopt CORRECT
setopt CORRECT_ALL

# Aliases
alias ls='/bin/ls -Gh'
alias la='/bin/ls -Ga'
alias ll='/bin/ls -alFG'
alias lsd='/bin/ls -Gh -d */'
alias ps='ps -a'

alias -g reload='source ~/.zshrc'
alias -g grep='grep -i --color'
# take out grep command and then pull search term $1 from remaining
alias -g G='| grep -v "grep -i --color" | grep $1'

# Set up brew
alias ibrew='arch -x86_64 /usr/local/bin/brew'
alias brewit='brew update && noti brew upgrade'
# Reinstall all key apps using ~/.brewfile
alias brewbundle='brew bundle --file="~/.brewfile"'

# Fix Open With in OSX
alias fixow='/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local -domain user;killall Finder;echo "Open With has been rebuilt, Finder will relaunch"'

# Clean DNS Cache
alias dnsclear='sudo killall -HUP mDNSResponder'

alias myip="curl ipinfo.io"

# set up 1Password to use fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /Users/stott/.config/op/plugins.sh

# Set up tool to manage history
eval "$(mcfly init zsh)"

# Set up easy management of config files - see https://www.atlassian.com/git/tutorials/dotfiles
alias myconfig='/usr/bin/git --git-dir=/Users/stott/.myconfig/ --work-tree=/Users/stott'

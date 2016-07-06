# Path to your oh-my-zsh installation.
export ZSH=/Users/dsargeant/.oh-my-zsh
# Theme
ZSH_THEME="agnoster"
DEFAULT_USER=dsargeant
# Plugins
plugins=(git aws brew)
# Path
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/dsargeant/bin:/usr/local/sbin:/usr/local/go/bin"

source $ZSH/oh-my-zsh.sh

export PROJ=~/Projects
export BROWSER="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Aliases
alias magit="emacs -nw --load='~/.emacs.d/magit-init.el' --no-splash"
alias v=nvim
alias e="emacs -nw"
alias dirs="dirs -v"
# Paging via Vim
export MANPAGER="vim -c MANPAGER -"
export PAGER="vim -c PAGER -"
# Python
export PYTHONPATH="/Library/Python/2.7/site-packages:$PYTHONPATH"
# Go
export GOPATH=$HOME/Go
export PATH=$PATH:$GOPATH/bin
# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND="ag -U --hidden -g ''"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# Artifact authentication
source ~/.boomtown
# z (nice autocompletion of frequently used directories)
source ~/Projects/z/z.sh

# Change projects
proj() {
  cd $PROJ/$1
}

# Change to root directory of current git repo
groot() {
  git branch > /dev/null 2>&1 || return 1
  cd "$(git rev-parse --show-cdup)".
  pwd
}

# Directory history
dh() {
  local dir
  dir=$(z | sed 's/^[0-9.]*[[:space:]]*//' | fzf --tac --no-sort)
  cd "$dir"
}

# Find directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf)
  cd "$dir"
}

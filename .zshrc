# Path to your oh-my-zsh installation.
export ZSH=/Users/dsargeant/.oh-my-zsh
# Theme
ZSH_THEME="david"
# DEFAULT_USER=dsargeant
# Plugins
plugins=(aws brew docker)
# Path
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/dsargeant/bin:/usr/local/sbin

source $ZSH/oh-my-zsh.sh

export PROJ=~/Projects
export BROWSER="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Aliases
alias c=clear
alias g=git
alias magit="emacs -nw --load='~/.emacs.d/magit-init.el' --no-splash"
alias v=nvim
alias e="emacs -nw"
alias reload="source ~/.zshrc"
alias sed="sed -E"
# Paging via Vim
export MANPAGER="vim -c MANPAGER -"
export PAGER="vim -c PAGER -"
# Go
export GOPATH=$HOME/Go
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin
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
gr() {
  cd "$(git rev-parse --show-cdup)"
}

# Open the current repo in github
gh() {
  open "$(git config remote.origin.url | sed 's/\.git$//')"
}

# Directory history
dh() {
  local dir
  dir=$(z | sed 's/^[0-9.]*[[:space:]]*//' | fzf --tac --no-sort)
  cd "$dir"
}

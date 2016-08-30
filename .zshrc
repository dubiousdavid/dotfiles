# Path to your oh-my-zsh installation.
export ZSH=/Users/dsargeant/.oh-my-zsh
# Theme
ZSH_THEME="david"
# Plugins
plugins=(urltools aws brew docker)
# zsh-autosuggestions
# Path
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/dsargeant/bin:/usr/local/sbin

source $ZSH/oh-my-zsh.sh

# Autosuggest accept
# ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(end-of-line vi-forward-char vi-end-of-line vi-add-eol)

export EDITOR=nvim
export PROJ=~/Projects
export BROWSER="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Aliases
alias c=clear
alias g=git
alias magit="emacs -nw --load='~/.emacs.d/magit-init.el' --no-splash"
# alias magit="emacsclient -nw -e '(magit-status-window)'"
alias v=nvim
alias e="emacs -nw"
# alias e="emacsclient -nw -a ''"
alias dk=docker
alias reload="source ~/.zshrc"
alias sed="sed -E"
alias qa="AWS_PROFILE=qa"
alias maurice="git log --after='yesterday' --author='Maurice'"
alias ports="lsof -iTCP -sTCP:LISTEN -n -P"
alias b=bible
join() { local IFS="$1"; shift; echo "$*"; }
port() {
  if [ -z "$@" ]; then
    echo "Usage: port [port # ...]"
  else
    lsof -iTCP:$(join , "$@") -sTCP:LISTEN -n -P
  fi
}
kill-port() {
  if [ -z "$@" ]; then
    echo "Usage: kill-port [port # ...]"
  else
    port "$@" | awk 'NR!=1 {print $2}' | xargs kill
  fi
}
# Paging via Vim
export MANPAGER="nvim -c MANPAGER -"
export PAGER="nvim -c PAGER -"
# Go
export GOPATH=$PROJ/Go
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

bible() {
  if [ -z "$*" ]; then
    http --body "http://www.esvapi.org/v2/rest/dailyVerse?key=IP&output-format=plain-text"
  else
    local passage
    passage=$(urlencode "$*")
    http --body "http://www.esvapi.org/v2/rest/passageQuery?key=IP&output-format=plain-text&passage=$passage"
  fi
}

# Edit an rc file
rc(){
  case $1 in
    zsh) v ~/.zshrc ;;
    vim) v ~/.config/nvim/init.vim ;;
    emacs) e ~/.emacs.d/init.el ;;
    clojure) e ~/.lein/profiles.clj ;;
  esac
}

findContainerId() {
  docker ps --filter ancestor=$1 | awk 'NR!=1 {print $1}'
}

pull-docs() {
  local containerId
  containerId=$(findContainerId boomtown/api-docs)
  [ $containerId ] && echo "Stopping api-docs ($containerId)" && docker stop $containerId 1>/dev/null
  cd "$PROJ/api-docs"
  git pull origin master
  docker build -t boomtown/api-docs .
  api-docs
}

api-docs() {
  local containerId
  containerId=$(findContainerId boomtown/api-docs)

  if [[ -z $containerId ]]; then
    echo "Starting api-docs..."
    docker run -d -p 4567:4567 boomtown/api-docs
  else
    echo "api-docs already running ($containerId)"
  fi
}

redis() {
  local containerId
  containerId=$(findContainerId redis)

  if [[ -z $containerId ]]; then
    echo "Starting redis..."
    docker run -d -p 6379:6379 redis
  else
    echo "redis already running ($containerId)"
  fi
}

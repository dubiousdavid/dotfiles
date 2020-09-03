# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# Theme
ZSH_THEME="david"
# Path
export PATH=$HOME/bin:/usr/local/opt/python/libexec/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin
# zsh
source $ZSH/oh-my-zsh.sh

export EDITOR=vim
export PROJ=~/Projects
export BROWSER="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Aliases
alias g=git
alias v=vim
alias reload="source ~/.zshrc"
alias sed="sed -E"
alias ports="lsof -iTCP -sTCP:LISTEN -n -P"
alias jpp="jq '.'"
alias sp="cd $PROJ/SmartProcure"
alias cat="bat"
alias find="fd"
alias branches="git for-each-ref --count=10 --sort=-committerdate refs/heads/ --format='%(refname:short)' | fzf | xargs git checkout"
# Paging via Vim
export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""
# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --no-ignore-vcs --hidden --files'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export NODE_PATH=`npm root -g`
# z (nice autocompletion of frequently used directories)
source ~/Projects/z/z.sh

join() { local IFS="$1"; shift; echo "$*"; }

# Print what's running on one or more ports
port() {
  if [ -z "$@" ]; then
    echo "Usage: port [port # ...]"
  else
    lsof -iTCP:$(join , "$@") -sTCP:LISTEN -n -P
  fi
}

# Kill one or more ports
port-kill() {
  if [ -z "$@" ]; then
    echo "Usage: port-kill [port # ...]"
  else
    port "$@" | awk 'NR!=1 {print $2}' | xargs kill
  fi
}

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

# Edit an rc file
rc(){
  case $1 in
    zsh) v ~/.zshrc ;;
    vim) v ~/.vimrc ;;
    emacs) e ~/.emacs.d/init.el ;;
    clojure) v ~/.lein/profiles.clj ;;
  esac
}

docker-clean-images() {
  docker rmi "$(docker images -a --filter=dangling=true -q)"
}

docker-clean-ps() {
  docker rm "$(docker ps --filter=status=exited --filter=status=created -q)"
}

findContainerId() {
  docker ps --filter ancestor=$1 | awk 'NR!=1 {print $1}'
}

# Print the absolute path of the given files
path() { for f in "$@"; do echo ${f}(:A); done }

myip() { ifconfig | grep inet }

current-branch() {
  git rev-parse --abbrev-ref HEAD
}

gup() {
  git push -u origin "$(current-branch)"
}

contains-branch() {
  local branch=$1

  git log --oneline | grep $(git show-branch --sha1-name $branch | sed 's/\[(.*)\].*/\1/')
}

show-tabs() {
  grep $'\t' $1
}

serve() {
  if [[ -z $1 ]]; then
    browser-sync start --server
  else
    browser-sync start --server --files "$1"
  fi
}

reverse() {
  echo $1 | rev
}

# Search with rip grep
s() {
  rg -M 125 -p -S --no-ignore-vcs --hidden "$@" | less -XFR
}

rm-swp() {
  find . -type f -name "*.sw[klmnop]" -delete
}

py() {
  case $1 in
    2) ln -s /usr/bin/python $HOME/bin/python ;;
    3) rm $HOME/bin/python ;;
  esac
}

# Host, keys
redis-del-bulk() {
  redis-cli -h "$1" --raw keys "$2" | xargs redis-cli -h "$1" del
}

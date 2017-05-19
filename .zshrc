# Path to your oh-my-zsh installation.
export ZSH=/Users/dsargeant/.oh-my-zsh
# Theme
ZSH_THEME="david"
# Plugins
plugins=(urltools aws docker)
# Path
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/dsargeant/bin:/usr/local/sbin
export NODE_PATH=/usr/local/lib/node_modules

source $ZSH/oh-my-zsh.sh

export EDITOR=nvim
export PROJ=~/Projects
export BROWSER="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Aliases
alias c=clear
alias g=git
alias magit="emacs -nw --load='~/.emacs.d/magit-init.el' --no-splash"
alias v=nvim
alias e="emacs -nw"
alias dk=docker
alias reload="source ~/.zshrc"
alias sed="sed -E"
alias qa="AWS_PROFILE=qa"
alias ports="lsof -iTCP -sTCP:LISTEN -n -P"
alias b=bible
alias jpp="jq '.'"
alias saml="python $PROJ/BoomTownROI/saml-setup.py"
# Paging via Vim
export MANPAGER="nvim -c 'set ft=man' -"
# export PAGER="nvim -c PAGER -"
# Go
export GOPATH=$PROJ/Go
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin
# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND="ag -f -U --hidden -g ''"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# export FZF_DEFAULT_OPTS="--reverse"
# Private Boomtown commands
source ~/Projects/boomtown.sh
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
kill-port() {
  if [ -z "$@" ]; then
    echo "Usage: kill-port [port # ...]"
  else
    port "$@" | awk 'NR!=1 {print $2}' | xargs kill
  fi
}
#
# Change projects
proj() {
  cd $PROJ/$1
}

bt() {
  proj BoomTownROI
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

# Search the Bible
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

docker-clean-images() {
  docker rmi "$(docker images -a --filter=dangling=true -q)"
}

docker-clean-ps() {
  docker rm "$(docker ps --filter=status=exited --filter=status=created -q)"
}

findContainerId() {
  docker ps --filter ancestor=$1 | awk 'NR!=1 {print $1}'
}

# Build API docs
build-docs() {
  local containerId
  containerId=$(findContainerId boomtown/api-docs)
  [ $containerId ] && echo "Stopping api-docs ($containerId)" && docker stop $containerId 1>/dev/null
  cd "$PROJ/BoomTownROI/api-docs"
  docker build -t boomtown/api-docs .
  api-docs
}

# Start API docs
start-docs() {
  local containerId
  containerId=$(findContainerId boomtown/api-docs)

  if [[ -z $containerId ]]; then
    echo "Starting api-docs..."
    docker run -d -p 4567:4567 -v $PROJ/BoomTownROI/api-docs/source:/app/source boomtown/api-docs
  else
    echo "api-docs already running ($containerId)"
  fi
}

# Open API docs
open-docs() {
  open "http://localhost:4567"
}

docs() {
  case $1 in
    open) open-docs ;;
    build) build-docs ;;
    start) start-docs ;;
    *) start-docs ;;
  esac
}

# Start Redis
redis-start() {
  local containerId
  containerId=$(findContainerId redis)

  if [[ -z $containerId ]]; then
    echo "Starting redis..."
    docker run -d -p 6379:6379 -v ~/data/redis:/data redis redis-server --appendonly yes
  else
    echo "redis already running ($containerId)"
  fi
}

# Stop redis
redis-stop() {
  local containerId
  containerId=$(findContainerId redis)
  docker kill $containerId
}

redis() {
  case $1 in
    stop) redis-stop ;;
    restart)
      redis-stop
      redis-start
      ;;
    *) redis-start ;;
  esac
}

truncate-alerts-visible() {
  aws dynamodb scan --table-name UserAlertsVisible |
  jq -c '.Items[] | {userId: .userId, userAlertId: .userAlertId}' |
  sed "s/.*/'&'/" |
  xargs -L1 aws dynamodb delete-item --table-name UserAlertsVisible --key
}

truncate-insights-visible() {
  aws dynamodb scan --table-name InsightsVisible |
  jq -c '.Items[] | {userId: .userId, insightId: .insightId}' |
  sed "s/.*/'&'/" |
  xargs -L1 aws dynamodb delete-item --table-name InsightsVisible --key
}

sbt-local() {
  sbt publishLocal "-DLIB_VERSION=$1"
}

# Print the absolute path of the given files
path() { for f in "$@"; do echo ${f}(:A); done }

myip() { ifconfig | grep inet }

current-branch() {
  git rev-parse --abbrev-ref HEAD
}

push-current() {
  git push -u origin "$(current-branch)"
}

contains-branch() {
  local branch=$1

  git log --oneline | grep $(git show-branch --sha1-name $branch | sed 's/\[(.*)\].*/\1/')
}

show-tabs() {
  grep $'\t' $1
}

endpoint() {
  local env=$1
  gatewayIds=($(aws apigateway get-rest-apis --region us-east-1 --query "items[?contains(name, '$env') == \`true\`].id" --output text))
  echo "https://$gatewayIds.execute-api.us-east-1.amazonaws.com/V1"
}

serve() {
  if [[ -z $1 ]]; then
    browser-sync start --server
  else
    browser-sync start --server --files "$1"
  fi
}

mongo() {
  docker run -d -p 27017:27017 mongo
}

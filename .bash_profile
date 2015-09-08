# Default editor
EDITOR=vim

# Exports
export AWS_ACCESS_KEY_ID=
export AWS_SECRET_ACCESS_KEY=
export LEIN_JVM_OPTS="-Dapple.awt.UIElement=true"
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
export PROJ=/Users/dsargeant/Projects

# Path
export PATH=$PATH:~/bin

# Aliases
alias ls="ls -G"
alias ll="ls -lhaG"
alias grep="grep --color=auto"
alias gs="git status"
alias ga="git add"
alias gco="git checkout"
alias e="emacs --no-splash"
alias emacsd="emacs --daemon"
alias magit="emacs --load='~/.emacs.d/magit-init.el' --no-splash"
alias google="open -a 'Google Chrome'"
alias ff="open -a 'Firefox'"
alias ack="ack -s --smart-case"
alias datez="date -u '+%Y-%m-%dT%H:%M:%SZ'"
alias ebp="vim ~/.bash_profile"

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

export PS1="\[\033[30;1m\]\u:\[\033[00m\]\[\033[35m\]\W\[\033[00m\]\[\033[34;1m\]\$(parse_git_branch)\[\033[00m\]$ "
export LSCOLORS=fxFxCxDxBxegedabagaced

# Change projects

function proj {
    cd $PROJ/$1
}

# Auto-complete for proj

proj_dir () {
  local cur=${COMP_WORDS[COMP_CWORD]}
  local prev=${COMP_WORDS[COMP_CWORD-1]}
  if [ $COMP_CWORD -eq 1 ]; then
      COMPREPLY=( $( compgen -W "$(cd "$PROJ" && find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)" -- $cur ) )
  elif [ $COMP_CWORD -eq 2 ]; then
      COMPREPLY=( $( compgen -W "$(cd "$PROJ/$prev" && find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)" -- $cur ) )
  fi
  return 0
}

complete -F proj_dir proj

# Emacs client

function em {
    local op

    if [ -z $1 ]; then
        op="."
    else
        op="$1"

        if [[ ! -e $op ]]; then
            echo -n "$op does not exists, create (y/N)? " && read create;

            if [[ -n $create && $create = "y" ]]; then
                touch $op
            else
                return 1
            fi
        fi
    fi

    /usr/local/bin/emacsclient -t $op
}

# Git root

function gr {
     ## If the current working directory is inside of
     ## a git repository, this function will change
     ## it to the git root (ie, the directory that
     ## contains the .git/ directory), and then print
     ## the new directory.
     git branch > /dev/null 2>&1 || return 1
     cd "$(git rev-parse --show-cdup)".
     pwd
 }

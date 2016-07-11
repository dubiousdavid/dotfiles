PROMPT=$'%{$fg_bold[blue]%}%n@%m %{$reset_color%}%{$terminfo[bold]$fg[yellow]%}%~%{$reset_color%} $(git_prompt_info)\
%{$fg[white]%}->%{$fg_bold[white]%} %#%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$terminfo[bold]$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}*%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

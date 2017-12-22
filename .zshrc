# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*'
zstyle :compinstall filename '/c/Users/Kamil/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install

function git_prompt_data {
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        if git status --porcelain | grep "^[?M]" > /dev/null; then
            local c="red"
        elif git status --porcelain | grep "^[A]" > /dev/null; then
            local c="yellow"
        else
            local c="green"
        fi
        echo "(%F{$c}$(git rev-parse --abbrev-ref HEAD)%f)"
    fi
}

setopt promptsubst

export PROMPT='[%F{yellow}%n%f@%F{red}%m%f] %F{blue}MSYS%f %F{cyan}%~%f $(git_prompt_data)
%B$%b '

alias cls=clear
alias la="ls -al"
alias ll="ls -l"

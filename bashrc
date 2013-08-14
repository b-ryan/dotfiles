# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#################
# Shell options #
#################

# For an explanation of any of the following options, see
# http://wiki.bash-hackers.org/internals/shell_options

shopt -s autocd
shopt -s checkwinsize
shopt -s histappend
shopt -s lithist

########################################################
# History configuration (See bash(1) for more options) #
########################################################

# don't put duplicate lines in the history
HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

####################################
# Other options and configurations #
####################################

RED="\e[1;31m"
GREY="\e[1;36m"
GREEN="\e[0;32m"
BLUE="\e[0;34m"
WHITE="\e[0m"

ps1_git() {
    local branch
    local status
    local color
    local numChanged
    local numUntracked
    local changed
    local untracked
    local stashed

    branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if [ "$branch" ]; then
        status=$(git status --short)

        color=$GREY
        numChanged=$(grep '^ M' <<< "$status" | wc -l)
        numUntracked=$(grep '^??' <<< "$status" | wc -l)
        numStashed=$(git stash list | grep "on $branch" | wc -l)

        [ $numChanged -gt 0 ] && {
            changed=" ~${numChanged}"
            color=$RED
        }
        [ $numUntracked -gt 0 ] && {
            untracked=" +$numUntracked"
        }
        [ $numStashed -gt 0 ] && {
            stashed=" {$numStashed}"
        }
        echo " \[$color\]git:[$branch$changed$untracked$stashed]"
    fi
}
ps1_hg() {
    local branch
    local status
    local color
    local numChanged
    local numUntracked
    local changed
    local untracked

    branch=$(hg branch 2> /dev/null)
    if [ "$branch" ]; then
        status=$(hg status)

        color=$GREY
        numChanged=$(grep '^M' <<< "$status" | wc -l)
        numUntracked=$(grep '^?' <<< "$status" | wc -l)

        [ $numChanged -gt 0 ] && {
            changed=" ~${numChanged}"
            color=$RED
        }
        [ $numUntracked -gt 0 ] && {
            untracked=" +$numUntracked"
        }
        echo " \[$color\]hg:[$branch$changed$untracked]"
    fi
}
ps1_update() {
    [[ "$VIRTUAL_ENV" ]] && local env="($(basename $VIRTUAL_ENV))"
    PS1="$env\[$GREEN\]\u@\h \[$BLUE\]\w$(ps1_git)$(ps1_hg)\[$WHITE\] \\\$ "
}
case $TERM in
    xterm)
        export PROMPT_COMMAND="ps1_update"
        ;;
    *)
        export PS1="\u@\h \w \$ "
        ;;
esac

export EDITOR=vim

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

[ -r ~/.dircolors ] && DIRCOLORS=~/.dircolors
eval "$(dircolors -b $DIRCOLORS)"

# Allow the up/down arrows to search commands backward and forward. See:
# http://www.reddit.com/r/linux/comments/mi80x/give_me_that_one_command_you_wish_you_knew_years/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# Alias definitions and local configuration options
[ -f ~/.bash_aliases ] && . ~/.bash_aliases
[ -f ~/.bashrc.local ] && . ~/.bashrc.local

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -z "$BASH_COMPLETION" ] && [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

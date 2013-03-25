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

PS1_git_color() {
    if git status 2> /dev/null | grep 'Changed' &> /dev/null; then
        echo -e "\e[1;31m"
    else
        echo -e "\e[1;36m"
    fi
}
PS1_git_part() {
    branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if [ "$branch" ]; then
        status=$(git status -s)
        numChanged=$(echo $status | grep '^ M' | wc -l)
        numUntracked=$(echo $status | grep '^??' | wc -l)
        [ $numChanged -gt 0 ] && changed=" ${numChanged}"
        [ $numUntracked -gt 0 ] && untracked=" +$numUntracked"
        echo " [$branch$changed$untracked]"
    else
        echo ""
    fi
}
export PS1="\[\e[01;32m\]\u@\h\[\$(PS1_git_color)\]\$(PS1_git_part) \[\e[01;34m\]\w \$\[\e[0m\] "

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

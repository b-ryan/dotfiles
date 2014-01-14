# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#################
# Shell options #
#################

# For an explanation of any of the following options, see
# http://wiki.bash-hackers.org/internals/shell_options

shopt -s checkwinsize
shopt -s histappend
shopt -s lithist

########################################################
# History configuration (See bash(1) for more options) #
########################################################

HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

####################################
# Other options and configurations #
####################################

export EDITOR=vim

[ -r ~/.dircolors ] && DIRCOLORS=~/.dircolors
eval "$(dircolors -b $DIRCOLORS)"

bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

[ -f ~/.bash_ps1     ] && . ~/.bash_ps1
[ -f ~/.bash_aliases ] && . ~/.bash_aliases
[ -f ~/.bashrc.local ] && . ~/.bashrc.local

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -z "$BASH_COMPLETION" ] && [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

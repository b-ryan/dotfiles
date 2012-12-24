# If not running interactively, don't do anything
[ -z "$PS1" ] && return

################
# Bash options #
################

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

_update_ps1() { export PS1="$(powerline-bash.py --cwd-only $?)"; }
PROMPT_COMMAND="_update_ps1"

EDITOR=vim

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

[ -r ~/.dircolors ] && DIRCOLORS=~/.dircolors
eval "$(dircolors -b $DIRCOLORS)"

# Allow the up/down arrows to search commands backward and forward
# From http://www.reddit.com/r/linux/comments/mi80x/give_me_that_one_command_you_wish_you_knew_years/
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

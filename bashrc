# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# if [ "$color_prompt" = yes ]; then
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
# fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
# case "$TERM" in
# xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#     ;;
# *)
#     ;;
# esac

PS1='\[\e[0;34m\]\u@\h\[\e[m\]:\[\e[1;35m\]\W\[\e[0;33m\]$(__git_ps1 " (%s)")\[\e[0m\]\$ '
# PS1 explanation:
# \[ - beginning of non-printing sequence; ie. characters or sequences that are not shown (like colors)
# \e[ - beginning of color sequence
# 0;34m - color code for blue
# \] - end of non-printing sequence
# \t - HH:MM:SS
# \h - hostname
# ...
# \e[m - end of color sequence
# ...
# 1;35m - dark purple
# ...
# \W - basename of the current working directory
# $(__git_ps1 " (%s)") - print current branch if within git repository
# 0m - no color
# \$ - $ for regular users, # for root

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# custom function to create directory and immediately cd into it
function mkcd() {
  [ -n "$1" ] && mkdir -p "$@" && cd "$1";
}

# aliases

alias bam!=bam
alias ping="ping -c 4"

# gvim was giving a warning as described in the following link,
# which is where the fix came from
# https://bugs.launchpad.net/ubuntu/+source/vim/+bug/771810
alias gvim="gvim -f"

# http://www.centerkey.com/tree/
alias ls_dirs="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"

# color diff
function d() { colordiff.pl "$@" | less -R  ; }

# vim shortcuts
function g() { gvim "$@" & }
alias v="vim"

# grep awesomeness
alias perg="egrep --exclude-dir=\*.git --exclude=tags -r"

# Allow the up/down arrows to search commands backward and forward
# From http://www.reddit.com/r/linux/comments/mi80x/give_me_that_one_command_you_wish_you_knew_years/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# Android SDK Path variable
export PATH=${PATH}:/usr/local/share/android-sdk/tools
export PATH=${PATH}:/usr/local/share/android-sdk/platform-tools

export EDITOR=vim

# Goofing around
alias dc="echo \"dc? I think you meant cd.  Here, let me take care of that for you.
Oh I'm wrong?  Just \\\"cd -\\\" and use the real dc (/usr/bin/dc).\" ; cd"
alias cd..="echo \"I think you meant 'cd ..'.  Here, let me take care of that for you.\"; cd .."

# use lh to ls *only* hidden files
alias lh='ls -a | egrep "^\."'

# git commands
alias pl-br="git pull origin \`git br\`"
alias ps-br="git push origin \`git br\`"

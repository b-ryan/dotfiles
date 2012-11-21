#!/bin/bash
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

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

# Goofing around
alias dc="echo \"dc? I think you meant cd.  Here, let me take care of that for you.
Oh I'm wrong?  Just \\\"cd -\\\" and use the real dc (/usr/bin/dc).\" ; cd"
alias cd..="echo \"I think you meant 'cd ..'.  Here, let me take care of that for you.\"; cd .."

# use lh to ls *only* hidden files
alias lh='ls -a | egrep "^\."'

alias gist="git status"
alias dev="git checkout develop"
alias mas="git checkout master"
alias c-="git checkout -"

# custom function to create directory and immediately cd into it
function mkcd() {
  [ -n "$1" ] && mkdir -p "$@" && cd "$1";
}

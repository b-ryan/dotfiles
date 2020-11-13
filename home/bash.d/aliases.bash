#!/bin/bash

# grep aliases
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

echo-do() {
  # Print a command and then run it.
    echo "$@" 1>&2
    "$@"
}

# ls aliases
[[ "$(uname)" = "Darwin" ]] || alias ls='ls --color=auto'
alias l='ls -CF' # show in columns and add file indicators
alias ll='ls -la' # show all files in long format
alias la='ls -A' # show almost all entries (exclude . and ..)
alias lh='ls -a | egrep "^\."' # ONLY show hidden files

# git aliases
alias gist="git status --short" # use --short, not --porcelain. porcelain doesn't do coloring
alias g="git status --short" # use --short, not --porcelain. porcelain doesn't do coloring
alias c-="git checkout -"
alias b="git for-each-ref --sort=-committerdate refs/heads/"
alias giff="git diff --color-words --ignore-space-change"
alias yep="git push -u origin \$(git branch | grep '\*' | awk '{print \$2}')"
alias p="git push"
alias d="git diff"
alias prune="git remote prune origin"
alias merged="git branch --merged"

dmerged() {
    for branch in $(merged | grep -v '^\*' | grep -v 'develop\|master'); do
        git branch -d $branch
    done
    prune
}

short() { N=${1:-1}; git log -n $N --first-parent; }
long() { git log --first-parent; }

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias lmk='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ping="ping -c 4"

rmswp() {
    ext=swp
    [ $1 ] && ext=$1
    find . -name \*.$ext -exec rm {} \;
}

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

alias tml="tmux list-sessions | column -t"
ta() {
    if (( $# > 0 )); then
        tmux attach -t "$@"
    else
        tmux attach -t "${PWD##*/}" || tmux attach
    fi
}
tn() {
    if (( $# > 0 )); then
        tmux new-session -s "$@"
    else
        if [[ -f .tmux-session-name ]]; then
            tmux new-session -s "$(cat .tmux-session-name)"
        else
            tmux new-session -s "${PWD##*/}"
        fi
    fi
}

alias PR="hub pull-request"

alias workonthis='workon "${PWD##*/}"'
mkthis() {
    python3 -m venv venv && {
        source venv/bin/activate
        if [[ -f setup.py ]]; then python setup.py develop; fi
        if [[ -f requirements.txt ]]; then pip install -r requirements.txt; fi
        if [[ -f requirements-dev.txt ]]; then pip install -r requirements-dev.txt; fi
    } || {
        return 1
    }
    pip install ipython jedi pylint
}
rmthis() {
    deactivate
    rmvirtualenv "${PWD##*/}"
}
alias refreshenv='rmthis && mkthis'

ggrepsed() {
    local grep="$1"
    local replace="$2"
    local inplace="${3:-}"
    local opts="${4:-g}"
    git grep -l "$grep" | xargs sed "$inplace" "s/$grep/$replace/$opts"
}

alias mux=tmuxinator

alias dc=docker-compose

mkcd() {
    mkdir -p "$1" && cd "$1"
}

polecat-reverse() {
    ssh -T -R $1:4000:$2:$3 anon@ssh.polecat.io
}

t() {
    if (( $# == 0 )); then
        task list
    else
        task "$@"
    fi
}

alias aws-kubectl="kubectl --context aws"

alias awk1="awk '{print \$1}'"

alias agenda='gcalcli agenda --details end 00:00 17:00'

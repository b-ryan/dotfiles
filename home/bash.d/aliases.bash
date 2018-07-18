#!/bin/bash

# grep aliases
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

echo-do() {
  # Print a command and then run it.
    local cmd

    echo "$@" 1>&2
    "$@"
}

perg() {
  # Wrapper around grep. Excludes a bunch of files and
  # directories that don't need to be searched.
  # Also allows you to specify "+" as the last argument
  # which will cause the grep command to be printed to
  # the screen before it is executed.
    local args
    local pre

    args=( "$@" )
    pre=""

    [[ "${*:$#}" == "+" ]] && {
      args=( "${*:1:$#-1}" )
      pre="echo-do "
    }

    [[ ${#args[@]} -eq 1 ]] && {
        args=( "$1" '.' )
    }

    ${pre}egrep -Ir \
        --color=auto \
        --exclude=tags \
        --exclude-dir=.git \
        --exclude-dir=build \
        --exclude-dir=Framework \
        --exclude-dir=vendor \
        --exclude=*min.js \
        "${args[@]}"
}
alias gr=perg

gw() {
    perg "\<$1\>" "${*:2}"
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
alias prune="git remote prune origin"
alias merged="git branch --merged"

dmerged() {
    for branch in $(merged | grep -v '^\*'); do
        git branch -d $branch
    done
    prune
}

short() { N=${1:-1}; git log -n $N --first-parent; }
long() { git log --first-parent; }

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ping="ping -c 4"

rmswp() {
    ext=swp
    [ $1 ] && ext=$1
    find . -name \*.$ext -exec rm {} \;
}

curl() {
    /usr/bin/curl "$@"; echo
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
    mkvirtualenv -p $(which python3) "${PWD##*/}" && {
        if [[ -f setup.py ]]; then python setup.py develop; fi
        if [[ -f requirements.txt ]]; then pip install -r requirements.txt; fi
        if [[ -f requirements-dev.txt ]]; then pip install -r requirements-dev.txt; fi
    }
    pip install ipython jedi pylint
}
rmthis() {
    deactivate
    rmvirtualenv "${PWD##*/}"
}
alias refreshenv='rmthis && mkthis'

alias target-stitch='/home/buck/.virtualenvs/target-stitch/bin/target-stitch'
alias stitch-orchestrator='/home/buck/.virtualenvs/stitch-orchestrator/bin/stitch-orchestrator'
alias tap-xero='/home/buck/.virtualenvs/tap-xero/bin/tap-xero'
alias singer-infer-schema='/home/buck/.virtualenvs/singer-tools/bin/singer-infer-schema'
alias singer-check-tap='/home/buck/.virtualenvs/singer-tools/bin/singer-check-tap'
alias singer-diff-jsonl='/home/buck/.virtualenvs/singer-tools/bin/diff-jsonl'

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
complete -o default -F __start_kubectl aws-kubectl

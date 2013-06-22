set PATH $PATH ~/bin

set --export INSTALL_ENV vm
set --export EMAIL fryan@rjmetrics.com
set --export CONFIRM y

alias ls "ls --color=auto"

alias ping "ping -c 4"

alias grep "grep --color=auto"
alias fgrep "fgrep --color=auto"
alias egrep "egrep --color=auto"
alias perg "egrep -Ir --exclude=tags --exclude-dir=.git --exclude-dir=build --exclude-dir=Framework"

# git aliases
alias gist "git status -s"
alias c-   "git checkout -"
alias b    "git for-each-ref --sort=-committerdate refs/heads/"
alias giff "git diff --color-words --ignore-space-change"

function git_prompt
    set branch (git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if test -n "$branch"
        set gitStatus (git status --short)
        set numChanged (echo "$gitStatus" | grep '^ M' | wc -l)
        set numUntracked (echo "$gitStatus" | grep '^??' | wc -l)
        set numStashed (git stash list | grep "on $branch" | wc -l)
        test $numChanged   -gt 0; and set changed   " ~$numChanged"
        test $numUntracked -gt 0; and set untracked " +$numUntracked"
        test $numStashed   -gt 0; and set stashed   " {$numStashed}"
        echo -n " [$branch$changed$untracked$stashed]"
    end
end

function fish_prompt
    set --local green '\e[32m'
    set --local blue  '\e[34m'
    set --local white '\e[0m'
    printf "$green%s@%s $blue%s $white\$ " (whoami) (hostname) (git_prompt)
end

function apropos
    apropos $argv 2> /dev/null
end

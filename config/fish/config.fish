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
    set --local branch (git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if test -n $branch
        set --local gitStatus (git status --short)
        set --local numChanged (echo "$gitStatus" | grep '^ M' | wc -l)
        set --local numUntracked (echo "$gitStatus" | grep '^??' | wc -l)
        set --local numStashed (git stash list | grep "on $branch" | wc -l)
        test $numChanged -gt 0; and set --local changed " ~$numChanged"
        test $numUntracked -gt 0; and set --local untracked " +$numUntracked"
        test $numStashed -gt 0; and set --local stashed " {$numStashed}"
        echo -n " [$branch$changed$untracked$stashed]"
    end
end

function fish_prompt
    printf "%s@%s%s\$ " (whoami) (hostname) (git_prompt)
end

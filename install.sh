#!/bin/bash

RED='\e[0;31m'
BLUE='\e[0;34m'
NC='\e[0m'

[[ "$(uname)" = "Darwin" ]] && {
    LINK_FLAGS="-s -f"
    DIR_LINK_FLAGS="${LINK_FLAGS} -F"
} || {
    LINK_FLAGS="--symbolic"
    DIR_LINK_FLAGS="${LINK_FLAGS} --no-dereference"
}

mymkdir() {
    mkdir -p $1 2> /dev/null || true
}

BK_DIR=$(pwd)/.bk/$(date '+%FT%T')
mymkdir $BK_DIR

# arguments:
# 1 flags to the ln command
# 2 the local file in this directory that will be linked
# 3 (optional) the destination. If no argument given, the destination will
#   be the same as the source, but prepended with a dot and put in ~
_create_link() {
    local flags=$1
    local source_rel=$2
    local source_abs=$(pwd)/$2
    local link=$3

    [ $link ] || link=~/.$source_rel
    ln $flags $source_abs $link && echo "$link -> $source_abs"
}
symlink() {
    _create_link "$LINK_FLAGS" $1 $2
}
dirlink() {
    _create_link "$DIR_LINK_FLAGS" $1 $2
}

mymkdir ~/bin
mymkdir ~/.config/powerline-shell
mymkdir ~/.tmuxinator

dirlink home/bash.d ~/.bash.d
dirlink home/vim ~/.vim
dirlink home/vim ~/.config/nvim

symlink home/bashrc ~/.bashrc
symlink home/gitconfig ~/.gitconfig
symlink home/gitignore ~/.gitignore
symlink home/gvimrc ~/.gvimrc
symlink home/hgrc ~/.hgrc
symlink home/inputrc ~/.inputrc
symlink home/pylintrc ~/.pylintrc
symlink home/sqliterc ~/.sqliterc
symlink home/taskrc ~/.taskrc
symlink home/tmux.conf ~/.tmux.conf
symlink home/config/powerline-shell/config.json ~/.config/powerline-shell/config.json

for file in $(ls home/bin); do
    symlink home/bin/$file ~/bin/$file
done

for file in $(ls home/tmuxinator); do
    symlink home/tmuxinator/$file ~/.tmuxinator/$file
done

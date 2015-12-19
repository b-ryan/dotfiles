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

backup() {
    mv $1 $BK_DIR/ 2> /dev/null
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
    backup $link
    ln $flags $source_abs $link && echo "$link -> $source_abs"
}
symlink() {
    _create_link "$LINK_FLAGS" $1 $2
}
dirlink() {
    _create_link "$DIR_LINK_FLAGS" $1 $2
}

mymkdir ~/bin
mymkdir ~/.config/fish
mymkdir ~/.config/pianobar
mymkdir ~/.lein
mymkdir ~/lib
mymkdir ~/.xmonad
mymkdir ~/.xmonad/lib

mkfifo ~/.config/pianobar/ctl 2> /dev/null

dirlink home/vim ~/.vim

symlink home/bash_aliases ~/.bash_aliases
symlink home/bash_completion ~/.bash_completion
symlink home/bash_profile  ~/.bash_profile
symlink home/bash_ps1 ~/.bash_ps1
symlink home/bashrc ~/.bashrc
symlink home/gitconfig ~/.gitconfig
symlink home/gitignore ~/.gitignore
symlink home/gvimrc ~/.gvimrc
symlink home/hgrc ~/.hgrc
symlink home/inputrc ~/.inputrc
symlink home/lein/profiles.clj ~/.lein/profiles.clj
symlink home/sqliterc ~/.sqliterc
symlink home/taskrc ~/.taskrc
symlink home/vimrc ~/.vimrc
symlink home/xmobarrc ~/.xmobarrc
symlink home/xmonad/MyXmobars.hs ~/.xmonad/lib/MyXmobars.hs
symlink home/xmonad/xmonad.hs ~/.xmonad/xmonad.hs

for file in $(ls home/bin); do
    symlink home/bin/$file ~/bin/$file
done

for file in $(ls home/lib); do
    symlink home/lib/$file ~/lib/$file
done

echo Backed up files to $BK_DIR

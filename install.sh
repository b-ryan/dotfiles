#!/bin/bash
LINK_FLAGS="--symbolic"
DIR_LINK_FLAGS="${LINK_FLAGS} --no-dereference"

mymkdir() {
    mkdir -p $1 2> /dev/null || true
}

backup() {
    mv $1 $BK_DIR/
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
    ln $flags $source_abs $link && echo "created link to '$source_abs'"
}
symlink() {
    _create_link "$LINK_FLAGS" $1 $2
}
dirlink() {
    _create_link "$DIR_LINK_FLAGS" $1 $2
}

mymkdir ~/bin
mymkdir ~/.xmonad
mymkdir ~/.config/pianobar
mymkdir ~/.config/fish
mkfifo ~/.config/pianobar/ctl 2> /dev/null

dirlink vim
symlink vimrc
symlink gvimrc
symlink bashrc
symlink bash_aliases
symlink gitconfig
symlink gitignore
symlink hgrc

for file in $(ls bin); do
    symlink bin/$file ~/bin/$file
done

symlink xmobarrc
symlink xmonad/xmonad.hs ~/.xmonad/xmonad.hs

symlink config/pianobar/config ~/.config/pianobar/config
symlink config/fish/config.fish ~/.config/fish/config.fish

echo Backed up files to $BK_DIR

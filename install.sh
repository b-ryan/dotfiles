#!/bin/bash
LINK_FLAGS="--symbolic"
DIR_LINK_FLAGS="${LINK_FLAGS}"

mymkdir() {
    mkdir -p $1 2> /dev/null || true
}

BK_DIR=$(pwd)/.bk/$(date '+%FT%T')
mymkdir $BK_DIR
backup() {
    mv $1 $BK_DIR/
}

# arguments:
# 1 flags to the ln command
# 2 the local file in this directory that will be linked
# 3 (optional) the destination. If no argument given, the destination will
#   be the same as the source, but prepended with a dot and put in ~
create_link() {
    FLAGS=$1
    SOURCE_REL=$2
    SOURCE_ABS=`pwd`/$2
    LINK=$3
    [ $LINK ] || LINK=~/.$SOURCE_REL
    backup $LINK
    ln $FLAGS $SOURCE_ABS $LINK && echo "created link to '$SOURCE_ABS'"
}
symlink() {
    create_link $LINK_FLAGS $1 $2
}
dirlink() {
    create_link $DIR_LINK_FLAGS $1 $2
}

mymkdir ~/bin
mymkdir ~/.xmonad
mymkdir ~/.config/pianobar
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

symlink pianobar/config ~/.config/pianobar/config

echo Backed up files to $BK_DIR

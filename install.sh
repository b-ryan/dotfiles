#!/bin/bash
LINK_FLAGS="-s"
if [ "$1" = "--force" ]
then
    LINK_FLAGS="-sf"
fi
DIR_LINK_FLAGS="${LINK_FLAGS}n"

mkhomedir() {
    mkdir -p ~/$1 2> /dev/null || true
}

# arguments:
# 1 flags to the ln command
# 2 the local file in this directory that will be linked
# 3 (optional) the destination relative to the home directory
#   for example, instead of /home/user/.vim, this arguments would
#   just be .vim. If no argument given, the destination will be
#   the same as the source, but prepended with a dot.
create_link() {
    FLAGS=$1
    SOURCE_REL=$2
    SOURCE_ABS=`pwd`/$2
    LINK=$3
    [ $LINK ] || LINK=.$SOURCE_REL
    ln $FLAGS $SOURCE_ABS ~/$LINK && echo "created link to '$SOURCE_ABS'"
}
symlink() {
    create_link $LINK_FLAGS $1 $2
}
dirlink() {
    create_link $DIR_LINK_FLAGS $1 $2
}

mkhomedir bin
mkhomedir .xmonad

dirlink vim
symlink vimrc
symlink gvimrc
symlink bashrc
symlink bash_aliases
symlink gitconfig
symlink gitignore
symlink git-completion.sh
symlink hgrc

symlink bin/colordiff.pl bin/colordiff.pl
symlink bin/powerline-bash.py bin/powerline-bash.py

symlink xmobarrc
symlink xmonad.hs .xmonad/xmonad.hs

true

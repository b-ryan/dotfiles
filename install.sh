#!/bin/bash
FLAGS="-sf"
if [ "$1" = "--no-force" ]
then
    FLAGS="-s"
fi
DIR_FLAGS="${FLAGS}n"
symlink() {
    ln $FLAGS `pwd`/$1 ~/$2
}
dirlink() {
    ln $DIR_FLAGS `pwd`/$1 ~/$2
}
dirlink vim .vim
symlink vimrc .vimrc
symlink gvimrc .gvimrc
symlink git-completion.sh .git-completion.sh
symlink bashrc .bashrc
symlink gitconfig .gitconfig
mkdir ~/bin 2> /dev/null || true
symlink bin/colordiff.pl bin/colordiff.pl
symlink bin/powerline-bash.py bin/powerline-bash.py

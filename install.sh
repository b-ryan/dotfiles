#!/bin/bash
FLAGS="-s"
if [ "$1" = "--force" ]
then
    FLAGS="-sf"
fi
DIR_FLAGS="${FLAGS}n"

create_link() {
    dest=`pwd`/$2
    ln $1 $dest ~/$3 && echo "created link '$dest'"
}
symlink() {
    create_link $FLAGS $1 $2
}
dirlink() {
    create_link $DIR_FLAGS $1 $2
}
dirlink vim .vim
symlink vimrc .vimrc
symlink gvimrc .gvimrc
symlink git-completion.sh .git-completion.sh
symlink bashrc .bashrc
symlink bash_aliases .bash_aliases
symlink gitconfig .gitconfig
symlink gitignore .gitignore
mkdir ~/bin 2> /dev/null || true
symlink bin/colordiff.pl bin/colordiff.pl
symlink bin/powerline-bash.py bin/powerline-bash.py
true

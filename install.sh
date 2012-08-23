#!/bin/bash
FLAGS="sf"
if [ "$1" = "--no-force" ]
then
    FLAGS="s"
fi
ln -${FLAGS}n `pwd`/vim ~/.vim
ln -${FLAGS} `pwd`/vimrc ~/.vimrc
ln -${FLAGS} `pwd`/gvimrc ~/.gvimrc
ln -${FLAGS} `pwd`/bashrc ~/.bashrc
ln -${FLAGS} `pwd`/gitconfig ~/.gitconfig
mkdir ~/bin 2> /dev/null || true
ln -${FLAGS} `pwd`/bin/colordiff.pl ~/bin/colordiff.pl

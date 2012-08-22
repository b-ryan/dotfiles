install:
	ln -sfn `pwd`/vim ~/.vim
	ln -sf `pwd`/vimrc ~/.vimrc
	ln -sf `pwd`/gvimrc ~/.gvimrc
	ln -sf `pwd`/bashrc ~/.bashrc
	ln -sf `pwd`/gitconfig ~/.gitconfig
	mkdir ~/bin 2> /dev/null || true
	ln -sf `pwd`/bin/colordiff.pl ~/bin/colordiff.pl

.PHONY: install

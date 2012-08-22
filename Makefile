install:
	ln -sf `pwd`/vimrc ~/.vimrc
	ln -sf `pwd`/gvimrc ~/.gvimrc
	ln -sf `pwd`/vim ~/.vim
	ln -sf `pwd`/bashrc ~/.bashrc
	ln -sf `pwd`/gitconfig ~/.gitconfig

.PHONY: install

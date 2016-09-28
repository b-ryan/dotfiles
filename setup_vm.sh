#!/bin/bash
sudo apt-get -y install \
    vim-gnome \
    exuberant-ctags

sudo pear install doc.php.net/pman

# Linters
sudo npm install -g jsonlint
sudo npm install -g jslint
sudo gem install mdl

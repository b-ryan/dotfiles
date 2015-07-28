#!/bin/bash
sudo apt-get install \
    xmonad \
    xmobar \
    dmenu \
    flashplugin-installer \
    tmux \
    python-pip \
    python-virtualenv \
    virtualenvwrapper \
    openjdk-7-jre \
    postgresql-client
sudo apt-get install hal || {
    echo "hal install failed, you may need to first:
        sudo add-apt-repository ppa:mjblenner/ppa-hal
        sudo apt-get update"
    exit 1
}
[[ -f ~/bin/lein ]] || {
    wget --output-document=~/bin/lein \
        https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && \
        chmod +x ~/bin/lein && \
        lein
}
sudo npm install -g jsonlint
sudo npm install -g jslint

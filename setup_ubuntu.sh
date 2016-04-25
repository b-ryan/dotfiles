#!/bin/bash
sudo apt-get -y install \
    dmenu \
    flashplugin-installer \
    openjdk-7-jre \
    postgresql-client \
    python-pip \
    python-virtualenv \
    task \
    tmux \
    virtualenvwrapper \
    xmobar \
    xmonad

sudo apt-get -y install hal || {
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

if [[ ! -d ~/goroot ]]; then
    wget https://storage.googleapis.com/golang/go1.5.2.linux-amd64.tar.gz
    mkdir ~/goroot
    tar -C ~/goroot -xzf go1.5.2.linux-amd64.tar.gz
    rm go1.5.2.linux-amd64.tar.gz
    mkdir ~/gopath
fi

(
    cd /tmp
    wget https://github.com/stedolan/jq/releases/download/jq-1.5/jq-linux64
    mv jq-linux64 ~/bin/jq
    chmod +x ~/bin/jq
)

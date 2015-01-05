#!/bin/bash
sudo apt-get install \
    xmonad \
    xmobar \
    dmenu \
    flashplugin-installer \
    tmux \
    python-pip
sudo apt-get install hal || {
    echo "hal install failed, you may need to first:
        sudo add-apt-repository ppa:mjblenner/ppa-hal
        sudo apt-get update"
    exit 1
}

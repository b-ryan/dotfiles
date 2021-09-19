#!/bin/bash

if [[ -x $HOME/bin/vault ]]; then
    complete -C $HOME/bin/vault vault
fi

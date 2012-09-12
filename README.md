# Buck's dot-files

This repository contains all of my configuration files (most notably those for
Vim). You may notice that none of the files are actually "dot" files in that
they do not start with a '.'. This is to make them non-hidden files.

## Installation

Just run

    ./install.sh

It will create symlinks for all the dot-files in your home directory (but won't
overwrite any existing files/links). If you want it to force the links to be
created, you can run

    ./install.sh --force

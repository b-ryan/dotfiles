# Buck's dot-files

This repository contains all of my configuration files (most notably those for
Vim). You may notice that none of the files are actually "dot" files in that
they do not start with a '.'. This is to make them non-hidden files.

## Installation

Just run

    ./install

It will create symlinks for all the dot-files in your home directory. If you
don't want it to force the links to be created (in case those files already
exist in your home directory), you can instead run

    ./install --no-force

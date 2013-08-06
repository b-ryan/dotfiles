# Buck's dot-files

This repository contains all of my configuration files (most notably those for
Vim). You may notice that none of the files are actually "dot" files in that
they do not start with a '.'. This is to make them non-hidden files.

## Installation

Just run

```bash
./install.sh [install-type] # install-type can be 'default', 'work', or 'necromancer'
```

It will create symlinks for all the dot-files in your home directory. It will
overwrte files wherever it tries to install stuff, but will back everything
up into the .bk directory.

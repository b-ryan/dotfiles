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

## XMonad with Gnome

My XMonad configs are designed to work with GNOME. You only need one
requirement for it to work:

    sudo apt-get install gnome-panel

Then log out and log back in using the XMonad with GNOME session.

## How To

* Launch System Properties from the command line:

  ```
  unity-control-center
  ```

* Turn off built-in display on laptop

  ```
  xrandr --output eDP1 --off
  ```

## Haskell Support

Follow directions in [here](http://www.stephendiehl.com/posts/vim_2016.html)
to install:

* stack
* ghc
* hlint
* ghc-mod
* hasktags

#!/bin/sh

alias _mv="mv -b --suffix .bak -f"

_mv emacs/.custom_emacs ~/
_mv emacs/.emacs ~/
_mv ssh/config ~/.ssh/config
_mv git/.gitconfig ~/.gitconfig
